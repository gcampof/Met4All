# Set up common directories (shared across all analyses)
setup_common_dirs <- function(cfg) {
  common_dirs <- list(
    cache                = here::here(cfg$cache),
    data                 = here::here(cfg$data),
    filter               = here::here(cfg$filter),
    custom_color_palette = here::here(cfg$custom_color_palette),
    pathways             = here::here(cfg$pathways)
  )
  
  for (d in common_dirs) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  
  message("Common directories ready")
  return(common_dirs)
}

# Set up analysis directory (creates new one with auto-generated ID from session)
setup_analysis_dir <- function(common_dirs, cfg, session) {
  analysis_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    substr(session$token, 1, 8) 
  )
  
  analysis_dir <- file.path(common_dirs$data, paste0("analysis_", analysis_id))
  
  dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)
  
  subdirs <- list()
  for (name in names(cfg$subdirs)) {
    sub_path <- file.path(analysis_dir, cfg$subdirs[[name]])
    dir.create(sub_path, showWarnings = FALSE, recursive = TRUE)
    subdirs[[name]] <- sub_path
  }
  
  message("Analysis directory created: ", analysis_dir)
  
  c(common_dirs, list(analysis = analysis_dir, analysis_id = analysis_id), subdirs)
}


cleanup_old_analysis_dirs <- function(base_dir, max_age_hours = 24) {
  if (!dir.exists(base_dir)) return()
  
  dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  now <- Sys.time()
  
  for (d in dirs) {
    info <- file.info(d)
    if (is.na(info$mtime)) next
    
    age <- difftime(now, info$mtime, units = "hours")
    
    if (age > max_age_hours) {
      unlink(d, recursive = TRUE, force = TRUE)
      message("[CLEANUP] Removed old analysis dir: ", d)
    }
  }
}


# GO: Biological Process gene sets (SYMBOL)
get_go_bp_gene_sets <- function() {
  go_terms <- AnnotationDbi::select(
    org.Hs.eg.db,
    keys    = keys(org.Hs.eg.db, keytype = "GO"),
    columns = c("GO", "SYMBOL", "ONTOLOGY"),
    keytype = "GO"
  )
  go_bp_terms <- go_terms[go_terms$ONTOLOGY == "BP", ]
  go_bp_gene_sets <- split(go_bp_terms$SYMBOL, go_bp_terms$GO)
  go_bp_gene_sets <- lapply(go_bp_gene_sets, unique)
  go_bp_gene_sets[!sapply(go_bp_gene_sets, is.null)]
}


# Process-level memo, shared by every Shiny session in this R process.
# Lives in globalenv() so it survives Shiny re-sourcing app.R on mtime change
# (routine under the dev bind mount); a file-scope env would be discarded.
if (!exists(".M4A_CACHE_MEMO", envir = globalenv(), inherits = FALSE)) {
  assign(".M4A_CACHE_MEMO", new.env(parent = emptyenv()), envir = globalenv())
}

# The only columns any consumer reads (global_met/global_utils.R). Keep at least
# two: annot[cond, ] on a single-column data.frame drops to a vector and
# rownames() becomes NULL, which empties the CGI/Shore/Shelf facets silently.
.M4A_RAW_ANNOT_COLS <- c("chr", "Relation_to_Island")

# Set up annotation cache.
# Artifacts are precomputed at image build time (see shiny/build_cache.R) and
# read from DIRS$cache; anything missing is built and persisted here.
# The returned objects are shared by reference across sessions — treat them as
# read-only (in particular, never apply data.table `:=` to them).
setup_cache <- function(DIRS, cfg) {
  memo <- get(".M4A_CACHE_MEMO", envir = globalenv())
  key  <- paste0(cfg$annotation_pkg, "@", DIRS$cache)

  hit <- get0(key, envir = memo, inherits = FALSE)
  if (!is.null(hit)) {
    message("[cache] Reusing process-level cache for ", cfg$annotation_pkg)
    return(hit)
  }

  # Annotation-specific artifacts are keyed by package, so switching between
  # EPIC and 450k is a cache miss instead of a silent stale read.
  p <- function(stem) {
    file.path(DIRS$cache, paste0(stem, "__", cfg$annotation_pkg, ".rds"))
  }

  read_or_build <- function(path, build_fn, label) {
    if (file.exists(path)) {
      message("[cache] Loading ", label, " from ", path)
      return(readRDS(path))
    }
    message("[cache] Building ", label, " (not cached at ", path, ")")
    obj <- build_fn()
    tryCatch(
      saveRDS(obj, path),
      error = function(e) warning("[cache] Could not persist ", path, ": ", e$message)
    )
    obj
  }

  built_annot <- read_or_build(
    p("built_annot"),
    function() methylation_buildannot(cfg$annotation_pkg),
    "built annotation"
  )

  raw_annot <- read_or_build(
    p("raw_annot_slim"),
    function() {
      full <- as.data.frame(minfi::getAnnotation(cfg$annotation_pkg))
      missing_cols <- setdiff(.M4A_RAW_ANNOT_COLS, colnames(full))
      if (length(missing_cols) > 0) {
        stop("Annotation '", cfg$annotation_pkg, "' lacks column(s): ",
             paste(missing_cols, collapse = ", "))
      }
      full[, .M4A_RAW_ANNOT_COLS, drop = FALSE]
    },
    "raw annotation (slim)"
  )

  # gene_set_list is only an intermediate for pathways, so it is built lazily
  # inside it and never needed once pathways.rds exists.
  pathways <- read_or_build(
    file.path(DIRS$cache, "pathways.rds"),
    function() list(
      go_bp     = read_or_build(file.path(DIRS$cache, "gene_set_list.rds"),
                                get_go_bp_gene_sets, "GO-BP gene sets"),
      kegg      = fgsea::gmtPathways(file.path(DIRS$pathways, cfg$gene_set$kegg)),
      hallmarks = fgsea::gmtPathways(file.path(DIRS$pathways, cfg$gene_set$hallmark))
    ),
    "pathways"
  )

  out <- list(
    built_annot = built_annot,
    raw_annot   = raw_annot,
    pathways    = pathways
  )

  assign(key, out, envir = memo)
  message("[cache] Setup complete for ", cfg$annotation_pkg)
  out
}


# Supported built-in color palettes
get_built_in_color_palettes <- function(){
  return(
    builtin_palettes <- list(
      "Set1 (Brewer)"      = function(n) RColorBrewer::brewer.pal(n, "Set1"),
      "Dark2 (Brewer)"     = function(n) RColorBrewer::brewer.pal(n, "Dark2"),
      "Paired (Brewer)"    = function(n) RColorBrewer::brewer.pal(n, "Paired"),
      "Set2 (Brewer)"      = function(n) RColorBrewer::brewer.pal(n, "Set2"),
      "viridis"            = function(n) viridis::viridis(n),
      "magma"              = function(n) viridis::magma(n),
      "plasma"             = function(n) viridis::plasma(n),
      "cividis"            = function(n) viridis::cividis(n)
    ))
}


# Load custom palettes from directory
load_custom_palettes <- function(dir) {
  txt_files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) return(list())
  
  palettes <- lapply(txt_files, function(f) {
    colors <- readLines(f, warn = FALSE)
    colors <- trimws(colors[nzchar(colors)])
    palette_name <- tools::file_path_sans_ext(basename(f))
    list(name = palette_name, colors = colors)
  })
  
  # Return as named list of functions 
  named <- setNames(
    lapply(palettes, function(p) {
      force(p)
      function(n) p$colors[seq_len(min(n, length(p$colors)))]
    }),
    paste0(sapply(palettes, `[[`, "name"))
  )
  
  named
}


# Prepare all color palettes
prepare_color_palettes <- function(dir) {
  builtin_palettes <- get_built_in_color_palettes()
  custom_palettes <- load_custom_palettes(dir)
  
  all_palettes <- c(builtin_palettes, custom_palettes)
  
  all_palette_choices <- Filter(
    function(x) length(x) > 0,
    list(
      "Custom"   = names(custom_palettes),
      "Built-in" = names(builtin_palettes)
    )
  )
  
  list(
    all_palettes        = all_palettes,
    all_palette_choices = all_palette_choices
  )
}


# Load and validate a new palette
load_new_palette <- function(file_path, palette_name, palette_dir) {
  tryCatch({
    # Read file safely
    if (!file.exists(file_path)) {
      return(list(success = FALSE, message = "File not found"))
    }
    
    # Read lines
    colors <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    
    # Clean: trim whitespace and remove empty lines
    colors <- trimws(colors)
    colors <- colors[nzchar(colors)]
    
    # Validate: each line must be a valid hex color
    valid_hex <- function(x) {
      grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})$", x)
    }
    
    invalid_colors <- colors[!valid_hex(colors)]
    
    if (length(invalid_colors) > 0) {
      msg <- paste(
        "Invalid hex colors found:",
        paste(invalid_colors[1:min(3, length(invalid_colors))], collapse = ", ")
      )
      return(list(success = FALSE, message = msg))
    }
    
    if (length(colors) == 0) {
      return(list(success = FALSE, message = "No valid hex colors found in file"))
    }
    
    # Save to palette directory
    dir.create(palette_dir, showWarnings = FALSE, recursive = TRUE)
    output_file <- file.path(palette_dir, paste0(palette_name, ".txt"))
    writeLines(colors, output_file, useBytes = TRUE)
    
    return(list(
      success = TRUE,
      message = paste0("Loaded ", length(colors), " colors from '", palette_name, "'"),
      file = output_file
    ))
    
  }, error = function(e) {
    list(success = FALSE, message = paste("Error:", e$message))
  })
}

start_logging <- function(out_dir) {
  tryCatch({
    log_file_path <- file.path(out_dir, "logs.txt")
    log_conn <- file(log_file_path, open = "wt")
    if (sink.number(type = "output") > 0) sink(type = "output")
    if (sink.number(type = "message") > 0) sink(type = "message")
    sink(log_conn, type = "output", split = TRUE)
    sink(log_conn, type = "message")
    assign(".log_conn", log_conn, envir = .GlobalEnv)
    options(crayon.enabled = FALSE)
    options(cli.num_colors = 1)
    cat("\n=== LOG STARTED", format(Sys.time()), "===\n\n")
  }, error = function(e) {
    warning("[Logging] Failed to start logging: ", e$message)
  })
}


stop_logging <- function() {
  cat("\n=== LOG STOPPED", format(Sys.time()), "===\n")
  
  # Close sinks in reverse order
  if (sink.number(type = "message") > 0) sink(type = "message")
  if (sink.number(type = "output") > 0) sink(type = "output")
  
  # Close and remove the connection
  if (exists(".log_conn")) {
    close(.log_conn)
    rm(.log_conn, envir = .GlobalEnv)
  }
}


load_heavy_components <- function(session, DIRS, cfg, APP_CACHE) {
  showModal(modalDialog(
    title = div(
      class = "text-center",
      icon("microchip", class = "fa-2x mb-2", style = "color: #2c7fb8;"),
      h4("Loading Analysis Components", class = "mt-2")
    ),
    div(
      class = "text-center",
      div(class = "spinner-border text-primary mb-3",
          role = "status",
          style = "width: 3rem; height: 3rem;"),
      p("This may take a moment...", class = "text-muted"),
      p("Loading packages and cache...", class = "small text-muted")
    ),
    footer = NULL,
    easyClose = FALSE,
    size = "s"
  ))
  
  session$onFlushed(function() {
    tryCatch({
      source("modules/common/all_imports.R", local = TRUE)
      cache <- setup_cache(DIRS, cfg)
      APP_CACHE(cache)
      removeModal()
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Failed to load components:", e$message),
        footer = modalButton("OK")
      ))
    })
  }, once = TRUE)
  
  invisible(NULL)  # explicit: this function intentionally returns nothing
}