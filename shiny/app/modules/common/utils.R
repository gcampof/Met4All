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

# An analysis id is safe to take from a URL only if it matches exactly this
# shape. The id becomes a path segment, so anything looser risks traversal.
# The trailing 32 hex characters are the full Shiny session token: it is what
# makes the id unguessable, which matters because the id is effectively the
# bearer token for resuming an analysis.
M4A_ANALYSIS_ID_RE <- "^[0-9]{8}_[0-9]{6}_[0-9a-f]{32}$"

valid_analysis_id <- function(id) {
  is.character(id) && length(id) == 1L && !is.na(id) && grepl(M4A_ANALYSIS_ID_RE, id)
}

# Set up analysis directory.
#
# With `resume_id` naming an existing analysis, that directory is reused instead
# of a new one being created, which is what lets a user come back to work they
# left. Falls back to a fresh analysis whenever the id is absent, malformed or
# no longer on disk.
setup_analysis_dir <- function(common_dirs, cfg, session, resume_id = NULL) {
  resumed <- FALSE

  if (valid_analysis_id(resume_id)) {
    candidate <- file.path(common_dirs$data, paste0("analysis_", resume_id))
    if (dir.exists(candidate)) {
      analysis_id <- resume_id
      resumed <- TRUE
      message("Resuming analysis: ", analysis_id)
    }
  }

  if (!resumed) {
    analysis_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", session$token)
  }

  analysis_dir <- file.path(common_dirs$data, paste0("analysis_", analysis_id))

  dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)

  # showWarnings = FALSE hides a permission failure, and everything downstream
  # then breaks somewhere far less obvious (an unwritable data directory used to
  # surface as addResourcePath killing the session at start-up). Fail here
  # instead, and say how to fix it.
  if (!dir.exists(analysis_dir)) {
    stop("Cannot create the analysis directory at '", analysis_dir, "'. ",
         "The data directory is not writable by the app. On the host, run:\n",
         "  chmod 777 ./shiny/logs ./shiny/app/data")
  }

  subdirs <- list()
  for (name in names(cfg$subdirs)) {
    sub_path <- file.path(analysis_dir, cfg$subdirs[[name]])
    dir.create(sub_path, showWarnings = FALSE, recursive = TRUE)
    subdirs[[name]] <- sub_path
  }
  
  message("Analysis directory created: ", analysis_dir)
  
  c(common_dirs,
    list(analysis = analysis_dir, analysis_id = analysis_id, resumed = resumed),
    subdirs)
}


# What the main process keeps in place of the beta matrix itself.
#
# The matrix is ~450 MB for a 70-sample EPIC run and used to be held in a
# per-session reactiveVal, so N connected users cost N x 450 MB before any
# analysis started. Every analysis now reads it from disk inside a worker, so
# the main process only needs to know that it exists and what is in it.
beta_descriptor <- function(path, beta = NULL) {
  if (is.null(beta) && file.exists(path)) {
    beta <- tryCatch(readRDS(path), error = function(e) NULL)
  }
  list(
    path     = path,
    samples  = if (!is.null(beta)) colnames(beta) else character(0),
    n_probes = if (!is.null(beta)) nrow(beta) else NA_integer_
  )
}


# --- Resuming an analysis ----------------------------------------------------
# A small manifest records where the finished artifacts live, so a later visit
# can rehydrate the session from disk instead of starting over. Only paths and
# small values go in it; the heavy objects stay where they already are.

manifest_path <- function(analysis_dir) file.path(analysis_dir, "manifest.rds")

write_analysis_manifest <- function(analysis_dir, type, array_names, mset_paths,
                                    beta_path, targets_path) {
  manifest <- list(
    version      = 1L,
    saved_at     = Sys.time(),
    type         = type,
    array_names  = array_names,
    mset_paths   = mset_paths,
    beta_path    = beta_path,
    targets_path = targets_path
  )
  tryCatch(
    saveRDS(manifest, manifest_path(analysis_dir)),
    error = function(e) warning("Could not write analysis manifest: ", e$message)
  )
  invisible(manifest)
}

# Persist the samplesheet and mark the analysis resumable. Called once an
# analysis has a usable beta matrix; the matrix itself is already on disk.
snapshot_analysis <- function(DIRS, type, array_names, mset_paths, targets) {
  merged_dir <- file.path(DIRS$beta, "merged")
  dir.create(merged_dir, showWarnings = FALSE, recursive = TRUE)

  beta_path    <- file.path(merged_dir, "beta_merged.rds")
  targets_path <- file.path(merged_dir, "targets_merged.rds")

  tryCatch(saveRDS(targets, targets_path),
           error = function(e) warning("Could not save samplesheet: ", e$message))

  write_analysis_manifest(DIRS$analysis, type, array_names, mset_paths,
                          beta_path, targets_path)
}


# Returns NULL unless the manifest exists and the files it points at are still
# there, so a half-finished or partly deleted analysis simply starts fresh.
read_analysis_manifest <- function(analysis_dir) {
  path <- manifest_path(analysis_dir)
  if (!file.exists(path)) return(NULL)

  manifest <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(manifest) || !identical(manifest$version, 1L)) return(NULL)

  needed <- c(manifest$beta_path, manifest$targets_path)
  if (length(needed) == 0 || !all(file.exists(needed))) return(NULL)

  manifest
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
# `dirs` may name several directories: the shared one shipped with the image and,
# when a user uploads one, their own session directory. Uploads are kept per
# session so one user's palette does not appear in everyone else's dropdowns.
load_custom_palettes <- function(dirs) {
  dirs <- dirs[dir.exists(dirs)]
  if (length(dirs) == 0) return(list())
  txt_files <- unlist(lapply(dirs, list.files, pattern = "\\.txt$", full.names = TRUE))
  
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
prepare_color_palettes <- function(dirs) {
  builtin_palettes <- get_built_in_color_palettes()
  custom_palettes <- load_custom_palettes(dirs)
  
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
