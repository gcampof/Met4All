align_targets_to_beta_cols <- function(beta, targets, id_col) {
  # Check id_col actually exists in targets
  shiny::validate(
    shiny::need(id_col %in% colnames(targets), paste0(
      "Column '", id_col, "' not found in sample sheet. ",
      "Available columns: ", paste(colnames(targets), collapse = ", ")
    ))
  )
  
  # Clean beta col names
  colnames(beta) <- trimws(colnames(beta))
  colnames(beta) <- gsub("^X", "", colnames(beta))
  colnames(beta) <- gsub("[.]", "-", colnames(beta))
  
  # Clean target IDs
  targets[[id_col]] <- trimws(as.character(targets[[id_col]]))
  
  # Match
  common_ids <- intersect(colnames(beta), targets[[id_col]])
  if (length(common_ids) < 2) {
    shiny::validate(
      shiny::need(FALSE, paste0(
        "Could not match SampleSheet to BetaMatrix using column '", id_col, "' — ",
        "only ", length(common_ids), " common ID(s) found. ",
        "Please try a different Sample ID column."
      ))
    )
  }
  
  beta2 <- beta[, common_ids, drop = FALSE]
  targets2 <- targets[match(common_ids, targets[[id_col]]), , drop = FALSE]
  rownames(targets2) <- targets2[[id_col]]
  rownames(beta2) <- rownames(beta)
  
  list(beta2 = beta2, targets2 = targets2)
}


# Get top cpg
get_top_mad_probes <- function(beta, n) {
  beta <- as.matrix(beta)
  storage.mode(beta) <- "numeric"
  
  idx <- order(matrixStats::rowMads(beta), decreasing = TRUE)[seq_len(n)]
  beta[idx, , drop = FALSE]
}


# Get matching colors from color palette
get_matching_colors <- function(color_vals, color_palette){
  n_needed    <- length(color_vals)
  base_colors <- color_palette(n_needed)
  if (length(base_colors) < n_needed) 
    base_colors <- rep_len(base_colors, n_needed)
  # Palettes can return more colours than asked for (brewer.pal has a minimum of
  # 3), which would leave NA-named entries.
  matched_colors <- setNames(base_colors[seq_len(n_needed)], color_vals)
  
  return(matched_colors)
}


update_all_palettes <- function(session, PALETTES) {
  all_palette_choices <- PALETTES$all_palette_choices
  palette_inputs <- c(
    "mds_color_palette",
    "pca_color_palette",
    "umap_color_palette",
    "heatmap_color_palette",
    "global_met_color_palette",
    "diff_met_color_palette",
    "cnv_color_palette"
  )
  
  for (input_id in palette_inputs) {
    updateSelectInput(session, input_id, choices = all_palette_choices)
  }
}

# Shared DT config
make_dt <- function(df, editable = FALSE, container_width = "100%") {
  
  edit_config <- if (isTRUE(editable)) {
    list(target = "cell", disable = list(columns = 0L))
  } else {
    FALSE
  }
  
  DT::datatable(
    df,
    editable   = edit_config,
    filter     = "top", 
    rownames   = FALSE,
    options    = list(
      scrollY        = "450px",
      scrollX        = TRUE,
      scrollCollapse = TRUE,
      deferRender    = TRUE,
      pageLength     = 500,
      lengthMenu     = c(50, 100, 250, 500),
      autoWidth      = FALSE,
      columnDefs     = list(
        list(targets = "_all", className = "dt-right"),
        list(targets = "_all", width = "150px")
      ),
      searching  = FALSE,
      pagingType = "simple_numbers"
    ),
    class      = "display compact hover stripe",
    style      = "bootstrap4",
    extensions = c("Buttons", "KeyTable", "Scroller"),
    callback   = JS("
      table.style.width = '100%';
      table.style.tableLayout = 'fixed';
    ")
  ) |>
    DT::formatStyle(
      columns     = names(df),
      `white-space` = "normal",
      `word-break`  = "break-word",
      `max-width`   = "150px"
    )
}

# Read an uploaded samplesheet (.csv/.xlsx) as a plain data.frame
read_samplesheet_file <- function(path, name) {
  df <- if (grepl("\\.xlsx?$", name, ignore.case = TRUE)) {
    readxl::read_excel(path)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  df
}


# Shared columns whose values overlap, best match first — usable as merge keys
samplesheet_key_candidates <- function(targets, new_ss) {
  shared <- intersect(colnames(targets), colnames(new_ss))
  if (length(shared) == 0) return(character(0))

  n_match <- vapply(shared, function(col) {
    length(intersect(trimws(as.character(targets[[col]])),
                     trimws(as.character(new_ss[[col]]))))
  }, integer(1))

  shared <- shared[n_match > 0]
  shared[order(n_match[n_match > 0], decreasing = TRUE)]
}


# Row indices of new_ss aligned to targets, NA where a sample has no match
match_samplesheet_rows <- function(targets, new_ss, key_col) {
  match(trimws(as.character(targets[[key_col]])),
        trimws(as.character(new_ss[[key_col]])))
}


# Add columns from an uploaded samplesheet, matching rows by key_col.
# Row order and rownames of targets are preserved so existing analyses keep working.
merge_samplesheet_columns <- function(targets, new_ss, key_col, overwrite = FALSE) {
  if (!key_col %in% colnames(targets) || !key_col %in% colnames(new_ss)) {
    stop("Match column '", key_col, "' must exist in both samplesheets.")
  }

  idx <- match_samplesheet_rows(targets, new_ss, key_col)
  matched <- !is.na(idx)
  if (!any(matched)) stop("No samples matched using column '", key_col, "'.")

  added <- character(0); updated <- character(0); skipped <- character(0)

  for (col in setdiff(colnames(new_ss), key_col)) {
    vals <- new_ss[[col]][idx]

    if (!col %in% colnames(targets)) {
      targets[[col]] <- vals
      added <- c(added, col)
    } else if (overwrite) {
      # Coerce to character when types differ, and leave unmatched rows untouched
      if (!identical(class(targets[[col]]), class(vals))) {
        targets[[col]] <- as.character(targets[[col]])
        vals <- as.character(vals)
      }
      targets[[col]][matched] <- vals[matched]
      updated <- c(updated, col)
    } else {
      skipped <- c(skipped, col)
    }
  }

  list(targets = targets, n_matched = sum(matched), n_total = nrow(targets),
       added = added, updated = updated, skipped = skipped)
}
