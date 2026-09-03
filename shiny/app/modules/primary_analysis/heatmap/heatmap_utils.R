row_cor_dist <- function(mat, method = "pearson") {
  cc <- stats::cor(t(mat), use = "pairwise.complete.obs", method = method)
  as.dist(1 - cc)
}


# Expensive half of the heatmap: consensus clustering AND the row dendrogram.
#
# Runs inside a worker, so it takes a path plus small values and must stay free of
# Shiny session calls. It returns plain data only (no ComplexHeatmap S4 objects):
# the annotation and Heatmap objects are cheap to build and are assembled by the
# caller instead.
#
# row_tree is computed here on purpose. It depends only on the probe matrix, not
# on rowK/colK, but it used to live in plot_heatmap and was therefore rebuilt on
# every slider nudge — a 10000x10000 correlation matrix (~800 MB) plus an O(n^2)
# hclust each time.
prepare_heatmap_cc <- function(
    beta_path,
    targets,
    id_col,
    annotation_cols,
    palette_dir,
    palette_name,
    top_cpgs,
    cc_kmax,
    cc_reps,
    cc_pItem,
    cc_seed
) {
  if (!isTRUE(top_cpgs >= 10 && top_cpgs <= 10000)) {
    stop("Top CpGs must be a number between 10 and 10000.")
  }
  if (!isTRUE(cc_kmax >= 2)) stop("CC Max K must be at least 2.")

  beta <- readRDS(beta_path)

  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2

  top_cpgs <- min(top_cpgs, nrow(beta2))
  mat <- get_top_mad_probes(beta2, top_cpgs)

  message("[heatmap] Computing consensus clustering")
  cc <- ConsensusClusterPlus::ConsensusClusterPlus(
    as.matrix(mat),
    maxK         = cc_kmax,
    reps         = cc_reps,
    pItem        = cc_pItem,
    pFeature     = 1,
    clusterAlg   = "hc",
    distance     = "pearson",
    innerLinkage = "ward.D2",
    finalLinkage = "ward.D2",
    seed         = cc_seed
  )

  message("[heatmap] Clustering rows")
  row_tree <- hclust(row_cor_dist(mat), method = "ward.D2")

  # Annotation colours, as plain named vectors.
  palettes      <- prepare_color_palettes(palette_dir)
  color_palette <- palettes$all_palettes[[palette_name]]
  if (is.null(color_palette)) color_palette <- palettes$all_palettes[[1]]

  valid_cols <- intersect(annotation_cols, colnames(targets2))
  anno_df    <- targets2[colnames(mat), valid_cols, drop = FALSE]

  anno_colors <- lapply(valid_cols, function(col) {
    vals     <- unique(as.character(anno_df[[col]]))
    vals     <- vals[!is.na(vals)]
    n_needed <- length(vals)

    if (n_needed <= 1) {
      stop("Column '", col, "' has only one unique value ('", paste(vals, collapse = ", "),
           "') - annotation requires at least 2 levels. Please select a different ",
           "annotation column.")
    }

    # brewer.pal() never returns fewer than 3 colours, so asking for 2 yields 3 and
    # setNames() would leave an NA-named entry that ComplexHeatmap rejects outright.
    base_colors <- color_palette(max(n_needed, 2))
    if (length(base_colors) < n_needed) base_colors <- rep_len(base_colors, n_needed)
    setNames(base_colors[seq_len(n_needed)], vals)
  })
  names(anno_colors) <- valid_cols

  list(mat = mat, cc = cc, targets2 = targets2,
       anno_df = anno_df, anno_colors = anno_colors, row_tree = row_tree)
}


# Cheap half: assemble the ComplexHeatmap objects from what the worker returned.
# Runs in the main process on every appearance change, so it must stay cheap —
# no clustering here.
plot_heatmap <- function(
    cc_data,
    rowK,
    colK,
    show_row_names = FALSE,
    show_col_names = FALSE
) {
  mat <- cc_data$mat
  cc  <- cc_data$cc

  if (!isTRUE(rowK >= 1)) stop("Row K must be at least 1.")
  if (colK > ncol(mat)) {
    stop("Col K (", colK, ") must be smaller than number of samples (", ncol(mat), ").")
  }
  if (colK > length(cc)) {
    stop("Col K (", colK, ") cannot exceed CC Max K (", length(cc) - 1, ").")
  }

  valid_cols <- names(cc_data$anno_colors)
  anno_df    <- cc_data$anno_df

  top_anno <- ComplexHeatmap::HeatmapAnnotation(
    df  = anno_df,
    col = cc_data$anno_colors,
    annotation_height = grid::unit(rep(6, length(valid_cols)), "mm"),
    annotation_legend_param = lapply(valid_cols, function(col) {
      n_levels <- length(unique(na.omit(as.character(anno_df[[col]]))))
      list(
        ncol      = max(1, ceiling(n_levels / 20)),  # wrap into columns if many levels
        legend_gp = grid::gpar(fontsize = 7)
      )
    }) |> setNames(valid_cols)
  )

  col_class <- cc[[colK]]$consensusClass[colnames(mat)]
  col_split <- factor(col_class)

  # NOTE: the ConsensusClass TSV is written by the caller, not here, so that it
  # always lands in the current session's results directory.

  chtm <- ComplexHeatmap::Heatmap(
    mat,
    name = "Methylation",
    col  = viridis::viridis(100),
    cluster_rows      = cc_data$row_tree,
    row_split         = rowK,
    cluster_columns   = TRUE,
    column_split      = col_split,
    top_annotation    = top_anno,
    show_row_names    = show_row_names,
    show_column_names = show_col_names,
    row_names_gp      = grid::gpar(fontsize = 6),
    column_names_gp   = grid::gpar(fontsize = 8)
  )

  list(ht = chtm, col_class = col_class)
}
