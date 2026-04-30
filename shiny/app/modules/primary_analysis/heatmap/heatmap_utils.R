row_cor_dist <- function(mat, method = "pearson") {
  cc <- stats::cor(t(mat), use = "pairwise.complete.obs", method = method)
  as.dist(1 - cc)
}


# Expensive: only recomputes when CC inputs change
prepare_heatmap_cc <- function(
    beta,
    targets,
    id_col,
    annotation_cols,
    color_palette,
    top_cpgs,
    cc_kmax,
    cc_reps,
    cc_pItem,
    cc_seed,
    out_dir
) {
  notification_id <- NULL
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2

  shiny::validate(
    shiny::need(top_cpgs >= 10,
                paste0("Too few CpGs selected (", top_cpgs, "). Please select at least 10.")),
    shiny::need(cc_kmax >= 2,
                "CC Max K must be at least 2.")
  )
  
  top_cpgs <- min(top_cpgs, nrow(beta2))
  mat <- get_top_mad_probes(beta2, top_cpgs)
  
  notification_id <- showNotification("Computing consensus clustering...", type="message")
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
  
  # Build top annotation
  top_anno <- NULL
  valid_cols <- intersect(annotation_cols, colnames(targets2))
  anno_df     <- targets2[colnames(mat), valid_cols, drop = FALSE]
  anno_colors <- lapply(valid_cols, function(col) {
    vals     <- unique(as.character(anno_df[[col]]))
    vals     <- vals[!is.na(vals)]
    n_needed <- length(vals)
    
    shiny::validate(
      shiny::need(n_needed > 1,
                  paste0("Column '", col, "' has only one unique value ('", vals, "') — ",
                         "annotation requires at least 2 levels. ",
                         "Please select a different annotation column."))
    )
    
    base_colors <- color_palette(max(n_needed, 2))
    
    if (length(base_colors) < n_needed) {
      base_colors <- rep_len(base_colors, n_needed)
    }
    
    setNames(base_colors, vals)
  })
  names(anno_colors) <- valid_cols
  
  removeNotification(notification_id)
  notification_id <- showNotification("Building annotation...", type="message", duration=0)
  top_anno <- ComplexHeatmap::HeatmapAnnotation(
    df  = anno_df,
    col = anno_colors,
    annotation_height = grid::unit(rep(6, length(valid_cols)), "mm"),
    annotation_legend_param = lapply(valid_cols, function(col) {
      n_levels <- length(unique(na.omit(as.character(anno_df[[col]]))))
      list(
        ncol        = max(1, ceiling(n_levels / 20)),  # wrap into columns if many levels
        legend_gp   = grid::gpar(fontsize = 7)
      )
    }) |> setNames(valid_cols)
  )
  removeNotification(notification_id)
  list(mat = mat, cc = cc, targets2 = targets2, top_anno = top_anno)
}


plot_heatmap <- function(
    cc_data,
    rowK,
    colK,
    show_row_names = FALSE,
    show_col_names = FALSE,
    out_dir
) {
  notification_id <- NULL
  mat      <- cc_data$mat
  cc       <- cc_data$cc
  top_anno <- cc_data$top_anno
  
  shiny::validate(
    shiny::need(rowK >= 1, "Row K must be at least 1."),
    shiny::need(colK <= ncol(mat),
                paste0("Col K (", colK, ") must be smaller than number of samples (", ncol(mat), ").")),
    shiny::need(colK <= length(cc),
                paste0("Col K (", colK, ") cannot exceed CC Max K (", length(cc) - 1, ")."))
  )
  
  row_tree  <- hclust(row_cor_dist(mat), method = "ward.D2")
  col_class <- cc[[colK]]$consensusClass[colnames(mat)]
  col_split <- factor(col_class)
  
  # Store cc output .tsv
  write.table(
    data.frame(sample = names(col_class), CCP_cluster = col_class),
    file = file.path(out_dir, paste0("ConsensusClass_k", colK, ".tsv")),
    sep = "\t", quote = FALSE, row.names = FALSE
  )
  
  chtm <- ComplexHeatmap::Heatmap(
    mat,
    name = "Methylation",
    col  = viridis::viridis(100),
    cluster_rows      = row_tree,
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