# Prepare MDS data from beta matrix
prepare_mds_data <- function(beta, targets, id_col, top_cpgs) {
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2
  
  beta_top <- get_top_mad_probes(beta2, top_cpgs)
  beta_t   <- t(beta_top)
  
  sds    <- matrixStats::rowSds(beta_t)
  beta_t <- beta_t[sds > 0, , drop = FALSE]
  
  dist_mat <- dist(beta_t, method = "euclidean")
  coords   <- cmdscale(dist_mat, k = 2)
  
  mds_df <- data.frame(
    Sample = rownames(coords),
    Dim1   = coords[, 1],
    Dim2   = coords[, 2],
    stringsAsFactors = FALSE
  )
  
  # targets2 rows are already aligned to beta cols, just cbind
  cbind(mds_df, targets2[mds_df$Sample, , drop = FALSE])
}


plot_mds <- function(mds_df, color_by, color_palette) {
  validate(need(!is.null(mds_df), "No data available"))
  validate(need(color_by %in% colnames(mds_df), "Color column not found"))
  
  color_vals <- unique(mds_df[[color_by]])
  matched_colors <- get_matching_colors(color_vals, color_palette)
  
  p <- ggplot2::ggplot(mds_df, ggplot2::aes(x = Dim1, y = Dim2, color = .data[[color_by]])) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_color_manual(values = matched_colors) +
    ggplot2::labs(x = "MDS Dimension 1", y = "MDS Dimension 2", color = color_by) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = "right"
    )
  
  return(p) 
}