# Prepare PCA data from beta matrix
prepare_pca_data <- function(beta, targets, id_col, top_cpgs) {
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2
  
  beta_top <- get_top_mad_probes(beta2, top_cpgs)
  beta_t   <- t(beta_top)
  
  pca <- prcomp(beta_t, scale. = FALSE)
  
  pca_df <- data.frame(
    Sample = rownames(pca$x),
    PC1    = pca$x[, 1],
    PC2    = pca$x[, 2],
    PC3    = pca$x[, 3],
    stringsAsFactors = FALSE
  )
  
  cbind(pca_df, targets2[pca_df$Sample, , drop = FALSE])
}


# Create PCA  plot
plot_pca <- function(pca_df, color_by, dims, color_palette, out_dir) {
  validate(need(!is.null(pca_df), "No data available"))
  validate(need(color_by %in% colnames(pca_df), "Color column not found"))
  
  dim_map <- list(
    "PC1 vs PC2" = c("PC1", "PC2"),
    "PC1 vs PC3" = c("PC1", "PC3"),
    "PC2 vs PC3" = c("PC2", "PC3")
  )
  
  validate(need(dims %in% names(dim_map), "Invalid dimension selection"))
  
  d <- dim_map[[dims]]
  
  color_vals <- unique(pca_df[[color_by]])
  matched_colors <- get_matching_colors(color_vals, color_palette)
  
  p <- ggplot2::ggplot(
    pca_df,
    ggplot2::aes(
      x = .data[[d[1]]],
      y = .data[[d[2]]],
      color = .data[[color_by]]
    )
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_color_manual(values = matched_colors) +
    ggplot2::labs(
      x = d[1],
      y = d[2],
      color = color_by
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = "right"
    )
  
  # Save to disk
  tryCatch({
    png_file <- file.path(out_dir, paste0("pca_plot_", gsub(" ", "_", dims), "_", Sys.Date(), ".png"))
    ggplot2::ggsave(png_file, p, width = 10, height = 6.5, dpi = 150, bg = "white")
    
    pdf_file <- file.path(out_dir, paste0("pca_plot_", gsub(" ", "_", dims), "_", Sys.Date(), ".pdf"))
    ggplot2::ggsave(pdf_file, p, width = 10, height = 6.5, bg = "white")
  }, error = function(e) {
    warning("Could not save plots: ", e$message)
  })
  
  return(p)
}