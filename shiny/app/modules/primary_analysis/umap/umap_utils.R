prepare_umap_data <- function(
    beta,
    targets,
    top_cpgs,
    min_dist,
    n_neighbors,
    metric,
    knn,
    consensus_k_max, 
    id_col,
    seed
) {
  # Prepare inputs
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2 <- align_res$beta2
  targets2 <- align_res$targets2
  
  # top variable CpGs
  top_cpgs <- min(top_cpgs, nrow(beta2))
  mat <- get_top_mad_probes(beta2, top_cpgs)
  mat_t <- t(mat)
  
  # --- SANITY CHECKS --
  n_samples <- nrow(mat_t)
  shiny::validate(
    shiny::need(top_cpgs >= 100,
                paste0("Too few CpGs selected (", top_cpgs, "). Please select at least 100.")),
    shiny::need(n_neighbors < n_samples,
                paste0("N neighbors (", n_neighbors, ") must be smaller than number of samples (", n_samples, ").")),
    shiny::need(knn < n_samples,
                paste0("KNN (", knn, ") must be smaller than number of samples (", n_samples, ").")),
    shiny::need(consensus_k_max < n_samples,
                paste0("Consensus K max (", consensus_k_max, ") must be smaller than number of samples (", n_samples, ")."))
  )
  
  # UMAP config
  n_samples <- nrow(mat_t)
  if (n_neighbors >= n_samples) {
    shiny::validate(
      shiny::need(FALSE, paste0(
        "N neighbors (", n_neighbors, ") must be smaller than the number of samples (", n_samples, "). ",
        "Please reduce N neighbors in the UMAP Parameters."
      ))
    )
  }
  
  cfg <- umap::umap.defaults
  cfg$min_dist <- min_dist
  cfg$n_neighbors <- n_neighbors
  cfg$metric <- metric
  cfg$random_state <- seed
  
  um <- umap::umap(mat_t, config = cfg)
  
  # output df one row one sample
  df <- data.frame(
    Sample = rownames(um$layout),
    UMAP1 = um$layout[, 1],
    UMAP2 = um$layout[, 2],
    stringsAsFactors = FALSE
  )
  
  # attach metadata
  df <- cbind(df, targets2[df$Sample, , drop = FALSE])
  
  # SNN graph + walktrap clustering
  grph <- bluster::makeSNNGraph(as.data.frame(um$layout), k = knn, type = "rank")
  cluster <- igraph::cluster_walktrap(grph)$membership
  
  # Add cluster as first metadata column directly onto df
  cluster_col <- factor(
    paste0("Clust", cluster),
    levels = paste0("Clust", seq_len(consensus_k_max))
  )
  
  # Reorder so consensus cluster is first metadata vlaue
  df <- cbind(
    df[, c("Sample", "UMAP1", "UMAP2"), drop = FALSE],
	snn_walktrap_cluster = cluster_col,
    df[, !colnames(df) %in% c("Sample", "UMAP1", "UMAP2"), drop = FALSE]
  )
  
  # Update targets with the new consensus_cluster column
  targets_updated <- targets2
  targets_updated$snn_walktrap_cluster <- cluster_col[match(rownames(targets2), df$Sample)]
  
  # Return both
  list(
    umap_df = df,
    targets_updated = targets_updated,
    um_model = um
  )
}

plot_umap <- function(
    umap_df,
    color_by,
    legend_position,
    color_palette,
    show_summary = TRUE,
    show_labels = FALSE,
    top_cpgs = NULL,
    min_dist = NULL,
    n_neighbors = NULL,
    knn = NULL,
    consensus_k_max = NULL,
    metric = NULL,
    is_predicted = FALSE
) {
  
  df <- data.frame(
    UMAP1 = umap_df$UMAP1,
    UMAP2 = umap_df$UMAP2,
    Color = umap_df[[color_by]],
    Label = umap_df$Sample
  )
  
  # colour mapping — use selected palette, extend by cycling if too short
  color_vals  <- unique(df$Color)
  matched_colors <- get_matching_colors(color_vals, color_palette)
  
  # Support additional information for walktrap
  color_label <- if (color_by == "consensus_cluster") {
    paste0(color_by, " (knn=", knn, ", k_max=", consensus_k_max, ")")
  } else {
    color_by
  }
  
  # summary string
  if (is_predicted) {
    summary_txt <- paste0(
      "Predicted embedding | New samples projected onto uploaded UMAP model",
      " | color=", color_by
    )
  } else {
    # Support additional information for walktrap
    color_label <- if (color_by == "snn_walktrap_cluster") {
      paste0(color_by, " (knn=", knn, ", k_max=", consensus_k_max, ")")
    } else {
      color_by
    }
    
    summary_txt <- paste0(
      "CpGs=", top_cpgs,
      " | min_dist=", min_dist,
      " | n_neighbors=", n_neighbors,
      " | metric=", metric,
      " | color=", color_label
    )
  }
  
  # base ggplot 
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = UMAP1,
      y = UMAP2,
      color = Color
    )
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_color_manual(values = matched_colors, name = color_by) +
    ggplot2::labs(
      x = "UMAP 1",
      y = "UMAP 2"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.line = ggplot2::element_line(linewidth = 0.5),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = legend_position
    )
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggrepel::geom_label_repel(
      ggplot2::aes(label = Label),
      size = 3, color = "black", fill = "white",
      box.padding = 0.4, max.overlaps = 60
    )
  }
  
  # Add summary as subtitle if requested
  if (show_summary) {
    p <- p + ggplot2::labs(subtitle = summary_txt) +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(size = 11, color = "grey40")
      )
  }
  
  return(p)
}



predict_umap <- function(beta, umap_model) {
  
  umap_features <- colnames(umap_model$data)
  matched_cpgs  <- intersect(umap_features, rownames(beta))
  
  shiny::validate(
    shiny::need(length(matched_cpgs) > 0,
                "No overlapping CpGs between UMAP model and new data.")
  )
  
  # Build aligned matrix (samples x features), fill unmatched CpGs with 0
  aligned_matrix <- matrix(0, nrow = ncol(beta), ncol = length(umap_features))
  colnames(aligned_matrix) <- umap_features
  rownames(aligned_matrix) <- colnames(beta)
  aligned_matrix[, matched_cpgs] <- t(beta[matched_cpgs, ])
  
  # Predict new coordinates
  new_umap_embedding <- umap:::predict.umap(umap_model, aligned_matrix)
  rownames(new_umap_embedding) <- rownames(aligned_matrix)
  
  # Combine training + new
  training_embedding  <- umap_model$layout
  umap_combined       <- rbind(training_embedding, new_umap_embedding)
  
  sample_ids_training <- rownames(training_embedding)
  sample_ids_new      <- rownames(new_umap_embedding)
  all_ids             <- c(sample_ids_training, sample_ids_new)
  
  umap_df <- data.frame(
    Sample        = all_ids,
    UMAP1         = umap_combined[, 1],
    UMAP2         = umap_combined[, 2],
    sample_origin = factor(
      ifelse(all_ids %in% sample_ids_new, "New Sample", "Training"),
      levels = c("Training", "New Sample")
    ),
    stringsAsFactors = FALSE,
    row.names = all_ids
  )
  
  return(umap_df)
}


