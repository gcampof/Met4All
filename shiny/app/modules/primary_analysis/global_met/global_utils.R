plot_global_methylation <- function(
    beta,
    targets,
    id_col,
    comparison_col,
    comparison_type = "between", 
    group1          = NULL,      
    group2          = NULL,   
    annot,
    color_palette,
    out_dir
) {
  # Prepare inputs
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2
  
  message("[DEBUG] First 5 beta rownames: ", paste(head(rownames(beta2)), collapse=", "))
  message("[DEBUG] First 5 annot rownames: ", paste(head(rownames(annot)), collapse=", "))
  
  # Extract and clean groups
  groups <- trimws(as.character(targets2[[comparison_col]]))
  groups[groups == ""] <- NA
  
  #NA filtering
  keep     <- !is.na(groups)
  targets2 <- targets2[keep, , drop = FALSE]
  beta2    <- beta2[, keep, drop = FALSE]
  groups   <- groups[keep]
  
  # Handle comparison type
  if (comparison_type == "custom") {
    
    shiny::validate(
      shiny::need(length(group1) > 0, "Please assign at least one level to Group 1."),
      shiny::need(length(group2) > 0, "Please assign at least one level to Group 2."),
      shiny::need(
        length(intersect(group1, group2)) == 0,
        paste0("Levels cannot be in both groups: ",
               paste(intersect(group1, group2), collapse = ", "))
      )
    )
    
    # Recode groups: levels in group1 -> "Group 1", group2 -> "Group 2", rest dropped
    keep2  <- groups %in% c(group1, group2)
    beta2  <- beta2[, keep2, drop = FALSE]
    groups <- ifelse(groups[keep2] %in% group1, "Group 1", "Group 2")
    
    comparison_label <- paste0(
      "Group 1: [", paste(group1, collapse = ", "), "]  vs  ",
      "Group 2: [", paste(group2, collapse = ", "), "]"
    )
    comparisons_list <- list(c("Group 1", "Group 2"))
    
  } else {
    
    shiny::validate(
      shiny::need(length(unique(groups)) >= 2,
                  paste0("Column '", comparison_col, "' needs at least 2 groups."))
    )
    
    comparison_label <- comparison_col
    present_groups   <- sort(unique(groups))
    comparisons_list <- combn(present_groups, 2, simplify = FALSE)
  }
  
  # Annotation subsets
  beta_all    <- beta2
  beta_cgi    <- beta2[rownames(beta2) %in% rownames(annot[annot$Relation_to_Island == "Island", ]),]
  beta_shore  <- beta2[rownames(beta2) %in% rownames(annot[annot$Relation_to_Island %in% c("N_Shore", "S_Shore"), ]),]
  beta_shelves <- beta2[rownames(beta2) %in% rownames(annot[annot$Relation_to_Island %in% c("S_Shelf", "N_Shelf", "OpenSea"), ]),]
  
  message("[DEBUG] beta_cgi rows: ", nrow(beta_cgi))
  message("[DEBUG] beta_shore rows: ", nrow(beta_shore))
  message("[DEBUG] beta_shelves rows: ", nrow(beta_shelves))
  
  get_means <- function(mat, label) {
    data.frame(
      SampleMean = colMeans(mat, na.rm = TRUE),
      Group      = groups, 
      CpGType    = label,
      stringsAsFactors = FALSE
    )
  }
  
  plot_data <- rbind(
    get_means(beta_all, "All CpGs"),
    get_means(beta_cgi, "CGI"),
    get_means(beta_shore, "Shores"),
    get_means(beta_shelves, "Shelves & OpenSea")
  )
  
  plot_data$CpGType <- factor(plot_data$CpGType,
                              levels = c("All CpGs", "CGI", "Shores", "Shelves & OpenSea"))
  plot_data$Group   <- factor(plot_data$Group)
  
  # Color mapping
  color_vals <- levels(plot_data$Group)
  matched_colors <- get_matching_colors(color_vals, color_palette)
  
  # Create title based on comparison type
  if (comparison_type == "custom") {
    plot_title <- comparison_label
  } else {
    plot_title <- paste0("Global Mean Methylation by ", comparison_label)
  }
  
  # Plot with clean theme_classic styling
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Group, y = SampleMean, fill = Group)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.8, linewidth = 0.5) +
    ggplot2::geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
    ggpubr::stat_compare_means(
      comparisons = comparisons_list,
      method      = "wilcox.test",
      label       = "p.signif",
      size        = 5,  # Increased from 3 to 5 for larger significance stars
      vjust       = 0.5
    ) +
    ggplot2::facet_wrap(~CpGType, nrow = 1) +
    ggplot2::scale_fill_manual(values = matched_colors) +
    ggplot2::coord_cartesian(ylim = c(0.18, 0.85), clip = "off") +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = "Mean Beta Value",
      fill = comparison_col
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      # Title styling - larger and bold
      plot.title = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(b = 10)),
      
      # Text styling
      text = ggplot2::element_text(family = "sans"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", size = 0.5),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 20, l = 10),
      
      # Legend styling
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 9),
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.8, "cm"),
      
      # Facet styling
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      
      # Remove grid
      panel.grid = ggplot2::element_blank()
    )
  
  # Save to disk with explicit dimensions
  tryCatch({
    png_file <- file.path(out_dir, paste0("global_methylation_plot_", Sys.Date(), ".png"))
    ggplot2::ggsave(png_file, p, width = 12, height = 6, dpi = 150, bg = "white")
    
    pdf_file <- file.path(out_dir, paste0("global_methylation_plot_", Sys.Date(), ".pdf"))
    ggplot2::ggsave(pdf_file, p, width = 12, height = 6, bg = "white")
  }, error = function(e) {
    warning("Could not save plots: ", e$message)
  })
  
  return(p)
}