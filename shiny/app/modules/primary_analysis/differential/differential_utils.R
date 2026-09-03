# Run FGSEA for a specific pathway
run_fgsea <- function(stats, pws) {
  # Fixed values
  gsea_minSize = 30
  gsea_maxSize = 600
  seed = 123456
  
  # Scope the seed to this call. A bare set.seed() reseeds the RNG of the shared
  # R process, silently perturbing other users' UMAP and consensus clustering.
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  set.seed(seed)
  res <- fgsea::fgsea(
    pathways = pws,
    stats    = stats,
    minSize  = gsea_minSize,
    maxSize  = gsea_maxSize,
    eps      = 0
  )
  res <- res[order(res$pval), ]
  as.data.frame(res)
}

# Clean up fgsea output
tidy_fgsea <- function(df) {
  tibble::as_tibble(df) %>%
    dplyr::arrange(desc(NES)) %>%
    dplyr::mutate(leadingEdge = vapply(leadingEdge, function(x) paste(x, collapse = ";"),
                                       FUN.VALUE = character(1)))
}


prepare_differential_methylation_data <- function(
    beta, 
    targets,
    built_annot,
    id_col,
    comparison_col,
    baseline = NULL,      
    comparison = NULL
){
  shiny::validate(
    shiny::need(length(baseline) > 0, "Please assign at least one level to Baseline"),
    shiny::need(length(comparison) > 0, "Please assign at least one level to Comparison"),
    shiny::need(
      length(intersect(baseline, comparison)) == 0,
      paste0("Levels cannot be in both groups: ",
             paste(intersect(baseline, comparison), collapse = ", "))
    )
  )
  
  notification_id <- showNotification("Preparing differential methylation data...", 
                                      type = "message", duration = NULL, id = "diff_prep")
  
  
  # Prepare inputs
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2
  
  # Keep only probes in annotation
  keep_probes <- rownames(beta2) %in% built_annot$Name
  beta2 <- beta2[keep_probes, ]
  
  # Get tss200 beta matrix annotatted
  tss200 <- methylation_genemat_dt(beta2, built_annot)
  # tss200 <- methylation_genemat(beta2, built_annot)
  # NA filter for row names
  tss200 <- tss200[!is.na(rownames(tss200)), ]
  
  # Extract and clean groups
  groups <- trimws(as.character(targets2[[comparison_col]]))
  groups[groups == ""] <- NA
  
  # NA filter 
  keep     <- !is.na(groups)
  targets2 <- targets2[keep, , drop = FALSE]
  beta2    <- beta2[, keep, drop = FALSE]
  groups   <- groups[keep]

  # Recode groups in levels
  keep2  <- groups %in% c(baseline, comparison)
  beta2  <- beta2[, keep2, drop = FALSE]
  tss200_2    <- tss200[, keep2, drop = FALSE]
  groups_subset <- groups[keep2]
  groups_recoded <- ifelse(groups_subset %in% baseline, "Baseline", "Comparison")
  groups_factor <- factor(groups_recoded, levels = c("Baseline", "Comparison"))
  
  # Validate we have enough samples
  shiny::validate(
    shiny::need(sum(groups_factor == "Baseline") >= 2, 
                "Need at least 2 samples in Baseline group"),
    shiny::need(sum(groups_factor == "Comparison") >= 2, 
                "Need at least 2 samples in Comparison group")
  )
  
  # Run limma on genes for later use (FGSEA)
  desing <- model.matrix(~ groups_factor)
  fit_tss  <- limma::lmFit(tss200_2, desing)
  fit2_tss <- limma::eBayes(fit_tss)
  toptab_gene_all <- limma::topTable(fit2_tss, adjust = "fdr", number = Inf, sort.by = "p")
  
  # Build comparison label for plot titles
  comparison_label <- paste0(
    "Baseline: [", paste(baseline, collapse = ", "), "]  vs  ",
    "Comparison: [", paste(comparison, collapse = ", "), "]"
  )
  removeNotification(id = "diff_prep")
  
  list(
    beta_diff = beta2,
    groups_factor = groups_factor,
    limma_desing = desing,
    toptab_gene_all = toptab_gene_all,
    comparison_label = comparison_label
  )
}


plot_diff_methylation_density <- function(diff_met_data, color_palette, out_dir) {
  notification_id <- showNotification("Generating density plot...", 
                                      type = "message", duration = NULL, id = "density_plot")
  
  beta_diff <- diff_met_data$beta_diff
  groups <- diff_met_data$groups_factor
  comparison_label <- diff_met_data$comparison_label
  
  # Calculate group means
  group_levels <- levels(groups)
  group_means <- sapply(group_levels, function(g) {
    cols <- which(groups == g)
    rowMeans(beta_diff[, cols, drop = FALSE], na.rm = TRUE)
  })
  group_means_mat <- matrix(group_means,
                            ncol = length(group_levels),
                            dimnames = list(rownames(beta_diff), group_levels))
  
  # Get unique groups and map colors
  color_vals <- group_levels
  matched_colors <- get_matching_colors(color_vals, color_palette)
  
  minfi::densityPlot(
    as.matrix(group_means_mat),
    sampGroups = group_levels,
    main = paste("Mean density plot - ", comparison_label),
    xlab = "Beta",
    pal = matched_colors
  )
  
  # Save plot to disk
  tryCatch({
    # Save as PNG
    png_file <- file.path(out_dir, paste0("density_plot_", Sys.Date(), ".png"))
    png(png_file, width = 1000, height = 800, res = 150)
    minfi::densityPlot(
      as.matrix(group_means_mat),
      sampGroups = group_levels,
      main = paste("Mean density plot - ", comparison_label),
      xlab = "Beta",
      pal = matched_colors
    )
    dev.off()
    
    # Save as PDF
    pdf_file <- file.path(out_dir, paste0("density_plot_", Sys.Date(), ".pdf"))
    pdf(pdf_file, width = 10, height = 8)
    minfi::densityPlot(
      as.matrix(group_means_mat),
      sampGroups = group_levels,
      main = paste("Mean density plot - ", comparison_label),
      xlab = "Beta",
      pal = matched_colors
    )
    dev.off()
    
  }, error = function(e) {
    warning("Could not save density plot: ", e$message)
  })
  
  removeNotification(id = "density_plot")
}


get_dmps <- function(diff_met_data, 
                     fdr_cut, 
                     lfc_cut, 
                     with_champ,
                     out_dir
){
  notification_id <- showNotification("Computing DMPs...", 
                                      type = "message", duration = NULL, id = "dmps_calc")
  
  beta_diff <- diff_met_data$beta_diff
  groups <- diff_met_data$groups_factor
  
  if(!with_champ) {
    # run with limma
    removeNotification(id = "dmps_calc")
    notification_id <- showNotification("Running limma, please wait ...", 
                                        type = "message", duration = NULL, id = "dmps_calc")
    desing <- diff_met_data$limma_desing
    fit  <- limma::lmFit(beta_diff, desing)
    fit2 <- limma::eBayes(fit)
    
    dmps_prefilter <- limma::topTable(
      fit2,
      coef = 2,
      adjust.method = "fdr",
      sort.by = "p",
      p.value = fdr_cut,
      number = Inf
    )
  } else {
    removeNotification(id = "dmps_calc")
    notification_id <- showNotification("Running ChAMP, please wait this might take a while ...", 
                                        type = "message", duration = NULL, id = "dmps_calc")
    # run with champ
    pheno <- data.frame(group = groups)
    rownames(pheno) <- colnames(beta_diff)
    
    dmps_champ_res <- ChAMP::champ.DMP(
      beta          = as.matrix(beta_diff),
      pheno         = pheno$group,
      compare.group = c("Baseline", "Comparison"),
      arraytype     = "EPIC",
      adjPVal       = fdr_cut,
      adjust.method = "BH"
    )
    
    # extract results
    dmps_prefilter <- dmps_champ_res$Baseline_to_Comparison
  }
  
  if (is.null(dmps_prefilter)) {
    dmps <- data.frame()
  } else {
    # filtering lfc
    dmps <- subset(dmps_prefilter, abs(logFC) > lfc_cut)
    
    if (nrow(dmps) > 0) {
      # Move CpG IDs to column
      dmps <- cbind(CpG = rownames(dmps), dmps)
      rownames(dmps) <- NULL
    }
  }
  
  # Save DMPs to disk
  tryCatch({
    if (nrow(dmps) > 0) {
      # Save as CSV
      csv_file <- file.path(out_dir, paste0("dmps_", ifelse(with_champ, "champ", "limma"), 
                                            "_", Sys.Date(), ".csv"))
      write.csv(dmps, csv_file, row.names = FALSE)
      
      # Save as XLSX if openxlsx is available
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        xlsx_file <- file.path(out_dir, paste0("dmps_", ifelse(with_champ, "champ", "limma"), 
                                               "_", Sys.Date(), ".xlsx"))
        openxlsx::write.xlsx(dmps, xlsx_file, row.names = FALSE)
      }
    } else {
      warning("No DMPs found to save")
    }
  }, error = function(e) {
    warning("Could not save DMPs: ", e$message)
  })
  
  removeNotification(id = "dmps_calc")
  return(dmps)
}


get_dmrs <- function(
    diff_met_data, 
    with_champ,
    out_dir
){
  shiny::validate(
    shiny::need(with_champ, "DMRs can only be calculated when 'Run ChAMP' is activated"),
  )
  beta_diff <- diff_met_data$beta_diff
  groups <- diff_met_data$groups_factor
  pheno <- data.frame(group = groups)
  rownames(pheno) <- colnames(beta_diff)
  
  notification_id <- showNotification("Computing DMRs...", 
                                      type = "message", duration = NULL, id = "dmrs_calc")
  
  # Fixed values
  champ_minProbes = 5
  champ_cores = m4a_threads_per_job()
  champ_dmr_method = "ProbeLasso"

  # ChAMP writes ProbeLasso output to resultsDir; left unset it defaults to
  # ./CHAMP_ProbeLasso/ in the shared working directory, where concurrent users
  # overwrite each other (and the dev bind mount puts it in the git tree).
  champ_results_dir <- file.path(out_dir, "champ_probelasso")
  dir.create(champ_results_dir, showWarnings = FALSE, recursive = TRUE)

  # run champ
  # ChAMP plots unconditionally, so swallow it into a throwaway device. Close only
  # that device — graphics devices are process-global and a bare dev.off() would
  # close whichever device another user's render happened to leave current.
  png(tempfile())
  champ_dev <- grDevices::dev.cur()
  on.exit({
    if (champ_dev %in% grDevices::dev.list()) grDevices::dev.off(champ_dev)
  }, add = TRUE)
  removeNotification(id = "dmrs_calc")
  notification_id <- showNotification("Running ChAMP, please wait this might take a while ...", 
                                      type = "message", duration = NULL, id = "dmrs_calc")
  dmrs_champ_res <- tryCatch({
    ChAMP::champ.DMR(
      beta          = as.matrix(beta_diff),
      pheno         = pheno$group,
      cores         = champ_cores,
      method        = champ_dmr_method,
      arraytype     = "EPIC",
      compare.group = c("Baseline", "Comparison"),
      resultsDir    = champ_results_dir,
      minProbes     = champ_minProbes
    )
  }, error = function(e) {
    message("champ.DMR failed: ", e$message)
    return(NULL)
  })

  # extract results
  dmrs <- dmrs_champ_res$ProbeLassoDMR
  
  if (is.null(dmrs)) {
    dmrs <- data.frame()
  } else {
    # Move DMR IDs to column
    dmrs <- cbind(DMRs = rownames(dmrs), dmrs)
    rownames(dmrs) <- NULL
  }
  
  removeNotification(id = "dmrs_calc")
  notification_id <- showNotification("ChAMP Finished running!", 
                                      type = "message", duration = NULL, id = "dmrs_calc")
  
  # Save DMRs to disk
  tryCatch({
    if (nrow(dmrs) > 0) {
      # Save as CSV
      csv_file <- file.path(out_dir, paste0("dmrs_", Sys.Date(), ".csv"))
      write.csv(dmrs, csv_file, row.names = FALSE)
      
      # Save as XLSX if openxlsx is available
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        xlsx_file <- file.path(out_dir, paste0("dmrs_", Sys.Date(), ".xlsx"))
        openxlsx::write.xlsx(dmrs, xlsx_file, row.names = FALSE)
      }
    } else {
      warning("No DMRs found to save")
    }
  }, error = function(e) {
    warning("Could not save DMRs: ", e$message)
  })
  
  removeNotification(id = "dmrs_calc")
  dmrs
}

get_dmgs <- function(
  diff_met_data,
  lfc_cut,
  out_dir
){
  toptab_gene_all <- diff_met_data$toptab_gene_all
  dmgs <- subset(toptab_gene_all, abs(logFC) > lfc_cut)
  
  if(nrow(dmgs) > 0){
    # Move DMG IDs to column
    dmgs <- cbind(DMGs = rownames(dmgs), dmgs)
    rownames(dmgs) <- NULL
  }
  
  # Save DMPs to disk
  tryCatch({
    if (nrow(dmgs) > 0) {
      # Save as CSV
      csv_file <- file.path(out_dir, paste0("dmgs_", Sys.Date(), ".csv"))
      write.csv(dmgs, csv_file, row.names = FALSE)
      
      # Save as XLSX if openxlsx is available
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        xlsx_file <- file.path(out_dir, paste0("dmgs_", Sys.Date(), "xlsx"))
        openxlsx::write.xlsx(dmgs, xlsx_file, row.names = FALSE)
      }
    } else {
      warning("No DMGs found to save")
    }
  }, error = function(e) {
    warning("Could not save DMGs: ", e$message)
  })
  
  return(dmgs)
}


get_fgsea <- function(
    diff_met_data,
    pathways,
    selected_pathway,
    out_dir
){
  notification_id <- showNotification(paste("Running FGSEA on", selected_pathway, "pathways..."), 
                                      type = "message", duration = NULL, id = "fgsea_calc")
                                      
                                      
  beta_diff <- diff_met_data$beta_diff
  toptab_gene_all <- diff_met_data$toptab_gene_all

  stats <- toptab_gene_all$logFC
  names(stats) <- rownames(toptab_gene_all)
  stats <- sort(stats, decreasing = TRUE)
  
  if(selected_pathway == "gobp"){
    fgsea_out <- tidy_fgsea(run_fgsea(stats, pathways$go_bp))
  } else if (selected_pathway == "kegg"){
    fgsea_out <- tidy_fgsea(run_fgsea(stats, pathways$kegg))
  } else if(selected_pathway == "hallmark"){
    fgsea_out <- tidy_fgsea(run_fgsea(stats, pathways$hallmarks))
  }
  fgsea_out <- as.data.frame(fgsea_out)
  
  # Save FGSEA results to disk
  tryCatch({
    if (nrow(fgsea_out) > 0) {
      # Save as CSV
      csv_file <- file.path(out_dir, paste0("fgsea_", selected_pathway, "_", Sys.Date(), ".csv"))
      write.csv(fgsea_out, csv_file, row.names = FALSE)
      
      # Save as XLSX if openxlsx is available
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        xlsx_file <- file.path(out_dir, paste0("fgsea_", selected_pathway, "_", Sys.Date(), ".xlsx"))
        openxlsx::write.xlsx(fgsea_out, xlsx_file, row.names = FALSE)
      }
    } else {
      warning("No FGSEA results found to save")
    }
  }, error = function(e) {
    warning("Could not save FGSEA results: ", e$message)
  })
  
  removeNotification(id = "fgsea_calc")
  return(fgsea_out)
}

