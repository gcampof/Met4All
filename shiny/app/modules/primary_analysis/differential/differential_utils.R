# Run FGSEA for a specific pathway
run_fgsea <- function(stats, pws) {
  # Fixed values
  gsea_minSize = 30
  gsea_maxSize = 600
  seed = 123456
  
  # Scope the seed to this call. A bare set.seed() reseeds the RNG of the shared
  # R process, silently perturbing other users' UMAP and consensus clustering.
  # base:: qualified: config::get() masks base::get() in the app process.
  old_seed <- if (base::exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    base::get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(base::rm(".Random.seed", envir = globalenv()))
    } else {
      base::assign(".Random.seed", old_seed, envir = globalenv())
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

# GO ids are not readable on their own. Adds the term description next to the
# id, from GO.db, and leaves the table untouched if anything is unavailable.
add_go_terms <- function(df, id_col = "pathway") {
  if (!is.data.frame(df) || nrow(df) == 0 || !id_col %in% names(df)) return(df)
  ids <- as.character(df[[id_col]])
  if (!any(grepl("^GO:", ids))) return(df)
  terms <- tryCatch({
    # mapIds() errors on any key GO.db does not know (obsolete ids happen), so
    # ask only for the ones it has and leave the rest NA.
    known <- ids %in% AnnotationDbi::keys(GO.db::GO.db, keytype = "GOID")
    out   <- rep(NA_character_, length(ids))
    if (any(known)) {
      out[known] <- suppressMessages(AnnotationDbi::mapIds(
        GO.db::GO.db, keys = ids[known], column = "TERM",
        keytype = "GOID", multiVals = "first"))
    }
    out
  }, error = function(e) { warning("Could not add GO terms: ", conditionMessage(e)); NULL })
  if (is.null(terms)) return(df)
  rest <- df[, setdiff(names(df), id_col), drop = FALSE]
  data.frame(pathway = ids, TERM = unname(terms), rest,
             stringsAsFactors = FALSE, check.names = FALSE)
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
    comparison = NULL,
    region = "TSS200"
){
  if (length(baseline) == 0) stop("Please assign at least one level to Baseline")
  if (length(comparison) == 0) stop("Please assign at least one level to Comparison")
  if (length(intersect(baseline, comparison)) > 0) {
    stop("Levels cannot be in both groups: ",
         paste(intersect(baseline, comparison), collapse = ", "))
  }
  
  message("[diff] Preparing differential methylation data")
  # Prepare inputs
  align_res <- align_targets_to_beta_cols(beta, targets, id_col)
  beta2    <- align_res$beta2
  targets2 <- align_res$targets2
  
  # Keep only probes in annotation
  keep_probes <- rownames(beta2) %in% built_annot$Name
  beta2 <- beta2[keep_probes, ]
  
  # Gene-level matrix for the chosen region (promoter by default).
  tss200 <- methylation_genemat_dt(beta2, built_annot, group = region)
  # Reporting columns for the gene table (attributes set by the line above):
  # how many probes each median is based on, and how far apart they lie.
  n_region_probes <- attr(tss200, "n_probes")
  region_span_bp  <- attr(tss200, "span_bp")
  # tss200 <- methylation_genemat(beta2, built_annot)
  # NA filter for row names
  tss200 <- tss200[!is.na(rownames(tss200)), ]
  
  # Extract and clean groups
  groups <- trimws(as.character(targets2[[comparison_col]]))
  groups[groups == ""] <- NA
  
  # NA filter
  # The gene matrix must be filtered with the same mask as the beta matrix: it
  # was built from every sample, while `keep2` below is computed on the groups
  # AFTER this filter. Left unfiltered, R recycles the shorter logical index and
  # lmFit() fails with "row dimension of design doesn't match column dimension of
  # data object" -- but only for sample sheets that have a blank or NA group.
  keep     <- !is.na(groups)
  targets2 <- targets2[keep, , drop = FALSE]
  beta2    <- beta2[, keep, drop = FALSE]
  tss200   <- tss200[, keep, drop = FALSE]
  groups   <- groups[keep]

  # Recode groups in levels
  keep2  <- groups %in% c(baseline, comparison)
  beta2  <- beta2[, keep2, drop = FALSE]
  tss200_2    <- tss200[, keep2, drop = FALSE]
  groups_subset <- groups[keep2]
  groups_recoded <- ifelse(groups_subset %in% baseline, "Baseline", "Comparison")
  groups_factor <- factor(groups_recoded, levels = c("Baseline", "Comparison"))
  
  # Validate we have enough samples
  if (sum(groups_factor == "Baseline") < 2) stop("Need at least 2 samples in Baseline group")
  if (sum(groups_factor == "Comparison") < 2) stop("Need at least 2 samples in Comparison group")
  
  # Run limma on genes for later use (FGSEA)
  desing <- model.matrix(~ groups_factor)
  fit_tss  <- limma::lmFit(tss200_2, desing)
  fit2_tss <- limma::eBayes(fit_tss)
  toptab_gene_all <- limma::topTable(fit2_tss, adjust = "fdr", number = Inf, sort.by = "p")
  # First column: the number of probes summarised per gene, so a reader can see
  # whether a gene-level logFC rests on one probe or on twenty.
  if (!is.null(n_region_probes) && nrow(toptab_gene_all) > 0) {
    gene_ids <- rownames(toptab_gene_all)
    span <- if (is.null(region_span_bp)) {
      rep(NA_integer_, length(gene_ids))
    } else {
      as.integer(unname(region_span_bp[gene_ids]))
    }
    toptab_gene_all <- data.frame(
      `Probes in region` = as.integer(unname(n_region_probes[gene_ids])),
      `Probe span (bp)` = span,
      toptab_gene_all,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    rownames(toptab_gene_all) <- gene_ids
  }
  
  # Build comparison label for plot titles
  comparison_label <- paste0(
    "Baseline: [", paste(baseline, collapse = ", "), "]  vs  ",
    "Comparison: [", paste(comparison, collapse = ", "), "]"
  )
  list(
    beta_diff = beta2,
    groups_factor = groups_factor,
    limma_desing = desing,
    toptab_gene_all = toptab_gene_all,
    comparison_label = comparison_label
  )
}


plot_diff_methylation_density <- function(diff_met_data, color_palette, out_dir) {
  message("[diff] Generating density plot")
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
  
  draw <- function() {
    minfi::densityPlot(
      as.matrix(group_means_mat),
      sampGroups = group_levels,
      main = paste("Mean density plot - ", comparison_label),
      xlab = "Beta",
      pal = matched_colors
    )
  }

  # Rendered to files only: this runs in a worker with no screen device, and the
  # UI displays the PNG. Previously the same plot was drawn three times.
  png_file <- file.path(out_dir, paste0("density_plot_", Sys.Date(), ".png"))
  pdf_file <- file.path(out_dir, paste0("density_plot_", Sys.Date(), ".pdf"))
  svg_file <- file.path(out_dir, paste0("density_plot_", Sys.Date(), ".svg"))

  tryCatch({
    # ~2x for the same reason as the CNV plots: shown at width:100%.
    png(png_file, width = 2000, height = 1600, res = 300)
    on.exit(if (dev.cur() != 1L) dev.off(), add = TRUE)
    draw()
    dev.off()

    pdf(pdf_file, width = 10, height = 8)
    draw()
    dev.off()

    svglite::svglite(svg_file, width = 10, height = 8)
    draw()
    dev.off()
  }, error = function(e) {
    warning("Could not save density plot: ", e$message)
  })

  png_file
}


get_dmps <- function(diff_met_data, 
                     fdr_cut, 
                     lfc_cut, 
                     with_champ,
                     out_dir
){
  message("[diff] Computing DMPs")
  beta_diff <- diff_met_data$beta_diff
  groups <- diff_met_data$groups_factor
  
  if(!with_champ) {
    # run with limma
    message("[diff] Running limma")
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
    message("[diff] Running ChAMP, this might take a while")
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
  
  return(dmps)
}


get_dmrs <- function(
    diff_met_data, 
    with_champ,
    out_dir
){
  if (!isTRUE(with_champ)) stop("DMRs can only be calculated when 'Run ChAMP' is activated")
  beta_diff <- diff_met_data$beta_diff
  groups <- diff_met_data$groups_factor
  pheno <- data.frame(group = groups)
  rownames(pheno) <- colnames(beta_diff)
  
  message("[diff] Computing DMRs")
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
  message("[diff] Running ChAMP, please wait this might take a while ")
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
  
  message("[diff] ChAMP Finished running!")
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
  
  dmrs
}

# EPICv2 DMRs following the DMRcate EPICv2 vignette: cross-hybridising probes are
# remapped and replicates averaged by cpg.annotate(), then DMRs are called on hg38.
get_dmrs_dmrcate <- function(diff_met_data, out_dir) {
  message("[diff] Computing DMRs with DMRcate")
  beta <- as.matrix(diff_met_data$beta_diff)
  # cpg.annotate() fits on M-values, which are infinite at beta 0 or 1.
  beta <- beta[matrixStats::rowAlls(beta > 0 & beta < 1) %in% TRUE, , drop = FALSE]

  annot <- DMRcate::cpg.annotate("array", beta, what = "Beta", arraytype = "EPICv2",
                                 epicv2Remap = TRUE, epicv2Filter = "mean",
                                 analysis.type = "differential",
                                 design = diff_met_data$limma_desing, coef = 2)
  if (!any(annot@ranges$is.sig)) return(data.frame())

  ranges <- DMRcate::extractRanges(DMRcate::dmrcate(annot, lambda = 1000, C = 2),
                                   genome = "hg38")
  dmrs <- as.data.frame(ranges)
  dmrs <- cbind(DMRs = paste0(dmrs$seqnames, ":", dmrs$start, "-", dmrs$end), dmrs)

  if (nrow(dmrs) > 0) {
    write.csv(dmrs, file.path(out_dir, paste0("dmrs_", Sys.Date(), ".csv")), row.names = FALSE)
    openxlsx::write.xlsx(dmrs, file.path(out_dir, paste0("dmrs_", Sys.Date(), ".xlsx")))
  }
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
        xlsx_file <- file.path(out_dir, paste0("dmgs_", Sys.Date(), ".xlsx"))
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
  message("[diff] Running FGSEA on")
  beta_diff <- diff_met_data$beta_diff
  toptab_gene_all <- diff_met_data$toptab_gene_all

  stats <- toptab_gene_all$logFC
  names(stats) <- rownames(toptab_gene_all)
  stats <- sort(stats, decreasing = TRUE)
  
  if(selected_pathway == "gobp"){
    fgsea_out <- tidy_fgsea(run_fgsea(stats, pathways$go_bp))
    fgsea_out <- add_go_terms(fgsea_out)
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
  
  return(fgsea_out)
}



# Whole differential pipeline, run inside one worker job.
#
# beta_diff (the subsetted beta matrix) is the largest object in this analysis and
# is needed by every step, so the entire pipeline runs where it lives and only the
# display tables and file paths come back. Inputs are a path plus small values;
# `targets` is passed by value because the samplesheet is editable in-session and
# the copy on disk may be stale.
#
# DMPs are fitted at fdr_max (the top of the UI slider) so the caller can apply
# the user's FDR, logFC and row-count choices as cheap post-filters instead of
# re-running the fit on every slider drag.
run_differential_analysis <- function(
    beta_path,
    targets,
    cache_dir,
    pathways_dir,
    annotation_pkg,
    gene_set,
    palette_dir,
    palette_name,
    id_col,
    comparison_col,
    baseline,
    comparison,
    with_champ,
    fdr_max,
    with_dmrcate = FALSE,
    out_dir
) {
  # Pathway enrichment is no longer part of this run: it is chosen and launched
  # from the Gene-set enrichment tab, one region and one collection at a time.
  n_steps <- if (isTRUE(with_champ) || isTRUE(with_dmrcate)) 6L else 5L
  m4a_progress(0, n_steps, "Loading beta matrix and annotation")
  beta  <- readRDS(beta_path)
  cache <- setup_cache(
    DIRS = list(cache = cache_dir, pathways = pathways_dir),
    cfg  = list(annotation_pkg = annotation_pkg, gene_set = gene_set)
  )

  m4a_progress(1, n_steps, "Summarising probes to genes")
  diff <- prepare_differential_methylation_data(
    beta, targets, cache$built_annot,
    id_col, comparison_col, baseline, comparison
  )

  m4a_progress(2, n_steps, "Fitting differentially methylated positions")
  # Fitted once at the permissive end of the slider; filtered by the caller.
  dmps_all <- get_dmps(diff, fdr_cut = fdr_max, lfc_cut = 0,
                       with_champ = with_champ, out_dir = out_dir)

  # Background for missMethyl: every CpG the DMP fit was run on.
  all_cpg_path <- file.path(out_dir, "dmp_tested_cpgs.rds")
  tryCatch(saveRDS(rownames(diff$beta_diff), all_cpg_path),
           error = function(e) {
             warning("Could not save the tested CpG list: ", conditionMessage(e))
             all_cpg_path <<- NA_character_
           })

  m4a_progress(3, n_steps, "Collecting differentially methylated genes")
  dmgs <- get_dmgs(diff, 0, out_dir)

  # ChAMP DMRs are opt-in and by far the slowest step.
  if (isTRUE(with_champ)) m4a_progress(4, n_steps, "Detecting DMRs with ChAMP (slow)")
  if (isTRUE(with_dmrcate)) m4a_progress(4, n_steps, "Detecting DMRs with DMRcate (slow)")
  dmrs <- if (isTRUE(with_champ)) {
    tryCatch(get_dmrs(diff, TRUE, out_dir),
             error = function(e) { warning("DMRs failed: ", conditionMessage(e)); data.frame() })
  } else if (isTRUE(with_dmrcate)) {
    tryCatch(get_dmrs_dmrcate(diff, out_dir),
             error = function(e) { warning("DMRs failed: ", conditionMessage(e)); data.frame() })
  } else {
    data.frame()
  }

  palettes <- prepare_color_palettes(palette_dir)
  pal_fn   <- palettes$all_palettes[[palette_name]]
  if (is.null(pal_fn)) pal_fn <- palettes$all_palettes[[1]]

  m4a_progress(n_steps - 1L, n_steps, "Drawing the density plot")
  density_png <- tryCatch(
    plot_diff_methylation_density(diff, pal_fn, out_dir),
    error = function(e) { warning("Density plot failed: ", conditionMessage(e)); NULL }
  )

  m4a_progress(n_steps, n_steps, "Differential methylation complete", check = FALSE)

  list(
    dmps_all         = dmps_all,
    all_cpg_path     = all_cpg_path,
    dmrs             = dmrs,
    dmgs             = dmgs,
    density_png      = density_png,
    comparison_label = diff$comparison_label,
    with_champ       = isTRUE(with_champ),
    with_dmrcate     = isTRUE(with_dmrcate),
    n_baseline       = sum(diff$groups_factor == "Baseline"),
    n_comparison     = sum(diff$groups_factor == "Comparison")
  )
}


# ---- missMethyl gene-set testing --------------------------------------------
# Complements the gene-level route above rather than replacing it. Met4All
# summarises promoter probes to a gene median and runs limma on genes, so its
# fgsea ranking has no CpG-selection step and no probe-number bias to correct.
# missMethyl works the other way round: it takes the list of significant CpGs
# and corrects, per gene set, for the fact that genes covered by more probes are
# more likely to contain a significant one. Offering both means the whole gene
# can be tested (not only the promoter), with that correction applied.

# gsameth() works on Entrez IDs; the MSigDB GMTs we ship are gene symbols.
symbols_to_entrez_sets <- function(gmt) {
  syms <- unique(unlist(gmt, use.names = FALSE))
  map  <- suppressMessages(AnnotationDbi::mapIds(
    org.Hs.eg.db::org.Hs.eg.db, keys = syms, column = "ENTREZID",
    keytype = "SYMBOL", multiVals = "first"))
  sets <- lapply(gmt, function(g) unique(unname(map[g][!is.na(map[g])])))
  sets[lengths(sets) > 0]
}

# array_type: "450K", "EPIC" or "EPIC_V2", matching missMethyl's array.type, or
# "auto" for uploaded beta matrices whose array is unknown. EPICv2-only runs keep
# native (suffixed) EPICv2 IDs, which is exactly what missMethyl's EPIC_V2
# annotation is keyed by, so nothing has to be renamed.
# The gene sets are FGSEA's own (setup_cache()$pathways), so both methods test the
# same collections and nothing has to be fetched online.
run_missmethyl_gst <- function(
    sig_cpg,
    all_cpg_path,
    collection       = c("gobp", "kegg", "hallmark"),
    genomic_features = "ALL",
    array_type       = c("EPIC", "450K", "EPIC_V2", "auto"),
    sig_genes        = FALSE,
    cache_dir,
    pathways_dir,
    annotation_pkg,
    gene_set,
    out_dir
) {
  collection <- match.arg(collection)
  array_type <- match.arg(array_type)
  if (!requireNamespace("missMethyl", quietly = TRUE)) {
    stop("The missMethyl package is not installed in this image.")
  }
  if (is.na(all_cpg_path) || !file.exists(all_cpg_path)) {
    stop("The tested-CpG list is missing; re-run the differential methylation analysis.")
  }

  all_cpg <- readRDS(all_cpg_path)
  sig_cpg <- intersect(unique(as.character(sig_cpg)), all_cpg)
  if (length(sig_cpg) < 10) {
    stop("Only ", length(sig_cpg), " significant CpGs at the current thresholds; ",
         "gene-set testing needs at least 10. Relax the FDR or logFC cut-off.")
  }

  # 450K data carry ~30k probes EPICv1 dropped, so a beta matrix that is not
  # (almost) all EPICv1 probes is 450K.
  if (identical(array_type, "auto")) {
    epic <- rownames(minfi::getAnnotation(
      IlluminaHumanMethylationEPICanno.ilm10b4.hg19::IlluminaHumanMethylationEPICanno.ilm10b4.hg19,
      what = "Locations"))
    array_type <- if (mean(all_cpg %in% epic) < 0.98) "450K" else "EPIC"
  }
  # ExonBnd is an EPICv1-only annotation group; missMethyl stops on it elsewhere.
  features <- if (array_type == "EPIC") genomic_features else setdiff(genomic_features, "ExonBnd")

  pathways <- setup_cache(
    DIRS = list(cache = cache_dir, pathways = pathways_dir),
    cfg  = list(annotation_pkg = annotation_pkg, gene_set = gene_set)
  )$pathways
  sets <- symbols_to_entrez_sets(switch(collection,
                                        gobp     = pathways$go_bp,
                                        kegg     = pathways$kegg,
                                        hallmark = pathways$hallmarks))

  m4a_progress(0, 2, paste0("Testing ", length(sets), " ", toupper(collection),
                            " gene sets with missMethyl (", length(sig_cpg),
                            " significant CpGs). GO BP takes a few minutes."))
  res <- missMethyl::gsameth(sig.cpg = sig_cpg, all.cpg = all_cpg, collection = sets,
                             array.type = array_type,
                             genomic.features = features, sig.genes = sig_genes)

  m4a_progress(1, 2, "Saving missMethyl results", check = FALSE)
  res <- as.data.frame(res)
  res <- cbind(ID = rownames(res), res)
  rownames(res) <- NULL
  if ("P.DE" %in% names(res)) res <- res[order(res$P.DE), , drop = FALSE]
  if (identical(collection, "gobp")) res <- add_go_terms(res, id_col = "ID")

  stem <- paste0("missmethyl_", collection, "_", Sys.Date())
  tryCatch({
    write.csv(res, file.path(out_dir, paste0(stem, ".csv")), row.names = FALSE)
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      openxlsx::write.xlsx(res, file.path(out_dir, paste0(stem, ".xlsx")), rowNames = FALSE)
    }
  }, error = function(e) warning("Could not save the missMethyl results: ", conditionMessage(e)))

  m4a_progress(2, 2, "missMethyl complete", check = FALSE)
  list(
    table            = res,
    collection       = collection,
    genomic_features = genomic_features,
    array_type       = array_type,
    n_sig            = length(sig_cpg),
    n_all            = length(all_cpg),
    file_stem        = stem
  )
}


# ---- Gene-set enrichment over all CpGs --------------------------------------
# The gene-median route, run on demand for one region and one collection.
# Summarises the region's probes to a gene median, fits limma on genes, and
# ranks every gene by logFC for fgsea -- no CpG is selected, so there is no
# probe-number bias to correct. Returns the gene table too: it is the same
# object the DMGs tab shows, now tied to the region actually chosen.
run_gene_set_fgsea <- function(
    beta_path,
    targets,
    cache_dir,
    pathways_dir,
    annotation_pkg,
    gene_set,
    id_col,
    comparison_col,
    baseline,
    comparison,
    region     = "TSS200",
    collection = "gobp",
    out_dir
) {
  m4a_progress(0, 3, "Loading beta matrix and annotation")
  beta  <- readRDS(beta_path)
  cache <- setup_cache(
    DIRS = list(cache = cache_dir, pathways = pathways_dir),
    cfg  = list(annotation_pkg = annotation_pkg, gene_set = gene_set)
  )

  m4a_progress(1, 3, paste0("Summarising probes to genes (", region, ")"))
  diff <- prepare_differential_methylation_data(
    beta, targets, cache$built_annot,
    id_col, comparison_col, baseline, comparison,
    region = region
  )
  rm(beta)
  dmgs <- get_dmgs(diff, 0, out_dir)

  m4a_progress(2, 3, paste0("Running FGSEA (", collection, ")"))
  table <- get_fgsea(diff, cache$pathways, collection, out_dir)

  m4a_progress(3, 3, "Gene-set analysis complete", check = FALSE)
  list(
    table      = table,
    dmgs       = dmgs,
    region     = region,
    collection = collection,
    n_genes    = nrow(diff$toptab_gene_all)
  )
}
