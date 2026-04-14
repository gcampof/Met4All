# Prepare CNV data
prepare_cnv_data <- function(
    mset_list,
    array_type,
    bed_path = NULL,
    chrXY, 
    comparison_col,
    baseline = NULL,      
    comparison = NULL
){
  # Validate baseline and comparison
  shiny::validate(
    shiny::need(length(baseline) > 0, "Please assign at least one level to Baseline"),
    shiny::need(length(comparison) > 0, "Please assign at least one level to Comparison"),
    shiny::need(
      length(intersect(baseline, comparison)) == 0,
      paste0("Levels cannot be in both groups: ",
             paste(intersect(baseline, comparison), collapse = ", "))
    )
  )
  
  # Validate BED file BEFORE using it (check existence and not NULL/empty)
  shiny::validate(
    shiny::need(!is.null(bed_path) && nzchar(bed_path),
                "Please upload a BED file"),
    shiny::need(file.exists(bed_path),
                "Selected BED file does not exist on disk")
  )
  
  notification_id <- showNotification("Preparing CNV data...", type="message", duration=3)
  
  # Fixed values
  genome = "hg19"
  mset_path <- mset_list[[array_type]]
  mset_object <- readRDS(mset_path)
  pd <- pData(mset_object)
  normal_ids <- rownames(pd)[pd[[comparison_col]] %in% baseline]
  case_ids   <- rownames(pd)[pd[[comparison_col]] %in% comparison]
  
  # Adapt array type for conumee2
  array_type <- tolower(trimws(array_type))
  if (grepl("450K", array_type)) {
    array_type <- "450k"
  } else if (grepl("epic_v2", array_type)) {
    array_type <- "EPICv2"
  }
  
  # Validate we have enough samples
  shiny::validate(
    shiny::need(length(normal_ids) >= 2, 
                "Too few normal samples"),
    shiny::need(length(case_ids) >= 1, 
                "Too few comparison samples")
  )
  
  mset_ctrl <- mset_object[, normal_ids]
  mset_case <- mset_object[, case_ids]
  
  cnv_ctrl <- CNV.load(mset_ctrl)
  cnv_case <- CNV.load(mset_case)
  
  removeNotification(notification_id)
  notification_id <- showNotification("Reading regions from BED file...", type="message", duration=0)
  detail_regions <- rtracklayer::import(bed_path, format = "bed")
  removeNotification(notification_id)
  notification_id <- showNotification("Creating annotation object...", type="message", duration=40)
  
  anno <- conumee2::CNV.create_anno(
    detail_regions = detail_regions,
    array_type     = array_type,
    chrXY          = chrXY,
    genome         = genome
  )
  
  probes_mset <- featureNames(mset_ctrl)
  probes_anno <- names(anno@probes)
  common_probes <- probes_anno %in% probes_mset
  
  anno_filtered <- anno
  anno_filtered@probes <- anno@probes[common_probes]
  
  removeNotification(notification_id)
  notification_id <- showNotification("Getting gene annotation for each bin...", type="message", duration=0)
  
  x <- CNV.fit(cnv_case, cnv_ctrl, anno_filtered)
  x <- CNV.bin(x)
  x <- CNV.detail(x)
  
  ratio_list <- x@bin$ratio
  var_list   <- x@bin$variance
  anno_bins  <- x@anno@bins
  
  na_var    <- vapply(var_list, function(v) sum(is.na(v)), integer(1))
  zero_var  <- vapply(var_list, function(v) sum(v == 0, na.rm = TRUE), integer(1))
  len_ratio <- vapply(ratio_list, length, integer(1))
  len_var   <- vapply(var_list,   length, integer(1))
  
  message("NA variances per sample:   ", paste(na_var, collapse = ", "))
  message("Zero variances per sample: ", paste(zero_var, collapse = ", "))
  message("Bin counts per sample (ratio): ", paste(len_ratio, collapse = ", "))
  message("Bin counts per sample (var):   ", paste(len_var,   collapse = ", "))
  message("Bin count in anno:             ", length(anno_bins))
  
  removeNotification(notification_id)
  notification_id <- showNotification(paste0("NA variances per sample: ",  
                                             paste(na_var, collapse = ", ")), type="message", duration=3)
  notification_id <- showNotification(paste0("Bin count in anno: ",  length(anno_bins)), type="message", duration=3) 
  
  bad_bins <- Reduce(`|`, lapply(var_list, is.na))
  keep <- !bad_bins
  
  x@bin$ratio    <- lapply(x@bin$ratio,    function(v) v[keep])
  x@bin$variance <- lapply(x@bin$variance, function(v) v[keep])
  x@anno@bins    <- x@anno@bins[keep]
  
  x <- CNV.segment(x)
  
  notification_id <- showNotification("Finished processing data!", type="message", duration=3) 
  
  x
}

# Plot and save pile-up plot
plot_pile_up <- function(cnv_data, baseline, comparison, out_dir) {
  notification_id <- showNotification("Generating pile-up plot...", 
                                      type = "message", duration = NULL, id = "pileup_plot")
  
  # Create filenames
  png_file <- file.path(out_dir, paste0("cnv_pileup_", Sys.Date(), ".png"))
  pdf_file <- file.path(out_dir, paste0("cnv_pileup_", Sys.Date(), ".pdf"))
  
  # Generate title
  title <- paste("Pile-up plot:", 
                 paste(baseline, collapse = ", "), 
                 "vs", 
                 paste(comparison, collapse = ", "))
  
  # Save as PNG
  png(png_file, width = 1000, height = 650, res = 100)
  conumee2::CNV.summaryplot(cnv_data, main = title)
  dev.off()
  
  # Save as PDF
  pdf(pdf_file, width = 10, height = 7)
  conumee2::CNV.summaryplot(cnv_data, main = title)
  dev.off()
  
  removeNotification(id = "pileup_plot")
  
  # Return the path to the PNG for rendering
  return(png_file)
}

# Plot and save per-sample plot
plot_cnv_per_sample <- function(cnv_data, sample_name, baseline, comparison, out_dir) {
  notification_id <- showNotification(paste("Generating plot for sample:", sample_name), 
                                      type = "message", duration = NULL, id = "sample_plot")
  
  # Sanitize sample name for filename
  sample_safe <- gsub("[^A-Za-z0-9]", "_", sample_name)
  
  # Create filenames
  png_file <- file.path(out_dir, paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".png"))
  pdf_file <- file.path(out_dir, paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".pdf"))
  
  # Find sample index
  sample_idx <- which(names(cnv_data@seg$summary) == sample_name)
  
  # Validate sample exists
  if (length(sample_idx) == 0) {
    removeNotification(id = "sample_plot")
    stop(paste("Sample not found:", sample_name))
  }
  
  # Generate title
  title <- paste("Genome Plot -", sample_name)
  
  # Save as PNG
  png(png_file, width = 1000, height = 650, res = 100)
  conumee2::CNV.genomeplot(cnv_data[sample_idx], main = title)
  dev.off()
  
  # Save as PDF
  pdf(pdf_file, width = 10, height = 7)
  conumee2::CNV.genomeplot(cnv_data[sample_idx], main = title)
  dev.off()
  
  removeNotification(id = "sample_plot")
  
  # Return the path to the PNG for rendering
  return(png_file)
}