ARRAY_SUPPORTED <- list(
  `450K` = c("450k", "450", "hm450", "hm_450", "illumina 450k"),
  EPIC = c("epic","epicv1", "epic_v1", "epic_1", "epic v1", "epicv.1"),
  EPIC_V2 = c("epicv2", "epic_v2", "epic_2", "epic v2", "epicv.2")
)

# === load_data_helper.R ===
col_vector<-c(
  "#0d570b", "#54c40a", "#E41A1C", "#d97009", "#52367d", "#874c23", "#bfa21d",
  "#6998b5", "#8c8b8b", "#03465e",  "#e080c3", "#9d83d6", "#14b89c", "#2a4880",
  "#86f793", "#A6761D", "#E31A1C", "#FCCDE5", "#E6AB02", "#F4CAE4", "#FFF2AE",
  "#F781BF", "#CCEBC5", "#8DA0CB", "#E78AC3", "#A6CEE3", "#FFFFCC", "#7570B3",
  "#666666", "#984EA3", "#7FC97F", "#FC8D62", "#CCCCCC", "#CAB2D6", "#F2F2F2",
  "#B2DF8A", "#FDC086", "#FFFF33", "#CCEBC5", "#80B1D3", "#D9D9D9", "#FBB4AE",
  "#4DAF4A", "#66C2A5", "#E6F5C9", "#8DD3C7", "#1F78B4", "#B15928", "#FED9A6",
  "#E7298A", "#D95F02", "#FDDAEC", "#B3B3B3", "#FF7F00", "#FFFFB3", "#A6D854",
  "#33A02C", "#FDB462", "#386CB0", "#BEBADA", "#E5C494", "#B3E2CD", "#FDBF6F",
  "#E5D8BD", "#6A3D9A", "#1B9E77", "#FFED6F", "#B3CDE3", "#A65628", "#FDCDAC",
  "#BEAED4", "#999999", "#F1E2CC"
)


# Creates the new directory and returns the path
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

get_array_types <- function(){
  names(ARRAY_SUPPORTED)
}


get_array_synonims <- function(){
  ARRAY_SUPPORTED
}


is_samplesheet <- function(cols) {
  cols <- tolower(cols)
  # Just need at least one identifier column
  any(c("basename", "sentrixid", "sentrix_id") %in% cols)
}


normalize_array_type <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  
  # Store original for debugging if needed
  original_x <- x
  
  # Convert to lowercase for comparison only
  x_lower <- tolower(trimws(x))
  
  for (array_type in names(ARRAY_SUPPORTED)) {
    # Get synonyms and convert to lowercase for comparison
    synonyms <- tolower(trimws(ARRAY_SUPPORTED[[array_type]]))
    if (x_lower %in% synonyms) {
      # Return the array_type as is (preserves original case)
      return(array_type)
    }
  }
  
  NA_character_
}


order_idat_per_array <- function(idats_dir, preprocessing_dir){
  if (!dir.exists(idats_dir)) {
    stop("idats_dir does not exist: ", idats_dir)
  }
  
  # Supported arrays
  array_types <- get_array_types()
  array_paths <- file.path(preprocessing_dir, array_types)
  
  # Create missing directories
  for (path in array_paths) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Update idat_files to point to new location
  idat_files <- list.files(idats_dir, pattern = "\\.idat$", full.names = TRUE, 
                           ignore.case = TRUE)
  
  # Size thresholds (bytes)
  SIZE_450K_MAX  <- 10000000
  SIZE_EPIC_MAX  <- 14000000
  
  # Classify by size
  idat_sizes <- sapply(idat_files, file.size)
  idat_450k <- idat_files[idat_sizes <= SIZE_450K_MAX]
  idat_epic <- idat_files[idat_sizes > SIZE_450K_MAX & idat_sizes <= SIZE_EPIC_MAX]
  idat_epic_v2 <- idat_files[idat_sizes > SIZE_EPIC_MAX]
  
  file.rename(idat_450k, file.path(preprocessing_dir, "450K", basename(idat_450k)))
  file.rename(idat_epic, file.path(preprocessing_dir, "EPIC", basename(idat_epic)))
  file.rename(idat_epic_v2, file.path(preprocessing_dir, "EPIC_V2", basename(idat_epic_v2)))
}



idat_exists <- function(slide, array, array_type, preprocessing_dir) {
  dir <- file.path(preprocessing_dir, array_type)
  if (!dir.exists(dir)) return(FALSE)
  
  pattern <- paste0("^", slide, "_", array, "_.*\\.idat$")
  
  length(
    list.files(
      dir,
      pattern = pattern,
      full.names = FALSE,
      ignore.case = TRUE
    )
  ) > 0
}

parse_idat_files <- function(input_dir, preprocessing_dir) {
  # Find all IDAT files (including .idat.gz)
  idat_files <- list.files(
    input_dir, 
    pattern = "\\.idat(\\.gz)?$", 
    full.names = TRUE, 
    recursive = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(idat_files) == 0) {
    stop("No IDAT files found in the input directory.")
  }
  
  # Create idats folder
  idats_dir <- create_dir(file.path(preprocessing_dir, "idats"))
  
  # Copy and decompress if needed
  for (idat_file in idat_files) {
    if (grepl("\\.gz$", idat_file, ignore.case = TRUE)) {
      # Decompress .idat.gz
      output_file <- file.path(idats_dir, sub("\\.gz$", "", basename(idat_file), ignore.case = TRUE))
      R.utils::gunzip(idat_file, destname = output_file, overwrite = TRUE, remove = FALSE)
    } else {
      # Copy uncompressed .idat
      file.copy(idat_file, file.path(idats_dir, basename(idat_file)), overwrite = TRUE)
    }
  }
  return(idats_dir)
}


parse_samplesheets <- function(input_dir, preprocessing_dir) {
  
  # Find all CSV and XLSX files
  ss_files <- list.files(
    input_dir,
    pattern = "\\.(csv|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )
  
  # Find all IDATS
  all_idats <- list.files(
    preprocessing_dir,
    pattern = "\\.idat$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )
  
  if (length(ss_files) == 0) {
    stop("No CSV/XLSX files found in input directory.")
  }
  
  for (file in ss_files) {
    
    # Read file
    df <- tryCatch({
      if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
        readxl::read_excel(file)
      } else {
        readr::read_csv(file, show_col_types = FALSE)
      }
    }, error = function(e) NULL)
    
    if (is.null(df)) next
    
    cols <- colnames(df)
    cols_l <- tolower(cols)
    
    # ---- Determine which columns exist ----
    has_slide   <- any(cols_l == "slide")
    has_array   <- any(cols_l == "array")
    has_basename <- any(cols_l == "basename")
    has_sample_name <- any(cols_l == "sample_name")
    
    if (!has_slide && !has_array && !has_basename && !has_sample_name) {
      message("Skipping file (no slide/array, basename, or sample_name): ", basename(file))
      next
    }
    
    # ---- Prepare a unified identifier (base_name) for each row ----
    df$BaseName <- NA_character_
    
    # Case 1: Slide + Array columns exist
    if (has_slide && has_array) {
      slide_col <- cols[cols_l == "slide"][1]
      array_col <- cols[cols_l == "array"][1]
      for (i in seq_len(nrow(df))) {
        slide_val <- as.character(df[[slide_col]][i])
        array_val <- as.character(df[[array_col]][i])
        if (!is.na(slide_val) && !is.na(array_val)) {
          df$BaseName[i] <- paste0(slide_val, "_", array_val)
        }
      }
      message("Using Slide + Array to build base name.")
    }
    
    # Case 2: Basename column exists
    if (has_basename && any(is.na(df$BaseName))) {
      basename_col <- cols[cols_l == "basename"][1]
      
      # Create Slide and Array columns if they don't exist
      if (!any(cols_l == "slide")) {
        df$Slide <- NA_character_
      }
      if (!any(cols_l == "array")) {
        df$Array <- NA_character_
      }
      
      for (i in seq_len(nrow(df))) {
        if (is.na(df$BaseName[i])) {
          base_val <- as.character(df[[basename_col]][i])
          # Remove path and any trailing _Grn/_Red if present
          base_val <- basename(base_val)
          base_val <- sub("_(Grn|Red)$", "", base_val, ignore.case = TRUE)
          df$BaseName[i] <- base_val
          
          # Extract Slide and Array from Basename
          # Expected format: Slide_Array or Sample_Slide_Array or just Basename
          parts <- strsplit(base_val, "_")[[1]]
          
          if (length(parts) >= 2) {
            # If we have at least 2 parts, assume last part is Array, second last is Slide
            df$Array[i] <- parts[length(parts)]  # Last part is Array position
            df$Slide[i] <- parts[length(parts) - 1]  # Second last is Slide ID
          } else if (length(parts) == 1) {
            # Only one part, use as is for both (fallback)
            df$Slide[i] <- parts[1]
            df$Array[i] <- parts[1]
          }
        }
      }
      message("Using Basename column and extracted Slide/Array.")
    }
    # Case 3: Only Sample_Name (fallback)
    if (has_sample_name && any(is.na(df$BaseName))) {
      sample_col <- cols[cols_l == "sample_name"][1]
      for (i in seq_len(nrow(df))) {
        if (is.na(df$BaseName[i])) {
          df$BaseName[i] <- as.character(df[[sample_col]][i])
        }
      }
      message("Using Sample_Name column (fallback).")
    }
    
    # Remove rows where BaseName is still NA
    df <- df[!is.na(df$BaseName), , drop = FALSE]
    if (nrow(df) == 0) {
      message("No valid base names after processing, skipping: ", basename(file))
      next
    }
    
    # ---- Add Array_Type column if missing ----
    if (!any(cols_l == "array_type")) {
      df$Array_Type <- NA_character_
    }
    
    # ---- Add Sample_Group column if missing ----
    if (!any(cols_l == "sample_group")) {
      df$Sample_Group <- NA_character_
    }
    
    # ---- Find matching IDAT files ----
    keep <- logical(nrow(df))
    for (i in seq_len(nrow(df))) {
      base_name <- df$BaseName[i]
      
      # Look for any IDAT starting with base_name (case‑insensitive)
      # Pattern: base_name followed by optional _Grn/_Red and .idat
      pattern <- paste0("^", base_name, ".*\\.idat$")
      idats <- all_idats[grepl(paste0("^", base_name), basename(all_idats), ignore.case = TRUE)]
      
      if (length(idats) > 0) {
        keep[i] <- TRUE
        # Extract array type from the folder containing the first IDAT
        idat_path <- idats[1]
        array_type_name <- basename(dirname(idat_path))
        df$Array_Type[i] <- array_type_name
        # Set array type as sample group if it doesn't exist
        if(is.na(df$Sample_Group[i])){
          df$Sample_Group[i] <- array_type_name
        }
      }
    }
    
    df_clean <- df[keep, , drop = FALSE]
    if (nrow(df_clean) == 0) {
      message("No IDAT files found for any sample in: ", basename(file))
      next
    }
    
    # ---- Standardise Array_Type ----
    df_clean$Array_Type <- sapply(df_clean$Array_Type, normalize_array_type)
    
    # Remove rows where Array_Type became NA (should not happen if folder names are correct)
    df_clean <- df_clean[!is.na(df_clean$Array_Type), , drop = FALSE]
    
    # ---- Write per‑array sample sheets ----
    for (array_type in unique(df_clean$Array_Type)) {
      array_dir <- file.path(preprocessing_dir, array_type)
      if (!dir.exists(array_dir)) {
        dir.create(array_dir, recursive = TRUE)
      }
      df_array <- df_clean[df_clean$Array_Type == array_type, , drop = FALSE]
      # Optionally drop helper columns like BaseName if you don't want them in final sheet
      # df_array <- df_array[, !(colnames(df_array) %in% "BaseName")]
      readr::write_csv(df_array, file.path(array_dir, "SampleSheet.csv"))
      message("Saved SampleSheet for ", array_type, " (", nrow(df_array), " samples)")
    }
  }
  
  invisible(TRUE)
}



generate_idat_dataframe <- function(preprocessing_dir) {
  array_types <- get_array_types()
  array_paths <- file.path(preprocessing_dir, array_types)
  
  rows <- vector("list", length(array_types))
  
  for (i in seq_along(array_types)) {
    # Find all IDATS in the array folders
    idats <- list.files(
      array_paths[i],
      pattern = "\\.idat$",
      full.names = FALSE,
      ignore.case = TRUE
    )
    
    if (length(idats) == 0) next
    
    prefixes <- unique(
      sub("_(Grn|Red|Gr|Rd)\\.idat$", "", idats, ignore.case = TRUE)
    )
    
    # Extract the slide and the array part
    parts <- strsplit(prefixes, "_")
    
    # Prepare new rows with array information for summary
    rows[[i]] <- data.frame(
      Slide = vapply(parts, `[`, character(1), 1),
      Array = vapply(parts, `[`, character(1), 2),
      Array_Type = array_types[i],
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(rows)
}

# Remove unselected idats to a different folder
separate_unselected_idats <- function(idat_df, selected_indices, preprocessing_dir) {
  
  if (nrow(idat_df) == 0) {
    stop("Empty IDAT dataframe provided.")
  }
  
  if (is.null(selected_indices)) {
    selected_indices <- integer(0)
  }
  
  # Identify unselected samples
  unselected_df <- idat_df[-selected_indices, , drop = FALSE]
  
  if (nrow(unselected_df) == 0) {
    message("All samples selected. Nothing to move.")
    return(invisible(NULL))
  }
  
  # Create not_used folder
  not_used_dir <- create_dir(file.path(preprocessing_dir, "not_used"))
  
  # Loop over unselected samples
  for (i in seq_len(nrow(unselected_df))) {
    
    slide      <- unselected_df$Slide[i]
    array_pos  <- unselected_df$Array[i]
    array_type <- unselected_df$Array_Type[i]
    
    source_dir <- file.path(preprocessing_dir, array_type)
    
    if (!dir.exists(source_dir)) {
      warning("Source directory does not exist: ", source_dir)
      next
    }
    
    # Match both Grn and Red IDATs
    pattern <- paste0("^", slide, "_", array_pos, "_.*\\.idat$")
    
    files_to_move <- list.files(
      source_dir,
      pattern = pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(files_to_move) == 0) {
      warning("No IDAT files found for sample: ", slide, "_", array_pos)
      next
    }
    
    ok <- file.rename(
      files_to_move,
      file.path(not_used_dir, basename(files_to_move))
    )
    
    if (!all(ok)) {
      warning(
        "Some IDAT files could not be moved for sample: ",
        slide, "_", array_pos
      )
    }
  }
  invisible(TRUE)
}


generate_detection_p_barplot_subset <- function(array, rgSet, detP, threshold, total_samples, kept_samples) {
  mean_detP <- colMeans(detP, na.rm = TRUE)
  
  plot_df <- data.frame(
    Sample = colnames(detP),
    MeanDetP = mean_detP,
    Group = factor(rgSet$Sample_Group)
  )
  
  # Data is already sorted by MeanDetP descending from the subsetting step
  # Just ensure factor levels maintain the order
  plot_df <- plot_df[order(-plot_df$MeanDetP), ]
  plot_df$Sample <- factor(plot_df$Sample, levels = plot_df$Sample)
  
  ggplot(plot_df, aes(x = Sample, y = MeanDetP, fill = Group)) +
    geom_col() +
    scale_fill_manual(values = col_vector) +
    geom_hline(
      yintercept = threshold,
      color = "red",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = paste("Mean detection p-values for", array,
                    sprintf("(Top %d of %d samples)", kept_samples, total_samples)),
      y = "Mean detection p-value",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      legend.title = element_blank()
    )
}


generate_detection_p_barplot <- function(array, rgSet, detP, threshold) {
  mean_detP <- colMeans(detP, na.rm = TRUE)
  
  plot_df <- data.frame(
    Sample = colnames(detP),
    MeanDetP = mean_detP,
    Group = factor(rgSet$Sample_Group)
  )
  
  ggplot(plot_df, aes(x = Sample, y = MeanDetP, fill = Group)) +
    geom_col() +
    scale_fill_manual(values = col_vector) +
    geom_hline(
      yintercept = threshold,
      color = "red",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = paste("Mean detection p-values for", array),
      y = "Mean detection p-value",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
      legend.title = element_blank()
    )
}


load_qc_data_for_arrays_batch <- function(preprocessing_dir, qc_dir) {
  batch_size = 110
  array_types <- get_array_types()
  arrays_used <- c()
  
  # First pass: identify which arrays have data
  for (array in array_types) {
    array_prep_dir <- file.path(preprocessing_dir, array)
    if (!dir.exists(array_prep_dir) ||
        length(list.files(array_prep_dir, pattern = "\\.idat$", recursive = TRUE)) == 0) {
      notification_id <- showNotification(paste0("Skipping empty or missing array directory: ", 
                                                 array), type="message", duration=2)
      message("Skipping empty or missing array directory: ", array)
      next
    }
    create_dir(file.path(qc_dir, array))
    arrays_used <- c(arrays_used, array)
    
    notification_id <- showNotification(paste0("Found array: ", array), type="message", duration=2)
    message("Found array: ", array)
  }
  
  if (length(arrays_used) == 0) {
    notification_id <- showNotification("No arrays found to process", type="error", duration=0)
    stop("No arrays found to process")
  }
  
  # Initialize results
  qc_results <- list(
    rgsets = list(),
    detections = list()
  )
  
  # Process arrays ONE AT A TIME
  for (array in arrays_used) {
    message(paste0("\n=== Processing ", array, " ==="))
    notification_id <- showNotification(paste0("Processing: ", array), type="message", duration=0)
    
    array_prep_dir <- file.path(preprocessing_dir, array)
    array_qc_dir <- file.path(qc_dir, array)
    
    # Load targets
    targets <- minfi::read.metharray.sheet(array_prep_dir, pattern = "SampleSheet.csv")
    n_samples <- nrow(targets)
    
    removeNotification(notification_id)
    notification_id <- showNotification(paste0("Loaded targets for ", n_samples, " samples"), type="message", duration=0)
    message("Loaded targets for ", n_samples, " samples")
    
    if (array == "450K") {
      library(IlluminaHumanMethylation450kmanifest)
    } else if (array == "EPIC") {
      library(IlluminaHumanMethylationEPICmanifest)
    } else if (array == "EPIC_V2") {
      library(IlluminaHumanMethylationEPICv2manifest)
    } 
    
    # Determine if we need batch processing
    if (n_samples > batch_size & array != "EPIC_V2") {
      removeNotification(notification_id)
      notification_id <- showNotification(paste0("Large dataset detected (", n_samples, " samples). Using batch processing"), 
                       type = "message", duration =0)
      message("Large dataset detected (", n_samples, " samples). Using batch processing with batch size ", batch_size)
      
      # Calculate number of batches
      n_batches <- ceiling(n_samples / batch_size)
      removeNotification(notification_id)
      notification_id <- showNotification(paste0("Processing batch ", batch_idx, "/", n_batches), 
                                          type = "message", duration = 0)
      message("Processing in ", n_batches, " batches")
      
      # Create directory for batch files
      batch_dir <- create_dir(file.path(array_qc_dir, "batches"))
      batch_paths <- c()
      
      # Step 1: Load and save each batch individually
      for (batch_idx in 1:n_batches) {
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, n_samples)
        
        removeNotification(notification_id)
        notification_id <- showNotification(paste0("Processing batch ", batch_idx, "/", n_batches), 
                                            type = "message", duration = 0)
        message("Processing batch ", batch_idx, "/", n_batches, " (samples ", start_idx, "-", end_idx, ")")
        
        # Load batch targets
        batch_targets <- targets[start_idx:end_idx, ]
        
        # Load RGSet for this batch only
        batch_rgSet <- minfi::read.metharray.exp(
          base = array_prep_dir, 
          targets = batch_targets, 
          verbose = FALSE, 
          force = TRUE, 
          extended = TRUE
        )
        message("  Batch RGSet size: ", format(object.size(batch_rgSet), units = "auto"))
        
        # Prepare metadata for this batch
        batch_targets$ID <- paste(batch_targets$Sample_Group, batch_targets$Sample_Name, sep = "_")
        minfi::sampleNames(batch_rgSet) <- batch_targets$ID
        
        # Convert to factors
        index <- seq_along(batch_rgSet@colData@listData)
        batch_rgSet@colData@listData[index] <- lapply(batch_rgSet@colData@listData[index], as.factor)
        
        # Save individual batch to disk
        batch_path <- file.path(batch_dir, paste0(array, "_batch_", batch_idx, ".rds"))
        message("  Saving batch to: ", batch_path)
        saveRDS(batch_rgSet, file = batch_path, compress = FALSE)
        batch_paths <- c(batch_paths, batch_path)
        
        # Clean up batch from memory
        rm(batch_rgSet, batch_targets)
      }
      gc()
      message("\nAll batches saved. Now merging ", length(batch_paths), " batches...")
      
      # Step 2: Merge all batches from disk
      # Load first batch as base
      message("Loading first batch as base...")
      rgSet <- readRDS(batch_paths[1])
      message("  Base RGSet size: ", format(object.size(rgSet), units = "auto"))
      
      # Load and merge remaining batches one by one
      if (length(batch_paths) > 1) {
        for (i in 2:length(batch_paths)) {
          
          removeNotification(notification_id)
          notification_id <- showNotification(paste0("Merging ", length(batch_paths), " batches..."), 
                                              type = "message", duration = 0)
          message("Merging batch ", i, "/", length(batch_paths), "...")
          
          # Load batch from disk
          batch_rgSet <- readRDS(batch_paths[i])
          message("  Batch size: ", format(object.size(batch_rgSet), units = "auto"))
          
          # Combine with main RGSet
          rgSet <- minfi::combineArrays(rgSet, batch_rgSet, 
                                        outType = ifelse(array == "EPIC", 
                                                         "IlluminaHumanMethylationEPIC",
                                                         "IlluminaHumanMethylation450k"))
          
          # Clean up batch and force garbage collection
          rm(batch_rgSet)
          message("  Combined RGSet size: ", format(object.size(rgSet), units = "auto"))
        }
      }
      gc()
      message("Final RGSet size: ", format(object.size(rgSet), units = "auto"))
      
      # Step 3: Calculate detection p-values on the merged RGSet
      removeNotification(notification_id)
      notification_id <- showNotification("Calculating detection p-values...", 
                                          type = "message", duration = 0)
      message("Calculating detection p-values on merged dataset...")
      detP <- minfi::detectionP(rgSet)
      message("detP size: ", format(object.size(detP), units = "auto"))
      
      # Step 4: Generate QC report
      removeNotification(notification_id)
      notification_id <- showNotification("Generating QC report...", 
                                          type = "message", duration = 0)
      message("Generating QC report...")
      minfi::qcReport(rgSet, 
                      sampNames = rgSet$Sample_Name, 
                      sampGroups = rgSet$Sample_Group,
                      pdf = file.path(array_qc_dir, paste0("2.0-QC_Report_", array, ".pdf")))
      
      # Step 5: Save final merged objects
      rgset_path <- file.path(array_qc_dir, paste0(array, "_rgSet.rds"))
      detp_path <- file.path(array_qc_dir, paste0(array, "_detP.rds"))
      
      message("Saving final RGSet to disk: ", rgset_path)
      saveRDS(rgSet, file = rgset_path, compress = FALSE)
      
      message("Saving final detP to disk: ", detp_path)
      saveRDS(detP, file = detp_path, compress = TRUE)
      
      # Step 6: Optionally delete batch files to save space
      message("Cleaning up batch files...")
      unlink(batch_dir, recursive = TRUE)
      
      qc_results$rgsets[[array]] <- rgset_path
      qc_results$detections[[array]] <- detp_path
      
      # Clean up
      rm(rgSet, detP)
      
    } else {
      # Original processing for small datasets (<= batch_size samples or EPIC_V2)
      removeNotification(notification_id)
      notification_id <- showNotification(paste0("Small dataset (", n_samples, " samples). Loading all at once..."), 
                       type = "message", duration = 0)
      message("Small dataset (", n_samples, " samples). Loading all at once...")
      
      rgSet <- minfi::read.metharray.exp(
        base = array_prep_dir, 
        targets = targets, 
        verbose = TRUE, 
        force = TRUE, 
        extended = TRUE
      )
      
      message("RGSet size: ", format(object.size(rgSet), units = "auto"))
      
      # Prepare metadata
      targets$ID <- paste(targets$Sample_Group, targets$Sample_Name, sep = "_")
      minfi::sampleNames(rgSet) <- targets$ID
      
      # Convert to factors
      index <- seq_along(rgSet@colData@listData)
      rgSet@colData@listData[index] <- lapply(rgSet@colData@listData[index], as.factor)
      
      # Calculate detection p-values
      removeNotification(notification_id)
      notification_id <- showNotification("Calculating detection p-values...", 
                                          type = "message", duration = 0)
      message("Calculating detection p-values...")
      detP <- minfi::detectionP(rgSet)
      message("detP size: ", format(object.size(detP), units = "auto"))
      
      # Generate QC report
      removeNotification(notification_id)
      notification_id <- showNotification("Generating QC report...", 
                                          type = "message", duration = 0)
      message("Generating QC report...")
      minfi::qcReport(rgSet, 
                      sampNames = rgSet$Sample_Name, 
                      sampGroups = rgSet$Sample_Group,
                      pdf = file.path(array_qc_dir, paste0("2.0-QC_Report_", array, ".pdf")))
      
      # Save to disk
      rgset_path <- file.path(array_qc_dir, paste0(array, "_rgSet.rds"))
      detp_path <- file.path(array_qc_dir, paste0(array, "_detP.rds"))
      
      message("Saving RGSet to disk: ", rgset_path)
      saveRDS(rgSet, file = rgset_path, compress = FALSE)
      
      message("Saving detP to disk: ", detp_path)
      saveRDS(detP, file = detp_path, compress = TRUE)
      
      qc_results$rgsets[[array]] <- rgset_path
      qc_results$detections[[array]] <- detp_path
      
      # Clean up
      rm(rgSet, detP)
    }
    
    removeNotification(notification_id)
    showNotification(paste0("Completed ", array), type = "message", duration = 2)
    message("Completed ", array, "\n")
  }
  gc()
  return(list(
    qc_results = qc_results,
    arrays_used = arrays_used
  ))
}


normalizeMeth <- function(rgSet, norm_method) {
  batch_size <- 50
  method <- tolower(norm_method)
  message("Normalizing using method: ", method)
  
  if (method == "ssnoob" && ncol(rgSet) > 50) {
    message("More than 50 samples detected. Running ssNoob normalization in batches of ", batch_size, "...")
    
    n_samples <- ncol(rgSet)
    batch_indices <- split(seq_len(n_samples), ceiling(seq_len(n_samples) / batch_size))
    
    mset_list <- lapply(seq_along(batch_indices), function(i) {
      idx <- batch_indices[[i]]
      message("  Processing batch ", i, "/", length(batch_indices),
              " (samples ", idx[1], "-", idx[length(idx)], ")")
      minfi::preprocessNoob(rgSet[, idx])
    })
    
    message("Combining ", length(mset_list), " batches using combineArrays...")
    combined <- do.call(minfi::combineArrays, mset_list)
    return(combined)
  }
  
  switch(method,
         ssnoob    = minfi::preprocessNoob(rgSet),
         raw       = minfi::preprocessRaw(rgSet),
         illumina  = minfi::preprocessIllumina(rgSet),
         quantile  = minfi::preprocessQuantile(rgSet),
         funnorm   = minfi::preprocessFunnorm(rgSet),
         stop("Unknown normalization method: ", method,
              "\nValid options are: ssnoob, raw, illumina, quantile, funnorm"))
}


# calculateBeta <- function(rgSet, mSetSq) {
#   methy_unorm <- minfi::getMeth(preprocessRaw(rgSet))
#   unmethy_unorm <- minfi::getUnmeth(preprocessRaw(rgSet))
#   beta_unorm <- methy_unorm / (methy_unorm + unmethy_unorm + 100)
#   
#   methy <- minfi::getMeth(mSetSq)
#   unmethy <- minfi::getUnmeth(mSetSq)
#   beta <- methy / (methy + unmethy + 100)
#   
#   list(
#     beta = beta,
#     beta_unorm = beta_unorm,
#     methy = methy,
#     unmethy = unmethy
#   )
# }

filterDetectionP <- function(rgSet, detP, mSetSq, threshold) {
  detP <- detP[match(minfi::featureNames(mSetSq), rownames(detP)), ]
  keep <- rowSums(detP < threshold) == ncol(rgSet)
  
  message("Detection p-value filter: ", threshold)
  message("Probes failing: ", sum(!keep), " | Probes kept: ", sum(keep))
  
  mSetSq[keep, ]
}


filterProbes <- function(mSetSq, filter_dir) {
  readFilt <- function(fname) read.table(file.path(filter_dir, fname), header = FALSE)[, 1]
  
  amb <- readFilt("amb_3965probes.vh20151030.txt")
  epic <- readFilt("epicV1B2_32260probes.vh20160325.txt")
  snp <- readFilt("snp_7998probes.vh20151030.txt")
  xy <- readFilt("xy_11551probes.vh20151030.txt")
  
  rs <- grep("rs", rownames(mSetSq), value = TRUE)
  ch <- grep("ch", rownames(mSetSq), value = TRUE)
  
  remove <- unique(c(amb, epic, snp, xy, rs, ch))
  keep <- !rownames(mSetSq) %in% remove
  
  message("Filtering probes: removed ", sum(!keep), ", kept ", sum(keep))
  mSetSq[keep, ]
}


adjustFFPE <- function(methy, unmethy, tissue_type) {
  unique_types <- unique(tissue_type)
  
  if (is.null(tissue_type)) {
    message("No FFPE/Frozen adjustment applied (no tissue type column provided in Sample Sheet")
    return(methy / (methy + unmethy + 100))
  }
  
  if (length(unique_types) == 1) {
    message("No FFPE/Frozen adjustment needed (only one tissue type: ", unique_types, ")")
    return(methy / (methy + unmethy + 100))
  }
  
  message("Adjusting by FFPE/Frozen tissue type...")
  batch <- ifelse(tissue_type == "FFPE", 2, 1)
  methy.ba <- 2^limma:removeBatchEffect(log2(methy + 1), batch)
  unmethy.ba <- 2^limma:removeBatchEffect(log2(unmethy + 1), batch)
  
  methy.ba / (methy.ba + unmethy.ba + 100)
}


finalizeBeta <- function(beta) {
  beta <- aggregate_to_probes(beta)
  beta <- DMRcate::rmSNPandCH(beta, dist = 2, mafcut = 0.05, rmXY = TRUE, rmcrosshyb = TRUE)
  beta
}


plotPostQC <- function(mSet, array_name, resultsDir) {
  grDevices::pdf(file.path(resultsDir, paste0("3- Post-filtered data QC for array_", array_name, ".pdf")),
                 width = 12, height = 7)
  qc <- minfi::getQC(mSet)
  minfi::plotQC(qc)
  grDevices::dev.off()
}

generate_beta_boxplot_static <- function(array, beta, out_dir) {
  # Calculate boxplot statistics
  message("  Calculating boxplot statistics...")
  
  # Convert beta matrix to long format for ggplot
  beta_df <- as.data.frame(beta)
  beta_long <- tidyr::pivot_longer(
    beta_df,
    cols = dplyr::everything(),
    names_to = "sample",
    values_to = "beta_value"
  )
  
  # Calculate summary statistics for each sample
  stats_df <- beta_long %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(
      q1 = quantile(beta_value, 0.25, na.rm = TRUE),
      median = quantile(beta_value, 0.5, na.rm = TRUE),
      q3 = quantile(beta_value, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower_whisker = max(q1 - 1.5 * iqr, min(beta_value, na.rm = TRUE)),
      upper_whisker = min(q3 + 1.5 * iqr, max(beta_value, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Create color palette based on sample groups if available
  n_samples <- nrow(stats_df)
  color_palette <- RColorBrewer::brewer.pal(min(n_samples, 12), "Set3")
  if (n_samples > 12) {
    color_palette <- rep(color_palette, length.out = n_samples)
  }
  
  # Create the plot
  p <- ggplot2::ggplot(stats_df, aes(x = reorder(sample, median), fill = sample)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        lower = q1,
        middle = median,
        upper = q3,
        ymin = lower_whisker,
        ymax = upper_whisker
      ),
      stat = "identity",
      alpha = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::labs(
      title = paste("Beta Value Distribution -", array),
      x = "Sample",
      y = "Beta Value"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_blank()
    )
  
  # Save as PNG
  png_path <- file.path(out_dir, paste0("beta_boxplot_", array, ".png"))
  ggplot2::ggsave(png_path, p, width = max(10, n_samples * 0.3), height = 6, dpi = 150)
  message("  Saved boxplot to: ", png_path)
  
  # Also save as PDF for better quality
  pdf_path <- file.path(out_dir, paste0("beta_boxplot_", array, ".pdf"))
  ggplot2::ggsave(pdf_path, p, width = max(10, n_samples * 0.3), height = 6)
}


generate_beta_matrix <- function(array, rgSet, detP, norm_method, threshold,
                                 filter_dir, beta_dir) {
  # Required for aggregate_to_probes()
  library(IlluminaHumanMethylationEPICv2anno.20a1.hg38)

  message("Processing array: ", array)
  array_beta_dir <- create_dir(file.path(beta_dir, array))

  ## --- 0. Exclude poor quality samples ---
  keep <- colMeans(detP) < threshold
  detP  <- detP[, keep]
  rgSet <- rgSet[, keep]
  rm(keep)

  ## ---- 1. Normalization ----
  notification_id <- showNotification(paste0("Normalizing ", array, " ..."), type = "message", duration = 0)
  mSetSq <- normalizeMeth(rgSet, norm_method)

  ## ---- 2. Raw & normalized beta/M values ----
  # maybe use DelayedArray?
  # beta_unf_path <- file.path(array_beta_dir, paste0("000_beta_unfiltered_", array, ".rds"))
  # beta_unf <- calculateBeta(rgSet, mSetSq)$beta
  # saveRDS(beta_unf, file = beta_unf_path, compress = FALSE)
  # rm(beta_unf)

  ## ---- 3. Detection p-value filtering ----
  removeNotification(notification_id)
  notification_id <- showNotification("Detection p-value filtering...", type = "message", duration = 0)
  mSetSq <- filterDetectionP(rgSet, detP, mSetSq, threshold)
  rm(rgSet, detP)

  ## ---- 4. Probe filtering ----
  removeNotification(notification_id)
  notification_id <- showNotification("Probe filtering...", type = "message", duration = 0)
  mSetSq_flt <- filterProbes(mSetSq, filter_dir)

  ## ---- 5. FFPE / Frozen adjustment ----
  removeNotification(notification_id)
  notification_id <- showNotification("FFPE/ Froxen adjustment...", type = "message", duration = 0)
  meth   <- minfi::getMeth(mSetSq_flt)
  unmeth <- minfi::getUnmeth(mSetSq_flt)
  beta   <- adjustFFPE(meth, unmeth, mSetSq_flt$Tissue_Type)
  rm(meth, unmeth, mSetSq_flt)

  ## ---- 6. Array-specific handling ----
  # if (array == "EPIC_V2") {
  #   message("EPIC_V2 detected → aggregating to probes")
  #   beta <- aggregate_to_probes(beta)
    # mVals_unf <- minfi::getM(mSetSq)
    # mVals_unf <- aggregate_to_probes(mVals_unf)
    # unfiltered_data <- mSetSq
    # filtered_dat <- mSetSq_flt
  # }
  # else {
  #   mVals_unf <- minfi::getM(mSetSq)
  # }
  # rm(mVals_unf)

  ## ---- 7. Final SNP / XY / cross-hyb filtering ----
  removeNotification(notification_id)
  notification_id <- showNotification("Final SNP/XY/cross-hyb filtering...", type = "message", duration = 0)
  beta <- finalizeBeta(beta)

  ## ---- 8. Beta QCplots ----
  # plotPostQC(mSetSq_flt, array, array_beta_dir)

  ## ---- 9. Beta Boxplots ---
  removeNotification(notification_id)
  notification_id <- showNotification("Generating Beta boxplots...", type = "message", duration = 0)
  generate_beta_boxplot_static(array, beta, out_dir = array_beta_dir)

  ## ---- 10. Save outputs ----
  beta_path <- file.path(array_beta_dir, paste0("001_beta_", array, ".rds"))
  mset_path <- file.path(array_beta_dir, paste0("002_unfilteredData_", array, ".rds"))
  # mset_flt_path <- file.path(array_beta_dir, paste0("003_filteredData_", array, ".rds"))

  removeNotification(notification_id)
  notification_id <- showNotification("Saving beta matrix...", type = "message", duration = 0)
  message("Saving beta to: ", beta_path)
  saveRDS(beta, file = beta_path, compress = FALSE)
  rm(beta)

  message("Saving mSetSq to: ", mset_path)
  saveRDS(mSetSq, file = mset_path, compress = FALSE)
  rm(mSetSq)
  
  # message("Saving mSetSq_flt to: ", mset_flt_path)
  # saveRDS(mSetSq_flt, file = mset_flt_path, compress = FALSE)
  
  removeNotification(notification_id)
  notification_id <- showNotification(paset0("Finished array: ", array), type = "message", duration = 0)
  message("Finished array: ", array)

  # Cleanup
  gc()

  return(list(
    beta_path = beta_path,
    mset_path = mset_path,
    array = array
  ))
}


merge_samplesheets <- function(arrays, preprocessing_dir, beta_merge_dir) {
  all_targets <- list()
  
  for (array in arrays) {
    array_path <- file.path(preprocessing_dir, array)
    targets <- minfi::read.metharray.sheet(array_path, pattern = "SampleSheet.csv")
    targets$ID <- paste(targets$Sample_Group, targets$Sample_Name, sep = "_")
    all_targets[[array]] <- targets
    
    # Save individual targets to disk
    target_path <- file.path(beta_merge_dir, paste0("targets_", array, ".rds"))
    saveRDS(targets, target_path)
  }
  
  # Get all unique columns
  all_cols <- unique(unlist(lapply(all_targets, colnames)))
  
  # Add missing columns as NA to each data frame
  all_targets_aligned <- lapply(all_targets, function(df) {
    for (col in all_cols) {
      if (!col %in% colnames(df)) {
        df[[col]] <- NA
      }
    }
    df[, all_cols]
  })
  
  # Merge
  targets_merged <- do.call(rbind, all_targets_aligned)
  
  # Save merged
  merged_path <- file.path(beta_merge_dir, "targets_merged.rds")
  saveRDS(targets_merged, merged_path)
  write.csv(targets_merged, file.path(beta_merge_dir, "targets_merged.csv"))
  
  cat("\nMerged SampleSheet completed at", Sys.time(), "\n")
  
  return(list(
    targets_merged = targets_merged
  ))
}


merge_matrix <- function(x, y) {
  m <- merge(x, y, by = "row.names")
  rownames(m) <- m[, 1]; m[, 1] <- NULL; m
}


merge_beta_matrix_from_disk <- function(beta_paths, beta_merge_dir) {
  message("Merging beta matrices from disk...")
  
  # Load first beta matrix
  message("Loading first beta matrix: ", basename(beta_paths[1]))
  beta_merged <- readRDS(beta_paths[1])
  message("  Size: ", format(object.size(beta_merged), units = "auto"))
  
  # Load and merge remaining matrices one by one
  if (length(beta_paths) > 1) {
    for (i in 2:length(beta_paths)) {
      message("Loading beta matrix ", i, "/", length(beta_paths), ": ", basename(beta_paths[i]))
      beta_next <- readRDS(beta_paths[i])
      message("  Size: ", format(object.size(beta_next), units = "auto"))
      
      # Merge current with next
      message("  Merging...")
      beta_merged <- merge(beta_merged, beta_next, by = "row.names")
      
      # Clean up
      rownames(beta_merged) <- beta_merged[, 1]
      beta_merged[, 1] <- NULL
      
      rm(beta_next)
      
      message("  Merged size: ", format(object.size(beta_merged), units = "auto"))
    }
  }
  
  # Save merged result
  message("Saving merged beta matrix...")
  saveRDS(beta_merged, file = file.path(beta_merge_dir, "beta_merged.rds"))
  write.csv(beta_merged, file = file.path(beta_merge_dir, "beta_merged.csv"))
  
  cat("\nMerged BetaMatrix completed at", Sys.time(), "\n")
  cat("Final size: ", format(object.size(beta_merged), units = "auto"), "\n")
  
  return(beta_merged)
}


extract_beta_and_targets <- function(input_dir, beta_dir, notification_id){
  
  # Get all CSV files recursively from input directory
  all_files <- list.files(path = input_dir, pattern = "\\.csv$", 
                          recursive = TRUE, full.names = TRUE)
  
  if (length(all_files) == 0) {
    stop("No CSV files found in the input folder")
  }
  
  # create merged directory inside beta_dir
  merged_dir <- file.path(beta_dir, "merged")
  if (!dir.exists(merged_dir)) {
    dir.create(merged_dir, recursive = TRUE)
  }
  
  beta_matrix_file <- NULL
  targets_file <- NULL
  
  # Check each CSV file to validate if it's a beta matrix or targets file
  for (file_path in all_files) {
    tryCatch({
      df <- read.csv(file_path, nrows = 5)  # Read first few rows for validation
      
      # Check if it's a beta matrix file
      # Typically has numeric columns (except possibly first column with probe names)
      if (is.null(beta_matrix_file)) {
        # Check if it looks like a beta matrix:
        # - Multiple columns (at least 2)
        # - Mostly numeric data
        # - Often has row names/probe IDs
        if (ncol(df) >= 2) {
          numeric_cols <- sapply(df, is.numeric)
          numeric_proportion <- sum(numeric_cols) / ncol(df)
          
          # If at least 80% of columns are numeric, likely a beta matrix
          if (numeric_proportion >= 0.8) {
            beta_matrix_file <- file_path
          }
        }
      }
      
      # Check if it's a targets file
      # Typically has metadata columns like Sample_Name, Basename, Sentrix_ID, etc.
      if (is.null(targets_file)) {
        col_names_lower <- tolower(colnames(df))
        
        # Look for typical targets file columns
        targets_markers <- c("sample_name", "basename", "sentrix", "chip", 
                             "position", "array", "slide")
        
        has_targets_markers <- any(targets_markers %in% col_names_lower)
        
        if (has_targets_markers) {
          targets_file <- file_path
        }
      }
      
    }, error = function(e) {
      # Skip files that can't be read
      NULL
    })
  }
  
  # Validate that both files were found
  if (is.null(beta_matrix_file)) {
    stop("Beta matrix file not found in input directory: ", input_dir)
  }
  
  if (is.null(targets_file)) {
    stop("Targets file not found in input directory: ", input_dir)
  }
  
  # Copy files to output directory
  file.copy(from = beta_matrix_file, 
            to = file.path(merged_dir, "beta_merged.csv"), overwrite = TRUE)
  
  file.copy(from = targets_file, 
            to = file.path(merged_dir, "targets_merged.csv"), overwrite = TRUE)
  
  message("Successfully copied files:")
  message("  Beta matrix: ", basename(beta_matrix_file))
  message("  Targets file: ", basename(targets_file))
  
  removeNotification(notification_id)
  notification_id <- showNotification("Loading Beta Matrix...", type="message", duration=0)
  
  beta <- read.csv(file.path(merged_dir, "beta_merged.csv"))
  # Set cpgs id as rownames
  first_col_values <- as.character(beta[, 1])
  rownames(beta) <- first_col_values
  beta <- beta[, -1, drop = FALSE]  # Remove the first column
  
  # Convert to matrix
  beta <- as.matrix(beta)
  
  removeNotification(notification_id)
  notification_id <- showNotification("Loading Sample Sheet...", type="message", duration=3)
  
  targets <- read.csv(file.path(merged_dir, "targets_merged.csv"))
  
  invisible(list(
    beta = beta,
    targets = targets
  ))
}