
# Build annotation --------------------------------------------------------
#' @name methylation_buildannotation
#' @description Build annotation data.frame to summarise betas from probes 
#' to genes.
#' @param annot Character string with the data base used for annotation. Set
#' to \code{IlluminaHumanMethylationEPICanno.ilm10b4.hg19} by default
#' @value A data.frame with 6 columns: chr, pos, strand, probe, gene, group.
#' 
methylation_buildannot <- function(annot = "IlluminaHumanMethylationEPICanno.ilm10b4.hg19"){
  annotation <- as.data.frame(getAnnotation(annot))
  annotation[annotation == ""] <- NA
  annotation[annotation == " "] <- NA
  
  if (annot == "IlluminaHumanMethylationEPICanno.ilm10b2.hg19" | 
      annot == "IlluminaHumanMethylationEPICanno.ilm10b4.hg19"){
    aux <- annotation[, c(1:4, 22:24)]
    aux.long.grp <- strsplit(aux$UCSC_RefGene_Group, ";")
    aux.long.gene <- strsplit(aux$UCSC_RefGene_Name, ";")
    stopifnot(all(sapply(aux.long.gene, length) ==
                    sapply(aux.long.grp, length)))
    aux.len <- sapply(aux.long.grp, length)
    annotation <- data.frame(chr = rep(aux$chr, aux.len),
                             pos = rep(aux$pos, aux.len),
                             strand = rep(aux$strand, aux.len),
                             Name = rep(aux$Name, aux.len),
                             UCSC_Gene = unlist(aux.long.gene),
                             UCSC_Group = unlist(aux.long.grp),
                             stringsAsFactors = FALSE)
    annotation$Group <- gsub("5'UTR|1stExon|TSS200", "Promoter200", annotation$UCSC_Group)
    annotation$Group <- gsub("TSS1500", "Promoter1500", annotation$Group)
    annotation$Group <- gsub("ExonBnd", "Body", annotation$Group)
    annotation <- annotation[!duplicated(annotation), ]
    
    rm(aux, aux.long.gene, aux.long.grp, aux.len)
    annotation <- annotation[, -6]
    annotation <- annotation[! duplicated(annotation), ]
    annotation <- annotation[order(annotation$UCSC_Gene), ]
    return(annotation)
  } else {
    if (annot == "IlluminaHumanMethylation450kanno.ilmn12.hg19"){
      aux <- annotation[, c(1:4, 24:26)]
      aux.long.grp <- strsplit(aux$UCSC_RefGene_Group, ";")
      aux.long.gene <- strsplit(aux$UCSC_RefGene_Name, ";")
      stopifnot(all(sapply(aux.long.gene, length) ==
                      sapply(aux.long.grp, length)))
      aux.len <- sapply(aux.long.grp, length)
      annotation <- data.frame(chr = rep(aux$chr, aux.len),
                               pos = rep(aux$pos, aux.len),
                               strand = rep(aux$strand, aux.len),
                               Name = rep(aux$Name, aux.len),
                               UCSC_Gene = unlist(aux.long.gene),
                               UCSC_Group = unlist(aux.long.grp),
                               stringsAsFactors = FALSE)
      annotation$Group <- gsub("5'UTR|1stExon|TSS200", "Promoter200", annotation$UCSC_Group)
      annotation$Group <- gsub("TSS1500", "Promoter1500", annotation$Group)
      annotation$Group <- gsub("ExonBnd", "Body", annotation$Group)
      annotation <- annotation[! duplicated(annotation), ]
      
      rm(aux, aux.long.gene, aux.long.grp, aux.len)
      annotation <- annotation[, -6]
      annotation <- annotation[! duplicated(annotation), ]
      annotation <- annotation[order(annotation$UCSC_Gene), ]
      return(annotation)
    } else {
      stop("Not implemented yet, only works with IlluminaHumanMethylationEPICanno.ilm10b2/4.hg19 or IlluminaHumanMethylation450kanno.ilmn12.hg19")
    }
  }
}


# Summarise to gene -------------------------------------------------------
#' @name methylation_genemat
#' @description Summarise betas from probes to genes according to position 
#' from TSS.
#' @param beta_mat Beta matrix of probes.
#' @param annot Data frame as returned by function \code{methylation_buildannot}.
#' @param group Grouping factor: TSS200, TSS1500, Body, BodyUTR. TSS200 by 
#' default.
#' @param progressBar Boolean indicating whether or not to print a progress bar.
#' @param rm_mmap Boolean - Whether to remove or not multimapping probes 
#' (i.e. one probe links to more than one group/gene)
#' @value A matrix of beta values per gene and sample.
#' 
methylation_genemat_dt <- function(beta.matrix, annotation, group = "TSS200",
                                   rm_mmap = FALSE){
  library(data.table)
  
  group <- match.arg(group, choices = c("TSS200", "TSS1500", "Body", "BodyUTR"))
  
  # Define grouping factors
  groupingfactor <- switch(group,
                           TSS200 = "Promoter200",
                           TSS1500 = c("Promoter200", "Promoter1500"),
                           Body = "Body",
                           BodyUTR = c("Body", "3'UTR"))
  
  # Convert to data.table
  annot_dt <- as.data.table(annotation)
  annot_dt <- annot_dt[Group %in% groupingfactor]
  print(head(annot_dt))
  
  # Remove multimapping probes
  if (rm_mmap){
    annot_dt[, n_groups := uniqueN(Group), by = Name]
    annot_dt <- annot_dt[n_groups == 1]
    annot_dt[, n_groups := NULL]
  }
  
  # Get unique probes per gene
  probe_gene_map <- unique(annot_dt[, .(Name, UCSC_Gene)])
  
  # Convert beta matrix to data.table format
  beta_dt <- as.data.table(beta.matrix, keep.rownames = "Probe")
  setkey(beta_dt, Probe)
  setkey(probe_gene_map, Name)
  
  # Use data.table's merge syntax
  merged_dt <- probe_gene_map[beta_dt, on = c(Name = "Probe"), allow.cartesian = TRUE]
  setnames(merged_dt, "Name", "Probe")
  
  # Melt to long format for efficient median calculation
  melted_dt <- data.table::melt(merged_dt, 
                                id.vars = c("UCSC_Gene", "Probe"),
                                variable.name = "Sample",
                                value.name = "Beta")
  melted_dt <- as.data.table(melted_dt)
  
  # Calculate median per gene and sample
  result_dt <- melted_dt[, .(Beta = median(Beta, na.rm = TRUE)), 
                         by = .(UCSC_Gene, Sample)]
  
  # Cast back to matrix format
  mat <- dcast(result_dt, UCSC_Gene ~ Sample, value.var = "Beta")
  
  # Convert to matrix
  mat_matrix <- as.matrix(mat[, -1])
  rownames(mat_matrix) <- mat$UCSC_Gene
  
  # Remove rows with all NA
  mat_clean <- mat_matrix[rowSums(is.na(mat_matrix)) != ncol(mat_matrix), ]
  
  message(sprintf("Finished gene annotation: %d genes", nrow(mat_clean)))
  return(mat_clean)
}