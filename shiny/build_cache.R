# Build-time only: populate the annotation cache baked into the image.
# Calls the same setup_cache() the app uses, so build and runtime cannot drift.
suppressPackageStartupMessages({
  library(config)
  library(minfi)
  library(AnnotationDbi)
  library(org.Hs.eg.db)
  library(GO.db)
  library(fgsea)
})

source("/opt/met4all/build/annotations.R")  # methylation_buildannot()
source("/opt/met4all/build/utils.R")        # setup_cache(), get_go_bp_gene_sets()

cfg <- config::get(file = "/opt/met4all/build/config.yml")
library(cfg$annotation_pkg, character.only = TRUE)

# setup_cache only reads these two paths.
invisible(setup_cache(
  DIRS = list(cache = "/opt/met4all/cache", pathways = "/opt/met4all/build/pathways"),
  cfg  = cfg
))
