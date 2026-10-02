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

# Hub resources DMRcate::rmSNPandCH() and cpg.annotate() load: EPICv1/450K SNP,
# cross-hybridisation and XY lists, EPICv2 SNP list, and the EPICv2manifest.
AnnotationHub::cache(ExperimentHub::ExperimentHub()[c("EH3129", "EH3130", "EH3131", "EH8568")])
AnnotationHub::cache(AnnotationHub::AnnotationHub()["AH116484"])

# setup_cache only reads these two paths.
for (pkg in c(cfg$annotation_pkg, cfg$annotation_pkg_epicv2)) {
  library(pkg, character.only = TRUE)
  invisible(setup_cache(
    DIRS = list(cache = "/opt/met4all/cache", pathways = "/opt/met4all/build/pathways"),
    cfg  = modifyList(cfg, list(annotation_pkg = pkg))
  ))
}
