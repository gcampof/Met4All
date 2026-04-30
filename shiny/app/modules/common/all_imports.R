# Core
library(minfi)
library(ChAMP)
library(limma)

# Methylation annotations
library(IlluminaHumanMethylation450kmanifest)
library(IlluminaHumanMethylationEPICmanifest)
library(IlluminaHumanMethylationEPICv2manifest)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

# CNV
library(conumee2)
library(rtracklayer)

# Clustering & dimension reduction
library(bluster)
library(BiocNeighbors)
library(ConsensusClusterPlus)
library(umap)
library(uwot)
library(igraph)
library(ComplexHeatmap)

# Annotation & enrichment
library(AnnotationDbi)
library(GO.db)
library(org.Hs.eg.db)
library(fgsea)