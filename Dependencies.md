# Dependencies

Full list of dependencies used in M4A. All packages are installed automatically when building the Docker images — no manual installation required.

---

## Base Images

| Image | Version |
|-------|---------|
| [rocker/rstudio](https://rocker-project.org/) | 4.5 |
| [rocker/shiny](https://rocker-project.org/) | 4.5 |
| R | 4.5 |
| Bioconductor | 3.22 |

---

## System Libraries (apt)

These are low-level C/C++ libraries required to compile certain R packages.

| Library | Purpose |
|---------|---------|
| libxml2-dev | XML parsing |
| libssl-dev | SSL/TLS support |
| libcurl4-openssl-dev | HTTP requests |
| libfontconfig1-dev | Font rendering |
| libharfbuzz-dev | Text shaping |
| libfribidi-dev | Unicode bidirectional text |
| libfreetype6-dev | Font engine |
| libpng-dev | PNG image support |
| libtiff5-dev | TIFF image support |
| libjpeg-dev | JPEG image support |
| libbz2-dev | bzip2 compression |
| liblzma-dev | LZMA/XZ compression |
| libz-dev | zlib compression |
| libpcre2-dev | Regex engine |
| libglpk40 / libglpk-dev | Linear programming (graph algorithms) |
| libhdf5-dev | HDF5 file format (large datasets) |
| git | Version control / GitHub installs |

---

## CRAN Packages

### Shiny & UI
| Package | Purpose |
|---------|---------|
| [shiny](https://shiny.posit.co/) | Web application framework |
| [shinyWidgets](https://dreamrs.github.io/shinyWidgets/) | Extended UI widgets |
| [shinyjs](https://deanattali.com/shinyjs/) | JavaScript actions in Shiny |
| [bslib](https://rstudio.github.io/bslib/) | Bootstrap-based UI theming |
| [DT](https://rstudio.github.io/DT/) | Interactive data tables |

### Data Manipulation
| Package | Purpose |
|---------|---------|
| [dplyr](https://dplyr.tidyverse.org/) | Data manipulation |
| [stringr](https://stringr.tidyverse.org/) | String operations |
| [tibble](https://tibble.tidyverse.org/) | Modern data frames |
| [foreach](https://cran.r-project.org/package=foreach) | Loop constructs |
| [doParallel](https://cran.r-project.org/package=doParallel) | Parallel processing backend |
| [future](https://future.futureverse.org/) | Async/parallel evaluation |

### File I/O
| Package | Purpose |
|---------|---------|
| [readxl](https://readxl.tidyverse.org/) | Read Excel files |
| [openxlsx](https://ycphs.github.io/openxlsx/) | Write Excel files |
| [vroom](https://vroom.r-lib.org/) | Fast delimited file reading |
| [archive](https://cran.r-project.org/package=archive) | Read/write archives (zip, tar, gz) |
| [qs2](https://cran.r-project.org/package=qs2) | Fast R object serialization |
| [fst](https://www.fstpackage.org/) | Fast data frame storage |
| [config](https://cran.r-project.org/package=config) | Configuration file management |

### Visualization
| Package | Purpose |
|---------|---------|
| [ggplot2](https://ggplot2.tidyverse.org/) | Grammar of graphics plotting |
| [plotly](https://plotly.com/r/) | Interactive plots |
| [ggokabeito](https://cran.r-project.org/package=ggokabeito) | Colorblind-friendly palette |
| [viridis](https://cran.r-project.org/package=viridis) | Perceptually uniform color scales |
| [RColorBrewer](https://cran.r-project.org/package=RColorBrewer) | Color palettes |
| [ggrepel](https://ggrepel.slowkow.com/) | Non-overlapping labels for ggplot2 |
| [gridExtra](https://cran.r-project.org/package=gridExtra) | Arrange multiple grid plots |
| [colorspace](https://colorspace.r-forge.r-project.org/) | Color space manipulation |
| [ggpubr](https://rpkgs.datanovia.com/ggpubr/) | Publication-ready ggplot2 figures |
| [svglite](https://svglite.r-lib.org/) | SVG graphics device |

### Statistics & Machine Learning
| Package | Purpose |
|---------|---------|
| [umap](https://cran.r-project.org/package=umap) | UMAP dimensionality reduction |
| [fgsea](https://bioconductor.org/packages/fgsea/) | Fast gene set enrichment analysis |
| [msigdbr](https://igordot.github.io/msigdbr/) | MSigDB gene sets in R |

---

## Bioconductor Packages

### Methylation Analysis (Core)
| Package | Purpose |
|---------|---------|
| [minfi](https://bioconductor.org/packages/minfi/) | Illumina methylation array analysis |
| [ChAMP](https://bioconductor.org/packages/ChAMP/) | Cell-type adjusted methylation pipeline |
| [sesame](https://bioconductor.org/packages/sesame/) | SeSAMe methylation array analysis |
| [sesameData](https://bioconductor.org/packages/sesameData/) | Reference data for SeSAMe |
| [DMRcate](https://bioconductor.org/packages/DMRcate/) | Differentially methylated region detection |
| [DMRcatedata](https://bioconductor.org/packages/DMRcatedata/) | Reference data for DMRcate |
| [methylclock](https://bioconductor.org/packages/methylclock/) | DNA methylation clocks (biological age) |
| [limma](https://bioconductor.org/packages/limma/) | Linear models for differential analysis |

### Array Manifests & Annotations
| Package | Purpose |
|---------|---------|
| IlluminaHumanMethylation450kmanifest | 450k array manifest |
| IlluminaHumanMethylationEPICmanifest | EPIC array manifest |
| IlluminaHumanMethylationEPICv2manifest | EPICv2 array manifest |
| IlluminaHumanMethylation450kanno.ilmn12.hg19 | 450k annotation (hg19) |
| IlluminaHumanMethylationEPICanno.ilm10b4.hg19 | EPIC annotation (hg19) |

### Copy Number Variation
| Package | Purpose |
|---------|---------|
| [conumee](https://bioconductor.org/packages/conumee/) | CNV analysis from methylation arrays |
| [conumee2](https://github.com/hovestadtlab/conumee2) | Extended CNV analysis (GitHub) |

### Clustering & Dimensionality
| Package | Purpose |
|---------|---------|
| [bluster](https://bioconductor.org/packages/bluster/) | Clustering algorithms for Bioconductor |
| [BiocNeighbors](https://bioconductor.org/packages/BiocNeighbors/) | Nearest-neighbor algorithms |
| [ConsensusClusterPlus](https://bioconductor.org/packages/ConsensusClusterPlus/) | Consensus clustering |
| [ComplexHeatmap](https://bioconductor.org/packages/ComplexHeatmap/) | Advanced heatmap visualization |

### Pathway & Gene Annotation
| Package | Purpose |
|---------|---------|
| [clusterProfiler](https://bioconductor.org/packages/clusterProfiler/) | Enrichment analysis & visualization |
| [AnnotationDbi](https://bioconductor.org/packages/AnnotationDbi/) | Annotation database interface |
| [org.Hs.eg.db](https://bioconductor.org/packages/org.Hs.eg.db/) | Human gene annotation |
| [GO.db](https://bioconductor.org/packages/GO.db/) | Gene Ontology database |

### Other Bioconductor
| Package | Purpose |
|---------|---------|
| [patchwork](https://patchwork.data-imaginist.com/) | Compose ggplot2 figures |
| [rtracklayer](https://bioconductor.org/packages/rtracklayer/) | Genomic track import/export |
| [matrixStats](https://cran.r-project.org/package=matrixStats) | Fast matrix operations |

---

## GitHub Packages

| Package | Source | Purpose |
|---------|--------|---------|
| conumee2 | [hovestadtlab/conumee2](https://github.com/hovestadtlab/conumee2) | Extended CNV analysis from methylation data |
