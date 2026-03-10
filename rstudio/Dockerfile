FROM rocker/rstudio:4.5

# System dependencies for Bioconductor/CRAN packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libbz2-dev \
    liblzma-dev \
    libz-dev \
    libpcre2-dev \
    libglpk40 \
    libglpk-dev \
    libhdf5-dev \
    git \
    && rm -rf /var/lib/apt/lists/*



# Install CRAN packages via pak for speed
RUN R -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")'
RUN R -e 'pak::pkg_install(c("shiny", "shinyWidgets", "shinyjs", "bslib", "future", "DT", "dplyr", "ggplot2", "ggokabeito", "viridis", "RColorBrewer", "fgsea", "msigdbr", "plotly", "doParallel", "readxl", "vroom", "archive", "foreach", "doParallel", "stringr", "tibble", "umap", "svglite"))'

# Install BiocManager first
RUN R -e 'install.packages("BiocManager", repos = "https://cloud.r-project.org")'

# Bioconductor version 3.21
RUN R -e 'BiocManager::install(version = "3.21", ask = FALSE)'

# Install Bioconductor packages via pak
RUN R -e 'pak::pkg_install(c("BiocManager", "minfi", "patchwork", "ggpubr", "bluster", "ComplexHeatmap", "ConsensusClusterPlus", "IlluminaHumanMethylationEPICv2manifest", "IlluminaHumanMethylationEPICv2anno.20a1.hg38", "limma", "ChAMP","clusterProfiler", "sesame", "sesameData","AnnotationDbi","org.Hs.eg.db","GO.db","methylclock", "DMRcate", "DMRcatedata"))'

# Create ExperimentHub cache dir and fix permissions
RUN mkdir -p /home/rstudio/.cache/R/ExperimentHub && \
    chown -R rstudio:rstudio /home/rstudio/.cache

# Default workdir inside container
WORKDIR /home/rstudio/project
