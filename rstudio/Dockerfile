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
RUN R -e 'pak::pkg_install(c("shiny", "shinyWidgets", "shinyjs", "bslib", "future", "mirai", "openxlsx","DT", "dplyr", "ggplot2", "ggokabeito", "viridis", "RColorBrewer", "fgsea", "msigdbr", "plotly", "doParallel", "readxl", "vroom", "archive", "foreach", "stringr", "tibble", "umap", "uwot", "svglite", "config", "here", "qs2", "fst", "ggrepel", "gridExtra", "colorspace", "ggpubr", "rtracklayer"))'

# Install BiocManager first
RUN R -e 'install.packages("BiocManager", repos = "https://cloud.r-project.org")'

# Bioconductor version 3.22
RUN R -e 'BiocManager::install(version = "3.22", ask = FALSE)'

# Install Bioconductor packages via pak
RUN R -e 'pak::pkg_install(c("minfi", "patchwork", "bluster", "BiocNeighbors", "ComplexHeatmap", "ConsensusClusterPlus", "conumee", "IlluminaHumanMethylationEPICv2manifest", "limma", "ChAMP", "clusterProfiler", "sesame", "sesameData", "AnnotationDbi", "org.Hs.eg.db", "GO.db", "methylclock", "DMRcate", "DMRcatedata", "IlluminaHumanMethylation450kmanifest", "IlluminaHumanMethylationEPICmanifest", "IlluminaHumanMethylation450kanno.ilmn12.hg19", "IlluminaHumanMethylationEPICanno.ilm10b4.hg19", "matrixStats"))'

# Install conumee2 from GitHub using devtools
RUN R -e 'install.packages(c("remotes", "devtools"), repos = "https://cloud.r-project.org")'
RUN R -e 'devtools::install_github("hovestadtlab/conumee2", subdir = "conumee2", dependencies = TRUE)'

# Create ExperimentHub cache dir and fix permissions
RUN mkdir -p /home/rstudio/.cache/R/ExperimentHub && \
    chown -R rstudio:rstudio /home/rstudio/.cache

# Same runtime settings as the shiny image, so anything reproduced here behaves
# the way it does in production.
#   M4A_MAX_JOBS        concurrent analyses (worker pool size)
#   M4A_THREADS_PER_JOB threads inside each worker
ENV OMP_NUM_THREADS=1 \
    OPENBLAS_NUM_THREADS=1 \
    M4A_MAX_JOBS=2 \
    M4A_THREADS_PER_JOB=2

# The annotation cache is not baked into this image (its build context is
# ./rstudio and it has no copy of the app). Point it at the mounted project
# instead: the first setup_cache() call builds it there and every later session
# reuses it, and it survives container restarts because it lives on the host.
ENV M4A_CACHE_DIR=/home/rstudio/project/shiny/app/cache

# Default workdir inside container
WORKDIR /home/rstudio/project
