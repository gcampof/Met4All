FROM rocker/shiny:4.5

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
RUN R -e 'pak::pkg_install(c("shiny", "shinyWidgets", "shinyjs", "bslib", "future", "openxlsx","DT", "dplyr", "ggplot2", "ggokabeito", "viridis", "RColorBrewer", "fgsea", "msigdbr", "plotly", "doParallel", "readxl", "vroom", "archive", "foreach", "stringr", "tibble", "umap", "uwot", "svglite", "config", "qs2", "fst", "ggrepel", "gridExtra", "colorspace", "ggpubr", "rtracklayer"))'

# Install BiocManager first
RUN R -e 'install.packages("BiocManager", repos = "https://cloud.r-project.org")'

# Bioconductor version 3.22
RUN R -e 'BiocManager::install(version = "3.22", ask = FALSE)'

# Install Bioconductor packages via pak
RUN R -e 'pak::pkg_install(c("minfi", "patchwork", "bluster", "BiocNeighbors", "ComplexHeatmap", "ConsensusClusterPlus", "conumee", "IlluminaHumanMethylationEPICv2manifest", "limma", "ChAMP", "clusterProfiler", "sesame", "sesameData", "AnnotationDbi", "org.Hs.eg.db", "GO.db", "methylclock", "DMRcate", "DMRcatedata", "IlluminaHumanMethylation450kmanifest", "IlluminaHumanMethylationEPICmanifest", "IlluminaHumanMethylation450kanno.ilmn12.hg19", "IlluminaHumanMethylationEPICanno.ilm10b4.hg19", "matrixStats"))'

# Install conumee2 from GitHub using devtools
RUN R -e 'install.packages(c("remotes", "devtools"), repos = "https://cloud.r-project.org")'
RUN R -e 'devtools::install_github("hovestadtlab/conumee2", subdir = "conumee2", dependencies = TRUE)'

# Create ExperimentHub cache dir owned by shiny user
RUN mkdir -p /home/shiny/.cache/R/ExperimentHub && \
    chown -R shiny:shiny /home/shiny/.cache

# Create runtime directionres with correct ownership
RUN mkdir -p /srv/shiny-server/data /srv/shiny-server/cache && \
    chown -R shiny:shiny /srv/shiny-server

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY app/ /srv/shiny-server/

WORKDIR /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
