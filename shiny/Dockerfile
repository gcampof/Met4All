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

# Create ExperimentHub cache dir owned by shiny user
RUN mkdir -p /home/shiny/.cache/R/ExperimentHub && \
    chown -R shiny:shiny /home/shiny/.cache

# Bake the annotation cache outside every bind mount.
# Narrow COPY so this layer is not invalidated by ordinary app edits.
COPY app/config.yml                              /opt/met4all/build/config.yml
COPY app/modules/common/utils.R                  /opt/met4all/build/utils.R
COPY app/modules/primary_analysis/annotations.R  /opt/met4all/build/annotations.R
COPY app/common_files/pathways/                  /opt/met4all/build/pathways/
COPY build_cache.R                               /opt/met4all/build/build_cache.R

RUN mkdir -p /opt/met4all/cache && \
    Rscript /opt/met4all/build/build_cache.R && \
    chown -R shiny:shiny /opt/met4all && \
    ls -lh /opt/met4all/cache

ENV M4A_CACHE_DIR=/opt/met4all/cache

# Concurrency limits.
#   M4A_MAX_JOBS        concurrent heavy analyses (worker pool size)
#   M4A_THREADS_PER_JOB threads inside each worker
#
# OMP_NUM_THREADS/OPENBLAS_NUM_THREADS are deliberately NOT set here: pinning
# them caps data.table for the life of the process (setDTthreads cannot raise a
# value above the OpenMP limit read at startup). app.R sets them before the
# worker pool spawns, so each daemon inherits the right value.
#
# Keep M4A_MAX_JOBS * M4A_THREADS_PER_JOB <= host cores.
# Memory scales with cohort size, not just job count: one 252-sample EPIC
# ingest peaks at ~16 GB. Set a mem_limit in compose so an overrun kills the
# container instead of the host.
ENV M4A_MAX_JOBS=2 \
    M4A_THREADS_PER_JOB=4

# Create runtime directionres with correct ownership
RUN mkdir -p /srv/shiny-server/data /srv/shiny-server/cache && \
    chown -R shiny:shiny /srv/shiny-server

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY app/ /srv/shiny-server/

WORKDIR /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
