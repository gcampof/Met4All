# Use newer Shiny base image with R 4.5 (required for Bioconductor 3.21)
FROM rocker/shiny:4.5

# Install system dependencies for Bioconductor packages
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
RUN R -e 'pak::pkg_install(c("shinyWidgets", "shinyjs", "future", "DT", "dplyr", "ggplot2", "viridis", "RColorBrewer", "fgsea", "msigdbr"))'

# Install BiocManager first
RUN R -e 'install.packages("BiocManager", repos = "https://cloud.r-project.org")'

# Install Bioconductor packages via pak
RUN R -e 'pak::pkg_install(c("BiocManager", "minfi", "patchwork", "ggpubr", "bluster", "ComplexHeatmap", "ConsensusClusterPlus","IlluminaHumanMethylationEPICv2manifest", "IlluminaHumanMethylationEPICv2anno.20a1.hg38", "limma", "ChAMP","clusterProfiler", "sesame", "sesameData","AnnotationDbi","org.Hs.eg.db","GO.db","methylclock"))'

# Create and set working directory for Shiny app
WORKDIR /srv/shiny-server
RUN mkdir -p /srv/shiny-server

# Make directories writable for all users
RUN chmod -R a+rwX /usr/local/lib/R /srv /home /tmp

# Fix ownership for Shiny runtime
RUN chown -R shiny:shiny /srv/shiny-server

# Switch to Shiny user
USER shiny

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]


