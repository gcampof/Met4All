# M4A — DNA Methylation Analysis

M4A is a containerized platform for DNA methylation analysis, built on R/Bioconductor. It supports array-based methylation data (450k, EPIC and EPICv2) and provides tools for quality control, normalization, differential methylation analysis, copy number variation, pathway enrichment, and more.

It ships as two independent services:

- **RStudio** — interactive R environment for exploratory analysis and script development
- **Shiny** — web application for running the M4A analysis pipeline through a GUI

---

## Requirements

- [Docker](https://docs.docker.com/get-docker/) (v28.0 or later)
- [Docker Compose](https://docs.docker.com/compose/install/) (v2.39 or later)
- At least **16 GB RAM** recommended (Bioconductor packages are memory-intensive)
- At least **25 GB free disk space** for the Docker images

---

## Repository Structure

```
.
├── docker-compose.yml
├── rstudio/
│   └── Dockerfile
└── shiny/
    ├── Dockerfile
    └── app/
        └── app.R
```

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/gcampof/M4A-shiny.git
cd m4a
```

### 2. Build the images

The first build will take a while (~20–40 min) as it installs all R and Bioconductor packages.

```bash
docker compose build
```

### 3. Run the service you need

**Shiny app** (analysis GUI, accessible at http://localhost:3838):
```bash
docker compose up shiny
```

**RStudio** (interactive R environment, accessible at http://localhost:3939):
```bash
docker compose up rstudio
```

**Both at once:**
```bash
docker compose up
```

To run in the background, add the `-d` flag:
```bash
docker compose up -d shiny
```

---

## Accessing the Services

| Service | URL | Credentials |
|---------|-----|-------------|
| Shiny app | http://localhost:3838 | — |
| RStudio | http://localhost:3939 | user: `rstudio` / password: `rstudio` |

---

## Stopping the Services

```bash
docker compose down
```

---

## Notes

- The Shiny app mounts `./shiny/app` into the container at runtime, so changes to `app.R` are reflected without rebuilding the image — just restart the service with `docker compose restart shiny`
- The RStudio service mounts your local code directory at `/home/rstudio/project/code` inside the container
- Both services use `restart: unless-stopped`, meaning they will automatically restart on system reboot as long as Docker is running

---

## Dependencies

Built on [Rocker](https://rocker-project.org/) base images:

- `rocker/rstudio:4.5`
- `rocker/shiny:4.5`
- Bioconductor 3.22
- R 4.5
