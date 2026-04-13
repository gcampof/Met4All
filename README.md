# Methylation For All (M4A) — DNA Methylation Analysis Platform

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
├── docker-compose.prod.yml
├── rstudio/
│   └── Dockerfile
└── shiny/
    ├── Dockerfile
    ├── shiny-server.conf
    └── app/
        └── app.R
```

---

## Getting Started

### Before you start — prepare required directories

After cloning, create the directories the app needs to write to and set the correct permissions:

```bash
git clone https://github.com/gcampof/M4A-shiny.git
cd m4a

mkdir -p ./shiny/logs ./shiny/app/data ./shiny/app/cache
sudo chown -R 999:999 ./shiny/logs
chmod 777 ./shiny/logs ./shiny/app/data ./shiny/app/cache
```

> The `shiny` user inside the container runs as UID 999. These directories need to be writable by that user.

---

## Option A — Production (recommended for most users)

Pulls the pre-built Shiny image from DockerHub. No build required for the Shiny service.

```bash
docker compose -f docker-compose.prod.yml up -d
```

To stop:
```bash
docker compose -f docker-compose.prod.yml down
```

To update the Shiny image to the latest version:
```bash
docker compose -f docker-compose.prod.yml pull shiny
docker compose -f docker-compose.prod.yml up -d
```

---

## Option B — Development (build everything locally)

Builds both images from source. The first build will take a while (~20–40 min) as it installs all R and Bioconductor packages.

```bash
docker compose build
docker compose up -d
```

To rebuild only the Shiny service after changes:
```bash
docker compose build shiny && docker compose up -d shiny
```

To stop:
```bash
docker compose down
```

---

## Running individual services

Both options support running services independently:

```bash
# Shiny app only
docker compose [-f docker-compose.prod.yml] up -d shiny

# RStudio only
docker compose [-f docker-compose.prod.yml] up -d rstudio
```

---

## Accessing the Services

| Service | URL | Credentials |
|---------|-----|-------------|
| Shiny app | http://localhost:3838 | — |
| RStudio | http://localhost:3939 | user: `rstudio` / password: `rstudio` |

---

## Logs

Shiny server logs are written to `./shiny/logs/` on your host machine and are readable directly:

```bash
# Latest log
ls ./shiny/logs/
cat ./shiny/logs/<logfile>.log

# Or via Docker
sudo docker logs m4a-shiny
```

---

## Stopping the Services

```bash
docker compose down
# or for prod:
docker compose -f docker-compose.prod.yml down
```

---

## Notes

- The Shiny app mounts `./shiny/app` into the container at runtime, so changes to `app.R` are reflected without rebuilding — just restart with `docker compose restart shiny`
- The RStudio service mounts your local code at `/home/rstudio/project/shiny` inside the container
- Both services use `restart: unless-stopped` and will auto-restart on system reboot as long as Docker is running
- Analysis data is written to `./shiny/app/data/` and persists between sessions. Directories older than 24 hours are cleaned up automatically on next launch.

---

## Dependencies

Built on [Rocker](https://rocker-project.org/) base images:
- `rocker/rstudio:4.5`
- `rocker/shiny:4.5`
- Bioconductor 3.22
- R 4.5
