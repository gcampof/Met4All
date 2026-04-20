# Methylation For All (M4A-shiny)

**M4A** is a web-based application for DNA methylation analysis, no coding required. It runs entirely inside [Docker](https://docs.docker.com/get-docker/), so you don't need to install R, Bioconductor, or any dependencies manually. Just launch it and open your browser.

---

## What M4A Can Do

M4A accepts two types of input:

- **Raw IDAT files**: the direct output from Illumina 450k, EPIC, or EPICv2 arrays
- **A pre-computed beta matrix**: a table of methylation values (rows = CpG sites, columns = samples)

When you provide IDATs, M4A will automatically preprocess, normalize, and filter your data before analysis. From the resulting beta matrix, the application gives you access to:

| Analysis | Available from |
|---|---|
| Beta matrix distribution | IDATs only |
| Quality control (QC) plots | IDATs only |
| Copy number variation (CNV) | IDATs only |
| MDS plot | IDATs or beta matrix |
| PCA | IDATs or beta matrix |
| UMAP | IDATs or beta matrix |
| Heatmap | IDATs or beta matrix |
| Global methylation | IDATs or beta matrix |
| Differential methylation | IDATs or beta matrix |

For every analysis, you can customize both the **analytical parameters** and the **visual aesthetics** - colors, labels, font sizes, and more. Results can be exported with a single click.

---

## Test Dataset

We provide two options to get you up and running with real data right away and getting familiarized with the application.

### Option 1 - Dataset already ready to use

We provide a pre-downloaded dataset from [GSE267015](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE267015) (n = 70 samples, EPIC + 450k arrays), from the retinoblastoma study published in [PMID 39079981](https://pubmed.ncbi.nlm.nih.gov/39079981/). You can download it directly from this repository's [Releases](../../releases) page.

### Option 2 - Download any GEO dataset yourself

We provide a ready-to-use shell script (`download_geo.sh`) that downloads any public GEO dataset given its accession ID. Usage:

```bash
bash download_geo.sh GSE267015
```

The script will fetch the IDATs and sample sheet automatically.

---

## Requirements

Before you begin, make sure you have:

- [Docker](https://docs.docker.com/get-docker/) (v28.0 or later)
- [Docker Compose](https://docs.docker.com/compose/install/) (v2.39 or later)
- At least **24 GB of RAM** available
- At least **30 GB of free disk space**
---

## Installation & Launch

### Step 1 - Clone the repository

Open a terminal and run:

```bash
git clone https://github.com/gcampof/M4A-shiny.git
cd M4A-shiny
```

### Step 2 - Prepare data directories

```bash
mkdir -p ./shiny/logs ./shiny/app/data ./shiny/app/cache
sudo chown -R 999:999 ./shiny/logs
chmod 777 ./shiny/logs ./shiny/app/data ./shiny/app/cache
```

> These directories are where the app writes logs and analysis results. The `sudo chown` command gives the internal app user the right permissions.

### Step 3 - Start M4A

Pull the pre-built image from DockerHub and start the app:

```bash
docker compose -f docker-compose.prod.yml up -d
```

The first time you run this, Docker will download the image (~25 GB). This only happens once.

### Step 4 - Open the app

Once the container is running, open your browser and go to:

**http://localhost:3838**

The M4A interface will load and you're ready to start your analysis.

---

## Updating to the Latest Version

To pull the most recent version of M4A:

```bash
docker compose -f docker-compose.prod.yml pull shiny
docker compose -f docker-compose.prod.yml up -d
```

---

## Stopping M4A

```bash
docker compose -f docker-compose.prod.yml down
```

---

## Accessing Logs

If something doesn't look right, logs are written to `./shiny/logs/` on your machine.

```bash
# List available log files
ls ./shiny/logs/

# Read the latest log
cat ./shiny/logs/<logfile>.log

# Or check the container logs directly
sudo docker logs m4a-shiny
```

---

## Repository Structure

```
.
├── docker-compose.yml
├── docker-compose.prod.yml
├── download_geo.sh  
├── rstudio/
│   └── Dockerfile
└── shiny/
    ├── Dockerfile
    ├── shiny-server.conf
    └── app/
        └── app.R
```

---

## Notes

- Analysis results are saved to `./shiny/app/data/` on your machine and persist between sessions. Folders older than 24 hours are cleaned up automatically on next launch.
- Changes to `app.R` are picked up without rebuilding the image - just restart the service with `docker compose restart shiny`.
- M4A is configured with `restart: unless-stopped`, so it will automatically start again after a system reboot as long as Docker is running.

---

---

## For Developers

The section below is intended for users who want to modify or extend M4A.

### Building from Source

To build both the Shiny and RStudio images locally (first build takes ~20–40 min):

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

### Accessing the Services

| Service | URL | Credentials |
|---|---|---|
| Shiny app | http://localhost:3838 | - |
| RStudio | http://localhost:3939 | user: `rstudio` / password: `rstudio` |

### Running Individual Services

```bash
# Shiny only
docker compose up -d shiny

# RStudio only
docker compose up -d rstudio
```

### Development Notes

- The RStudio service mounts your local code at `/home/rstudio/project/shiny` inside the container, so you can edit files directly in RStudio and see changes in real time.
- To use the production image for Shiny but run RStudio locally, mix flags: `docker compose -f docker-compose.prod.yml up -d shiny` and `docker compose up -d rstudio`.

### Dependencies

Built on [Rocker](https://rocker-project.org/) base images:

- `rocker/rstudio:4.5`
- `rocker/shiny:4.5`
- Bioconductor 3.22
- R 4.5
