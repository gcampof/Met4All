# Met4All

**Met4All** is a web-based application for DNA methylation analysis, no coding required. It runs entirely inside [Docker](https://docs.docker.com/get-docker/), so you don't need to install R, Bioconductor, or any dependencies manually. 

A pre-built Docker image is available on Docker Hub: [gcampof/methylation4all-shiny](https://hub.docker.com/r/gcampof/methylation4all-shiny), allowing you to get started quickly without building from source.

Just launch it and open your browser.

---

## What Met4All Can Do

v accepts two types of input:

- **Raw IDAT files**: the direct output from Illumina 450k, EPIC, or EPICv2 arrays
- **A pre-computed beta matrix**: a table of methylation values (rows = CpG sites, columns = samples)

When you provide IDATs, Met4All will automatically preprocess, normalize, and filter your data before analysis. From the resulting beta matrix, the application gives you access to:

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

We provide a test dataset from [GSE267015](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE267015), the retinoblastoma study published in [PMID 39079981](https://pubmed.ncbi.nlm.nih.gov/39079981/). It can be run from IDATs (68 samples, EPIC + 450K arrays) or from the authors' pre-computed beta matrix (61 tumours). Download links and step-by-step instructions are in [TEST/readme.md](TEST/readme.md).

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
git clone https://github.com/gcampof/Met4All.git
cd Met4All
```

### Step 2 - Prepare data directories

```bash
mkdir -p ./shiny/logs ./shiny/app/data
chmod 777 ./shiny/logs ./shiny/app/data
```

> These directories are where the app writes logs, user uploads, and analysis results. The app code itself is bundled inside the Docker image.

### Step 3 - Start Met4All

Pull the pre-built image from DockerHub and start the app:

```bash
docker compose -f docker-compose.prod.yml up -d
```

The first time you run this, Docker will download the image (~25 GB). This only happens once.

### Step 4 - Open the app

Once the container is running, open your browser and go to:

**http://localhost:3838**

The Met4All interface will load and you're ready to start your analysis.

---

## Updating to a New Version

To update Met4All, edit `docker-compose.prod.yml` and change the image tag to the new version, then pull and restart:

```bash
docker compose -f docker-compose.prod.yml pull shiny
docker compose -f docker-compose.prod.yml up -d
```

Your data in `./shiny/app/data/` is not affected by updates.

---

## Stopping Met4All

```bash
docker compose -f docker-compose.prod.yml down
```

---

## Sharing Met4All Between Users

Met4All is meant to be left running and shared. Several people can use the same
deployment at once.

What it provides out of the box:

- **Run several analyses at the same time.** Launch a heatmap, then start a
  differential analysis without waiting for it, or work on a second dataset in
  another tab. They run alongside each other, and every open interface stays
  responsive while they do.
- **Close the tab and come back whenever you want.** The address in your browser
  bar identifies your analysis, so you can reopen it later from any machine and
  carry on. After 60 minutes with nobody connected the app releases the session
  from memory, but nothing is lost: reopening the address rebuilds it from disk.
  Results live in `./shiny/app/data/analysis_<date>_<time>_<id>` and are never
  deleted automatically.
- **Requests queue** If more analyses are submitted than
  `M4A_MAX_JOBS` allows, the extras wait and start automatically as earlier ones
  finish, and each user is told their position in the queue.
- **A downloadable log** of everything an analysis printed, for when a result
  looks wrong and you want to see what happened.

### One instance or several

**Start with `docker-compose.prod.yml`.** A single instance already runs several
analyses in parallel and keeps every open interface responsive, in one container
with nothing else to administer. This is the right choice on a workstation and
for most laboratory servers.

To run **more analyses at the same time**, raise `M4A_MAX_JOBS` in the
`environment:` block of your compose file (see [Tuning](#tuning)). Editing it
there takes effect on the next `docker compose up`, with no image rebuild. Bear
in mind that methylation analyses are memory intensive: budget the peak memory
for your largest cohort per simultaneous analysis, and leave the machine some
headroom.

Move to `docker-compose.scale.yml` when you want something a single instance
cannot provide:

- **Failure isolation.** With one instance, a crash disconnects everyone. With
  several, only the users on that instance are affected and the rest carry on.
- **Many people using the interface at once.** Drawing heatmaps, rendering CNV
  plots and preparing downloads all happen in each instance's interface process.
  With a dozen active users that becomes the bottleneck, and more instances
  spread the load.

```bash
docker compose -f docker-compose.scale.yml up -d --scale shiny=4
```

`--scale shiny=N` sets how many instances to start. They share one address
(**http://localhost:3838**, or set `M4A_PORT`), and each user is kept on the
instance that served them automatically. Both options run on a single machine, so
several instances divide the same memory rather than adding any: this is about
resilience and interface throughput, not extra capacity.

Because the image holds no state that other instances need, sites already running
Docker Swarm or Kubernetes can deploy it on their own platform unchanged.

### Resource requirements

Memory and disk scale with **dataset size**. Measured end to end on the [Test dataset](#test-dataset)  (68 samples:
59 EPIC and 9 450K):

| | |
|---|---|
| Peak memory, one analysis | ~5 GB |
| App itself, idle | ~250 MB |
| Each ready worker | ~1.5 GB |
| Disk, complete analysis | ~3.9 GB |

As a rough guide, allow **60 MB of working space per sample**.

If an analysis does run short of memory it stops with a clear message rather than
being killed, and the rest of the app keeps working.

### Tuning

| Variable | Default | Meaning |
|---|---|---|
| `M4A_MAX_JOBS` | 2 | Analyses running at the same time. Extra ones queue. |
| `M4A_THREADS_PER_JOB` | 4 | Threads inside each analysis. Keep `MAX_JOBS x THREADS_PER_JOB` within the machine's core count. |
| `M4A_MEM_LIMIT` | 24g | Memory ceiling for the container. |
| `M4A_MIN_FREE_GB` | 15 | Refuse to start if free disk is below this. |

Set them in the `environment:` block of your compose file, for example:

```yaml
    environment:
      - R_CONFIG_ACTIVE=default
      - M4A_MAX_JOBS=4
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
docker logs m4a-shiny
```

---

## Repository Structure

```
.
├── docker-compose.dev.yml
├── docker-compose.prod.yml
├── docker-compose.scale.yml
├── TEST/
│   ├── readme.md
│   └── targets.csv
├── rstudio/
│   └── Dockerfile
└── shiny/
    ├── Dockerfile
    ├── shiny-server.conf
    └── app/
        ├── app.R
        ├── config.yml
        ├── common_files/
        ├── modules/
        └── www/
```

---

## For Developers

The section below is intended for users who want to modify or extend Met4All.

### Building from Source

First, create the directories the app writes to (if not created previously):

```bash
mkdir -p ./shiny/logs ./shiny/app/data && chmod 777 ./shiny/logs ./shiny/app/data
```

To build both the Shiny and RStudio images locally and start them (first build takes ~20–40 min, as it installs the full Bioconductor stack):

```bash
docker compose -f docker-compose.dev.yml up -d --build
```

To rebuild only the Shiny service after changes:

```bash
docker compose -f docker-compose.dev.yml up -d --build shiny
```

To stop:

```bash
docker compose -f docker-compose.dev.yml down
```

### Accessing the Services

| Service | URL | Credentials |
|---|---|---|
| Shiny app | http://localhost:3838 | - |
| RStudio | http://localhost:3939 | user: `rstudio` / password: `rstudio` |

### Running Individual Services

```bash
# Shiny only
docker compose -f docker-compose.dev.yml up -d --build shiny

# RStudio only
docker compose -f docker-compose.dev.yml up -d --build rstudio
```

### Publishing a New Image to DockerHub

After making changes to the app, build and push a new versioned image:

```bash
docker build -t gcampof/methylation4all-shiny:1.x.x ./shiny
docker push gcampof/methylation4all-shiny:1.x.x

# Also update the latest tag
docker tag gcampof/methylation4all-shiny:1.x.x gcampof/methylation4all-shiny:latest
docker push gcampof/methylation4all-shiny:latest
```

Then update the image tag in `docker-compose.prod.yml` and commit.

### Development Notes

- `docker-compose.dev.yml` mounts `./shiny/app` over `/srv/shiny-server` in the container, so the running app uses the code in your working tree rather than the copy baked into the image. Edit `app.R` locally and pick up changes with `docker compose -f docker-compose.dev.yml restart shiny`, no rebuild needed.
- The production `docker-compose.prod.yml` pulls from DockerHub and does **not** mount the app code. Only `logs/` and `data/` are bind-mounted for persistence.
- The annotation cache is precomputed at image build time and baked in at `/opt/met4all/cache`. Override the location with the `M4A_CACHE_DIR` environment variable, when it is unset (the RStudio container) the app falls back to a local `cache/` directory. Installs that predate this can `rm -rf ./shiny/app/cache`.
- To use the production image for Shiny but run RStudio locally: `docker compose -f docker-compose.prod.yml up -d shiny` and `docker compose -f docker-compose.dev.yml up -d rstudio`.

### Dependencies

Built on [Rocker](https://rocker-project.org/) base images:

- `rocker/rstudio:4.5`
- `rocker/shiny:4.5`
- Bioconductor 3.22
- R 4.5
