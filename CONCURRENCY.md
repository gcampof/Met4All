# Concurrency and workers — maintainer's guide

How Met4All handles several users at once, what the moving parts are, and the
rules you have to keep in mind when adding or changing an analysis.

---

## 1. The problem this solves

Met4All is served by Shiny Server (open source), whose Simple Scheduler
associates **one R process with the application**. Every connected user shares
that process, and R is single-threaded.

Originally every analysis ran inline in that process. While one user's IDAT QC
ran (10–25 min), every other user's session was frozen — no plots, no
navigation, not even a notification, because Shiny can only flush messages to
the browser between reactive cycles.

The fix is not to make analyses faster. It is to run them **somewhere else**, so
the shared process stays free to serve everyone.

---

## 2. The model in one picture

```
            browser A        browser B        browser C
                |                |                |
                +--------- websockets ------------+
                                 |
                    ONE R process (Shiny Server)
                    - UI, reactivity, plots, tables
                    - holds only small objects
                    - never runs a long analysis
                                 |
                        m4a_submit()  (mirai)
                                 |
              +------------------+------------------+
              |                                     |
        worker 1 (R process)                  worker 2 (R process)
        - Bioconductor loaded once            - same
        - reads inputs from disk              - same
        - writes results to disk              - same
                          \                  /
                           \                /
                       ./shiny/app/data/analysis_<id>/
                       (the shared filesystem is the hand-off)
```

Two rules follow from this diagram, and almost every bug in this area comes
from breaking one of them:

1. **Large data moves by path, never by value.** The worker reads it from disk.
2. **Worker code cannot touch the session.** There is no session in a worker.

---

## 3. The files you need to know

| File | Role |
|---|---|
| `shiny/app/modules/common/concurrency.R` | Limits, worker pool, `m4a_submit()`, disk guard. The core of it. |
| `shiny/app/modules/common/worker_init.R` | What a worker loads before it can run anything. |
| `shiny/app/modules/common/utils.R` | Annotation cache memo, analysis dirs, manifests, `beta_descriptor()`. |
| `shiny/shiny-server.conf` | `app_idle_timeout` — keeps the process (and workers) alive. |
| `shiny/Dockerfile` | Thread caps and pool defaults as `ENV`. |
| `docker-compose.scale.yml` | Optional multi-instance deployment. |

---

## 4. `concurrency.R` — the core

### Settings

Everything is driven by three environment variables, defaulted in the Dockerfile:

| Variable | Default | Meaning |
|---|---|---|
| `M4A_MAX_JOBS` | 2 | Worker pool size = max concurrent analyses |
| `M4A_THREADS_PER_JOB` | 2 | Threads inside each worker |
| `M4A_MIN_FREE_GB` | 15 | Refuse to start with less free disk |

Keep `MAX_JOBS × THREADS_PER_JOB` within the host's cores.

### Thread caps — `m4a_apply_thread_caps()`

Four libraries independently size themselves from `detectCores()` and would each
grab the whole machine: `data.table` (OpenMP), BLAS/LAPACK, `BiocParallel`
(fgsea), and ChAMP. This function pins all of them to `M4A_THREADS_PER_JOB`.

Called once in `app.R` and once in every worker. `OMP_NUM_THREADS` and
`OPENBLAS_NUM_THREADS` are *also* set in the Dockerfile, because OpenMP reads
its environment when its pool is first created and may ignore a later
`Sys.setenv`.

### The pool — `m4a_ensure_workers()`

Starts `mirai::daemons(M4A_MAX_JOBS)` on first use, once per process.

**The pool is the admission control.** mirai queues anything beyond the pool
size, so at most `M4A_MAX_JOBS` analyses ever run regardless of how many users
press Run. There is no separate queue to maintain. `m4a_queue_message()` turns
`mirai::status()` into the "2 running, 1 queued ahead of yours" text.

If `mirai` is unavailable the pool never starts and everything falls back to
running in-process — slower and blocking, but working. That is why the app
survives on an image built before `mirai` was added.

### Dispatch — `m4a_submit(fn_name, args, app_dir)`

The single entry point for offloading work. Returns a promise.

```r
m4a_submit("prepare_mds_data",
           list(beta_path = ..., targets = ..., id_col = ..., top_cpgs = ...),
           app_dir)
```

Three deliberate choices, each of which cost a bug to learn:

- **The function is passed by NAME, not by value.** Sending a closure would drag
  its whole enclosing environment — reactives, the beta matrix — into the
  worker. The worker sources the module that defines it instead.
- **The function object is resolved explicitly** with
  `get(fn_name, envir = globalenv(), mode = "function")` before calling.
  `do.call("name", args)` relies on lookup rules that do not hold inside a mirai
  expression and fails with *"could not find function"*.
- **The in-process fallback returns a rejected promise**, not a synchronous
  throw, or `ExtendedTask$invoke()` would error instead of moving the task to
  its `"error"` state.

### Disk guard — `m4a_check_disk()`

Called before extracting an upload. Sizes the requirement from the actual upload
(6× expansion) with a floor of `M4A_MIN_FREE_GB`. **Never blocks when free space
cannot be determined** — an exotic filesystem must not lock users out.

---

## 5. `worker_init.R` — what a worker is

A worker starts as a bare R process. `m4a_worker_init(app_dir)` does the setup:
`setwd`, thread caps, a null graphics device, then loads the Bioconductor stack
and sources every analysis module.

### The two traps here

**Daemons run with `cleanup = FALSE`, so their state persists between tasks.**

Without it mirai resets a daemon after every job, so `m4a_worker_init` re-sources
every module and re-attaches the whole stack each time -- seconds of overhead on
an analysis that itself takes seconds. With it, init runs once per daemon and the
annotation cache memo survives too.

The trade: anything an analysis writes into the worker's `globalenv` is still
there for the next job, which may belong to a different user. Keep analysis
functions to local scope. The only deliberate global is the `setup_cache()` memo,
which is read-only shared data.

**The "already initialised" marker lives in `globalenv()`, not in an option.**

mirai clears a daemon's `globalenv` between tasks but **R options persist**. An
option-based flag therefore survived while the functions it vouched for were
gone, so the *second* task on a warm daemon skipped loading and failed with
"could not find function". Intermittent, and only under reuse — the exact
production pattern. Keeping the marker and the definitions in the same
environment makes them consistent by construction.

Measured consequence: init runs on **every** task, but the expensive part
(reading namespaces off disk) is only paid once per process. First task ~26 s,
subsequent tasks ~1–2 s.

**A worker has no graphics device.** Without `options(device = ...)` any
unguarded plot call writes `Rplots.pdf` into the working directory, which is
inside the image and read-only under Apptainer/Singularity.

### When you add a module

Add its `source()` line to `worker_init.R`. If you forget, the worker fails with
"could not find function" — which is also what the *other* bug above looks like,
so check this first.

---

## 6. The per-analysis pattern

Every analysis follows the same shape. Here is the whole contract.

### Worker side (in `*_utils.R`)

```r
# Runs in a worker: takes paths and small values, returns small values.
prepare_something <- function(beta_path, targets, id_col, out_dir) {
  if (length(targets) == 0) stop("...")     # plain stop(), NOT shiny::validate
  beta <- readRDS(beta_path)                # large input arrives as a path
  message("[something] working")            # message(), NOT showNotification
  ...
  list(table = small_df, png = written_path)   # small return only
}
```

### Server side (in `primary_analysis_server.R` or `load_data.R`)

```r
something_task <- ExtendedTask$new(function(args, app_dir) {
  m4a_submit("prepare_something", args, app_dir)
})

observeEvent(input$something_run, {
  req(targets_merged())
  validate(need(file.exists(beta_rds_path()), "Beta matrix not found..."))

  queued <- m4a_queue_message()
  showNotification(if (is.null(queued)) "Running..." else queued, type = "message")

  something_task$invoke(                    # snapshot reactives HERE
    args = list(beta_path = beta_rds_path(),
                targets   = targets_merged(),
                id_col    = input$something_id_col),
    app_dir = app_dir
  )
})

observe({                                   # keep the button honest
  if (identical(something_task$status(), "running")) shinyjs::disable("something_run")
  else shinyjs::enable("something_run")
})

something_data <- reactive({
  status <- something_task$status()
  validate(need(status != "initial", "Press Run Analysis to start."))
  validate(need(status != "running", "Running..."))
  tryCatch(something_task$result(),
           error = function(e) { validate(need(FALSE, conditionMessage(e))); NULL })
})
```

### The five rules

1. **No Shiny session calls in worker code** — no `showNotification`,
   `removeNotification`, `shiny::validate`, `shiny::need`, `req`. Use `stop()`
   and `message()`. This applies transitively: anything a worker function
   *calls* must also be clean.
2. **`ExtendedTask` must not read reactives.** Snapshot every value at
   `invoke()` time and pass it in. Reading `input$x` inside the task is a bug
   even when it appears to work.
3. **Large in, by path. Large out, by path.** Anything above a few MB.
4. **Side effects go in a success observer, not in the reactive.** Writing
   `targets_merged` or a cached model from inside a `reactive()` fires
   unpredictably.
5. **Never mutate the shared annotation cache.** It is shared *by reference*
   across every session in the process. In particular no `data.table` `:=` on
   `built_annot`.

### Two payload traps worth knowing

**Do not return a ggplot from a worker.** `aes()` quosures capture their
defining environment. Global methylation's plot function was defined where the
beta matrix and its three subsets lived, so returning the plot would have
dragged hundreds of MB back. The fix is the pattern used by both global
methylation and the heatmap: the worker returns the plain summary data, and the
main process assembles the plot object.

**mirai errors are not R errors.** They come back as
`c("miraiError", "errorValue", "try-error")`, which **do not inherit from
`"error"`**, so a plain `tryCatch(error = )` will not catch them. The promise
layer *does* reject properly, which is why `ExtendedTask$status()` reports
`"error"` and `$result()` throws — that is what the server pattern above relies
on. If you ever call `m4a_submit()` and collect with `m[]` directly, check
`inherits(r, "miraiError")` yourself.

---

## 7. What lives where at runtime

### The main process holds only small things

| Object | Size | Notes |
|---|---|---|
| Annotation cache | ~185 MB, **once for all sessions** | `built_annot` + slim `raw_annot` + pathways, memoised in `globalenv()` |
| `beta_merged` | ~1.4 KB per session | A **descriptor** — path, sample names, probe count |
| Analysis results | small | Tables, plot specs, file paths |

`beta_descriptor()` is why per-session memory went from ~450 MB to ~1.4 KB.
Nothing in the main process needs the matrix any more; every analysis reads it
from disk in a worker. If you add an analysis, **do not** reintroduce
`readRDS(beta_path)` in the main process.

### The slim `raw_annot`

`raw_annot` used to be the full ~40-column annotation: 969 MB in memory,
138 MB on disk, and its cached read (29.8 s) was *slower* than rebuilding it.
Only two columns are ever used (`chr`, `Relation_to_Island`, by the
CGI/Shore/Shelf subsets), so it is sliced down to those — 5.3 MB on disk.

Keep **at least two columns**. `annot[cond, ]` on a single-column data frame
drops to a vector, `rownames()` becomes `NULL`, and the facets go silently
empty. `.M4A_RAW_ANNOT_COLS` in `utils.R` is the list, and the call sites also
pass `drop = FALSE` as a second line of defence.

---

## 7a. Measured cost (800k probes x 70 samples)

Benchmarked on synthetic data at production scale with real EPIC probe IDs, UI
default parameters, one fresh worker per measurement. Peak RSS is the worker
process high-water mark (`VmHWM`), and **includes** the ~1.5 GB Bioconductor
stack every worker loads.

| Analysis | Wall time | Peak worker RSS | Returned payload |
|---|---|---|---|
| MDS (top 10k) | 10.4 s | 2.9 GB | 0.02 MB |
| PCA (top 10k) | 11.2 s | 3.0 GB | 0.02 MB |
| UMAP (top 10k) | 12.0 s | 3.2 GB | 6.1 MB |
| Global methylation | 14.1 s | 3.6 GB | 0.02 MB |
| Heatmap CC (1k probes, 500 reps) | 13.4 s | 3.3 GB | 1.4 MB |
| **Differential (limma, no ChAMP)** | **46.6 s** | **9.5 GB** | 3.3 MB |

Not measured — they need real IDATs and are still code-derived estimates: IDAT
QC (10-25 min), beta generation (15-40 min), CNV (5-20 min).

Other measured figures:

- Beta matrix: 482 MB in memory, 288 MB as `.rds`, 56 s to write.
- Main process: **165 MB idle**, 346 MB after six analyses. The matrix is never
  loaded there.
- Worker start-up (Bioconductor load): ~16-26 s, once per worker process. It is
  paid in the background by `m4a_warm_workers()`, triggered as soon as the user
  has data, so the first analysis does not wait for it.
- Measured effect on a 400k x 30 dataset: first MDS went from **20.2 s to 3.0 s**,
  and steady-state from 3.9 s to **1.9 s**.

### Sizing

**Differential dominates and sets the ceiling.** Its `methylation_genemat_dt`
step melts the probe-gene join to ~2x10^7 rows, which is where the 9.5 GB goes.

```
RAM  =  ~0.5 GB (main)  +  M4A_MAX_JOBS x  3 GB   (typical analyses)
        ~0.5 GB (main)  +  M4A_MAX_JOBS x 10 GB   (worst case: concurrent differential)
```

On the 24 GB the README asks for, `M4A_MAX_JOBS=2` is the right default: two
concurrent differential runs peak near 20 GB, which fits but has little room to
spare. **Do not raise `M4A_MAX_JOBS` above 2 without more RAM** — 4 concurrent
differential runs would need ~40 GB.

---

## 8. Session recovery

An analysis is identified by `analysis_<timestamp>_<32-hex session token>` and
that id is published in the URL as `?analysis=<id>`.

- `write_analysis_manifest()` records the type, array names and the paths of the
  finished artifacts into `manifest.rds` in the analysis directory.
- On session start, `setup_analysis_dir(..., resume_id =)` reuses the directory
  if a valid id names one that exists, and `app.R` rehydrates the reactiveVals
  from the manifest.
- The samplesheet is re-saved on every change, because it is edited in-session
  (cell edits, consensus clusters written back) and would otherwise be lost.

### Two things to preserve if you touch this

**The id is a bearer token.** Anyone with the URL gets the analysis. That is why
the full 32-character session token is used rather than a short prefix, and why
`valid_analysis_id()` enforces a strict pattern *before* the id is used as a
path segment. Do not loosen that regex.

**`run_beta_generation()` writes the manifest itself, inside the worker.** It
used to be written by the observer reacting to the worker finishing — but that
observer belongs to the session, so a user who closed the tab mid-run had the
analysis complete on disk with nothing recording it. Any future long stage
should write its own manifest for the same reason.

### What is not covered

An analysis still *running* when the tab closes will finish (see
`app_idle_timeout` below), but its return value goes to a dead session, so the
user must re-run it. Persisting in-flight results would need workers to write
their return values to disk.

---

## 9. `app_idle_timeout` — do not remove it

`shiny-server.conf` sets `app_idle_timeout 3600`. The default is **5 seconds**,
which is wrong here in two ways:

- The workers are children of the R process, so a user who starts a 40-minute
  run and closes the tab would have it killed seconds later.
- It discards the annotation cache and the worker pool on every lull, so the
  next visitor pays the rebuild again.

The cost is an idle process holding ~2 GB for up to an hour.

---

## 10. Deployment

### Default — one instance

`docker-compose.prod.yml`. Analyses run in the worker pool; sizing is
`RAM ≈ 2 GB + M4A_MAX_JOBS × per-job peak`.

### Optional — several instances

`docker-compose.scale.yml`, then
`docker compose -f docker-compose.scale.yml up -d --scale shiny=4`.

Things to know:

- **Traefik must be ≥ 3.7.** Older 3.x pin Docker API 1.24, which Docker 25+
  refuses, leaving the proxy with no backends and **every request a 404**.
  `DOCKER_API_VERSION` does not fix it; Traefik ignores it.
- **Sticky sessions are mandatory, not an optimisation.** Session state lives in
  the R process that served the page, and download URLs plus the SockJS polling
  fallback are ordinary HTTP requests that must reach the same instance.
- Timeouts are set to 0 because an analysis can run 40 minutes with no traffic
  on the websocket.
- The stack declares `name: met4all-scale` so bringing it up or down cannot
  touch the dev/prod containers, which otherwise share the default project name
  taken from the directory.

### HPC / Apptainer

- `M4A_DATA_DIR` and `M4A_CACHE_DIR` move the writable directories outside the
  image, which Apptainer mounts read-only.
- **Known blocker:** `run_as shiny` in `shiny-server.conf`. Under Apptainer you
  are the invoking user and cannot setuid. Bind-mount a config with
  `run_as :HOME_USER:`.
- The scale profile needs the Docker socket, which HPC will not have. For
  multi-node, mirai supports remote daemons launched via SLURM — the workers are
  already stateless and path-based, which is the shape that needs.

---

## 11. How to verify a change

Parse checks and unit tests are **not sufficient** — a missing reactiveVal once
passed every check while the app was completely dead on load. Always:

1. Parse every changed file.
2. Run the worker in a container and check the function is reachable and returns
   what you expect.
3. **Load the app in a browser** and confirm the socket is open, there is no
   disconnect overlay, and a server round-trip works.
4. Check the session log for `could not find function` and `Error in`.

The call-graph sweep is the highest-value check when touching worker code: walk
every function reachable from the worker entry points and grep for Shiny session
calls. It catches the transitive case that an entry-point-only check misses.
