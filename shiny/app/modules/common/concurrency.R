# Concurrency limits, shared by the main R process and every analysis worker.
#
# Shiny Server (open source) runs ONE R process for all connected users, so any
# library that sizes itself from detectCores() will grab the whole host while
# other users wait. Everything is bounded by two settings instead:
#
#   M4A_MAX_JOBS        concurrent heavy analyses (worker pool size)
#   M4A_THREADS_PER_JOB threads inside each worker
#
# Keep M4A_MAX_JOBS * M4A_THREADS_PER_JOB <= host cores.

m4a_env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name)))
  if (is.na(value) || value < 1L) default else value
}

m4a_max_jobs <- function() m4a_env_int("M4A_MAX_JOBS", 2L)

m4a_threads_per_job <- function() m4a_env_int("M4A_THREADS_PER_JOB", 4L)

m4a_min_free_gb <- function() m4a_env_int("M4A_MIN_FREE_GB", 15L)


# --- Disk headroom ----------------------------------------------------------
# Free space in GB on the filesystem holding `path`, or NA if it cannot be read.
m4a_free_disk_gb <- function(path) {
  probe <- path
  while (nzchar(probe) && !dir.exists(probe)) probe <- dirname(probe)
  if (!dir.exists(probe)) return(NA_real_)

  out <- tryCatch(
    suppressWarnings(system2("df", c("-Pk", shQuote(probe)),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL
  )
  if (length(out) < 2) return(NA_real_)

  fields   <- strsplit(trimws(out[length(out)]), "[[:space:]]+")[[1]]
  avail_kb <- suppressWarnings(as.numeric(fields[4]))
  if (is.na(avail_kb)) return(NA_real_)

  avail_kb / 1024^2
}

# Refuse to start when there is clearly not enough room. `expansion` is how many
# times the upload size the finished analysis is expected to occupy. Returns
# invisibly (and never blocks) when free space cannot be determined.
m4a_check_disk <- function(path, upload_bytes = 0, expansion = 6) {
  free <- m4a_free_disk_gb(path)
  if (is.na(free)) return(invisible(NA_real_))

  needed <- max(m4a_min_free_gb(), (upload_bytes / 1024^3) * expansion)
  if (free < needed) {
    stop(sprintf(
      paste0("Not enough free disk space: %.1f GB available but about %.0f GB is needed ",
             "for this analysis. Free up space, or remove old analyses from the data directory."),
      free, needed
    ))
  }

  invisible(free)
}

# --- Memory headroom ---------------------------------------------------------
# Peak RSS, not CPU, is what limits this app, but predicting a job's peak from
# sample count proved unreliable, so nothing is refused up front. Instead a job
# checks its remaining headroom at each stage boundary and stops itself with a
# readable error while there is still room to unwind

m4a_memory_headroom_gb <- function() {
  cg <- function(f) {
    v <- suppressWarnings(tryCatch(readLines(f, n = 1L, warn = FALSE),
                                   error = function(e) character(0)))
    if (length(v) == 0L || v[1] == "max") return(NA_real_)
    suppressWarnings(as.numeric(v[1]))
  }

  # Bytes of a named field in a cgroup memory.stat file, or NA.
  cg_stat <- function(f, field) {
    lines <- suppressWarnings(tryCatch(readLines(f, warn = FALSE),
                                       error = function(e) character(0)))
    hit <- grep(paste0("^", field, " "), lines, value = TRUE)
    if (length(hit) == 0L) return(NA_real_)
    suppressWarnings(as.numeric(sub(paste0("^", field, " "), "", hit[1])))
  }

  # cgroup v2, then v1
  for (spec in list(
    list(max = "/sys/fs/cgroup/memory.max",
         cur = "/sys/fs/cgroup/memory.current",
         stat = "/sys/fs/cgroup/memory.stat", field = "inactive_file"),
    list(max = "/sys/fs/cgroup/memory/memory.limit_in_bytes",
         cur = "/sys/fs/cgroup/memory/memory.usage_in_bytes",
         stat = "/sys/fs/cgroup/memory/memory.stat", field = "total_inactive_file"))) {
    lim <- cg(spec$max); use <- cg(spec$cur)
    # An "unlimited" cgroup reports a huge sentinel; treat it as absent.
    if (!is.na(lim) && !is.na(use) && lim < 2^62) {
      # memory.current includes the page cache, which the kernel drops under
      # pressure rather than failing an allocation. Counting it as used made a
      # container that had merely READ a large file look full. Subtract the
      # reclaimable part, as docker stats and kubernetes do for the working set.
      cache <- cg_stat(spec$stat, spec$field)
      if (!is.na(cache)) use <- max(use - cache, 0)
      return((lim - use) / 1024^3)
    }
  }

  tryCatch({
    hit <- grep("^MemAvailable:", readLines("/proc/meminfo", n = 60L, warn = FALSE),
                value = TRUE)
    if (length(hit) == 0L) return(NA_real_)
    as.numeric(sub("^MemAvailable:\\s*([0-9]+).*$", "\\1", hit[1])) / 1024^2
  }, error = function(e) NA_real_)
}

m4a_min_headroom_gb <- function() m4a_env_int("M4A_MIN_HEADROOM_GB", 2L)

# Called at stage boundaries. Collects garbage before judging, so a transient dip
# right after a big allocation does not abort a job that would have recovered.
m4a_memory_checkpoint <- function(what = "This analysis") {
  floor_gb <- m4a_min_headroom_gb()

  free <- m4a_memory_headroom_gb()
  if (is.na(free) || free >= floor_gb) return(invisible(free))

  gc(full = TRUE)
  free <- m4a_memory_headroom_gb()
  if (is.na(free) || free >= floor_gb) return(invisible(free))

  stop(sprintf(
    paste0("%s stopped: only %.1f GB of memory headroom left (needs at least ",
           "%.0f GB to continue safely). Try fewer samples at once, or give the ",
           "container more memory with M4A_MEM_LIMIT."),
    what, free, floor_gb), call. = FALSE)
}


# Cap every library that would otherwise size itself from detectCores().
#
# `threads` is the WORKER budget. It goes into the environment because mirai
# daemons inherit it at spawn, and process start is the only moment OpenMP and
# OpenBLAS read it -- which is also why it must not be baked into the Dockerfile
# (a 1 there caps data.table permanently; setDTthreads cannot raise it back).
# `local_threads` is what this process uses; the app passes 1, since it
# dispatches rather than computes.
m4a_apply_thread_caps <- function(threads = m4a_threads_per_job(),
                                  local_threads = threads) {
  Sys.setenv(OMP_NUM_THREADS = threads, OPENBLAS_NUM_THREADS = threads)

  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(local_threads)
  }

  # fgsea and friends fall back to bpparam(), which defaults to detectCores() - 2.
  if (requireNamespace("BiocParallel", quietly = TRUE)) {
    BiocParallel::register(
      if (local_threads > 1L) {
        BiocParallel::MulticoreParam(workers = local_threads)
      } else {
        BiocParallel::SerialParam()
      }
    )
  }

  invisible(threads)
}


# --- Worker pool ------------------------------------------------------------
# A bounded pool of persistent worker processes. This IS the admission control:
# mirai queues anything beyond the pool, so at most M4A_MAX_JOBS heavy analyses
# ever run at once no matter how many users press Run. Workers are persistent so
# the Bioconductor stack and the annotation cache are loaded once per worker,
# not once per job.
#
# Started lazily on first use, and degrades to in-process execution when mirai
# is unavailable, so the app still runs on an image built before mirai was added.

m4a_workers_ready <- function() isTRUE(getOption("m4a.workers_ready"))

m4a_ensure_workers <- function(n = m4a_max_jobs()) {
  if (m4a_workers_ready()) return(invisible(TRUE))

  if (!requireNamespace("mirai", quietly = TRUE)) {
    message("[workers] mirai unavailable - analyses will run in the main process")
    return(invisible(FALSE))
  }

  ok <- tryCatch({
    # cleanup = FALSE keeps each daemon's globalenv between tasks. Without it a
    # worker is reset every job, so m4a_worker_init has to re-source every module
    # and re-attach the whole stack each time -- seconds of pure overhead on an
    # analysis that itself takes seconds. It also lets the annotation cache memo
    # survive, so built_annot is read once per worker rather than once per job.
    mirai::daemons(n, cleanup = FALSE)
    TRUE
  }, error = function(e) {
    warning("[workers] could not start worker pool: ", conditionMessage(e))
    FALSE
  })

  if (ok) {
    options(m4a.workers_ready = TRUE)
    message("[workers] pool of ", n, " ready (", m4a_threads_per_job(), " threads each)")
  }
  invisible(ok)
}

m4a_stop_workers <- function() {
  if (m4a_workers_ready()) {
    try(mirai::daemons(0), silent = TRUE)
    options(m4a.workers_ready = FALSE)
  }
  invisible(NULL)
}

# Jobs currently running and waiting. Used to tell a queued user where they are
# rather than leaving them looking at a button that appears to do nothing.
m4a_pool_state <- function() {
  idle <- list(executing = 0L, awaiting = 0L, capacity = m4a_max_jobs())
  if (!m4a_workers_ready()) return(idle)

  st <- tryCatch(mirai::status(), error = function(e) NULL)
  if (is.null(st) || is.null(st$mirai)) return(idle)

  pick <- function(name) {
    v <- suppressWarnings(as.integer(st$mirai[[name]]))
    if (length(v) != 1L || is.na(v)) 0L else v
  }
  list(executing = pick("executing"),
       awaiting  = pick("awaiting"),
       capacity  = m4a_max_jobs())
}

# Short message for the UI, or NULL when the job can start immediately.
m4a_queue_message <- function() {
  s <- m4a_pool_state()
  if (s$awaiting < 1L) return(NULL)
  sprintf("Server busy: %d analysis(es) running, %d queued ahead of yours. It will start automatically.",
          s$executing, s$awaiting)
}


# --- Job dispatch -----------------------------------------------------------
# Run one heavy analysis off the main R thread and return a promise, so the
# Shiny process stays responsive for every other connected user.
#
# The function is named rather than passed by value: sending a closure would
# drag its whole enclosing environment (including reactives and the beta matrix)
# across to the worker. The worker sources the module that defines it instead,
# so only paths and scalar parameters cross the boundary.
#
# Falls back to running in-process when no pool is available, so behaviour is
# unchanged (just blocking) on an image built without mirai.
# `session_dir` is this analysis's directory. It is where the worker publishes
# progress and where its log is written, so the user can watch a long run and
# download what it printed afterwards.
# `heavy = FALSE` for jobs that need no Bioconductor (see m4a_worker_init).
m4a_submit <- function(fn_name, args, app_dir = getwd(), session_dir = NULL,
                       heavy = TRUE) {
  stopifnot(is.character(fn_name), length(fn_name) == 1L, is.list(args))
  # Otherwise the panel shows the previous job's last message until this one's first.
  m4a_clear_progress(session_dir)

  log_file <- if (!is.null(session_dir)) {
    d <- file.path(session_dir, "logs")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    file.path(d, "analysis.log")
  } else {
    NULL
  }

  if (m4a_ensure_workers()) {
    return(mirai::mirai(
      {
        source(file.path(app_dir, "modules/common/worker_init.R"),
               local = FALSE)
        # source() defines into the worker's globalenv, but a mirai expression is
        # not evaluated in a child of globalenv, so the lookup must be explicit —
        # otherwise this fails with "could not find function".
        get("m4a_worker_init", envir = globalenv())(app_dir, heavy)

        # Cleared on the way out: daemons are persistent, so a leftover value
        # would send the next user's progress into this user's directory.
        options(m4a.progress_dir = session_dir)
        on.exit(options(m4a.progress_dir = NULL), add = TRUE)
        # Hand pages back before the daemon goes idle.
        on.exit(gc(full = TRUE), add = TRUE)

        # Capture what the analysis prints into this analysis's own log. sink()
        # is process-global, which is exactly why it must never be used in the
        # shared app process -- but a worker handles one task at a time, so here
        # it is safe and gives the user a downloadable record.
        if (!is.null(log_file)) {
          con <- file(log_file, open = "a")
          writeLines(paste0("=== ", format(Sys.time()), "  ", fn_name, " ==="), con)
          sink(con, type = "message")
          sink(con, type = "output", split = FALSE)
          on.exit({
            for (ty in c("output", "message")) {
              if (sink.number(type = ty) > 0) sink(type = ty)
            }
            close(con)
          }, add = TRUE)
        }

        # Resolve to the function object before calling. Passing the name to
        # do.call() relies on lookup rules that do not hold inside a mirai
        # expression, which surfaces as "could not find function".
        fun <- get(fn_name, envir = globalenv(), mode = "function")
        do.call(fun, args)
      },
      fn_name     = fn_name,
      args        = args,
      session_dir = session_dir,
      log_file    = log_file,
      app_dir     = app_dir,
      heavy       = heavy
    ))
  }

  # In-process fallback. The error must come back as a REJECTED promise, not as a
  # synchronous throw out of this function, or ExtendedTask$invoke() would fail
  # instead of moving the task to the "error" state.
  outcome <- tryCatch(do.call(fn_name, args), error = function(e) e)
  if (inherits(outcome, "error")) {
    promises::promise_reject(outcome)
  } else {
    promises::promise_resolve(outcome)
  }
}


# --- Progress reporting -----------------------------------------------------
# A worker cannot talk to a session: there is none, and a mirai returns a single
# value only when it finishes. So progress goes through the filesystem, the same
# hand-off the analyses already use. The worker writes a tiny file, the main
# process polls it and updates the UI.
#
# The destination is set per task by m4a_submit(), so analysis functions just
# call m4a_progress() without having to thread a directory through every
# signature.

m4a_progress_path <- function(dir) file.path(dir, ".m4a_progress.rds")

# Worker-side. Cheap enough to call between steps of a long analysis.
# `check = FALSE` for steps that allocate nothing (saving a table, reporting
# completion). Aborting there discards an analysis that has already succeeded,
# which is the opposite of what the guard is for.
m4a_progress <- function(value, total, detail = "", check = TRUE) {
  # Every stage boundary already calls this, which makes it the natural place to
  # check headroom. Raises, so it must sit outside the tryCatch below.
  if (isTRUE(check)) {
    m4a_memory_checkpoint(if (nzchar(detail)) detail else "This analysis")
  }

  dir <- getOption("m4a.progress_dir")
  if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) return(invisible(NULL))

  tryCatch({
    target <- m4a_progress_path(dir)
    tmp    <- paste0(target, ".tmp")
    saveRDS(list(value = value, total = total, detail = detail, at = Sys.time()), tmp)
    # Write-then-rename so a reader polling mid-write never sees a partial file.
    file.rename(tmp, target)
  }, error = function(e) NULL)

  invisible(NULL)
}

# Main-process side.
m4a_read_progress <- function(dir) {
  if (is.null(dir)) return(NULL)
  p <- m4a_progress_path(dir)
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

m4a_clear_progress <- function(dir) {
  if (!is.null(dir)) unlink(m4a_progress_path(dir), force = TRUE)
  invisible(NULL)
}

# Percentage for a progress bar, or NULL when the analysis has not said yet.
m4a_progress_pct <- function(p) {
  if (is.null(p) || is.null(p$total) || is.na(p$total) || p$total <= 0) return(NULL)
  max(0, min(100, round(100 * p$value / p$total)))
}


# --- Progress UI -------------------------------------------------------------
# A live progress panel for a running analysis. This only works because the
# analysis is in a worker: the main process is free to keep flushing updates to
# the browser. While everything ran inline, withProgress() and friends could
# never appear, because Shiny only sends messages between reactive cycles and a
# blocking call never reaches the end of one.

m4a_progress_output <- function(ns, id = "m4a_progress") {
  shiny::uiOutput(ns(id))
}

# Renders the panel and keeps it refreshed. `task` is an ExtendedTask,
# `session_dir` the analysis directory the worker publishes into.
m4a_render_progress <- function(output, session, task, session_dir,
                                id = "m4a_progress", poll_ms = 1000) {
  output[[id]] <- shiny::renderUI({
    status <- task$status()
    if (!identical(status, "running")) return(NULL)

    # Re-read while the job runs; this is what makes the bar move.
    shiny::invalidateLater(poll_ms, session)

    p   <- m4a_read_progress(session_dir)
    pct <- m4a_progress_pct(p)
    detail <- if (!is.null(p) && nzchar(p$detail)) p$detail else "Starting..."

    # Before the first checkpoint there is nothing to measure, so show an
    # indeterminate bar rather than a fake 0%.
    bar_style <- if (is.null(pct)) {
      "width:100%; background-image:linear-gradient(45deg,rgba(255,255,255,.2) 25%,transparent 25%,transparent 50%,rgba(255,255,255,.2) 50%,rgba(255,255,255,.2) 75%,transparent 75%,transparent); background-size:1rem 1rem;"
    } else {
      paste0("width:", pct, "%;")
    }

    shiny::div(
      class = "card p-3 mb-3",
      style = "border-left: 3px solid #0d6efd;",
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::span(shiny::icon("spinner", class = "fa-spin"), " ", detail,
                    style = "font-size: 0.9rem;"),
        shiny::span(if (is.null(pct)) "" else paste0(pct, "%"),
                    class = "text-muted", style = "font-size: 0.85rem;")
      ),
      shiny::div(
        class = "progress", style = "height: 8px;",
        shiny::div(class = "progress-bar", role = "progressbar", style = bar_style)
      )
    )
  })
}


# --- Log download ------------------------------------------------------------
# Workers append what they print to <analysis_dir>/logs/analysis.log (see
# m4a_submit). This hands that file to the user so they can see what happened
# without needing access to the server.
m4a_log_download_handler <- function(session_dir, analysis_id = NULL) {
  shiny::downloadHandler(
    filename = function() {
      paste0("met4all_log_",
             if (is.null(analysis_id)) format(Sys.Date()) else analysis_id, ".txt")
    },
    content = function(file) {
      log_file <- file.path(session_dir, "logs", "analysis.log")
      if (file.exists(log_file)) {
        file.copy(log_file, file)
      } else {
        writeLines(
          c("No analysis log yet.",
            "",
            "This file records what each analysis prints while it runs.",
            "It fills in once you start an analysis."),
          file
        )
      }
    }
  )
}


# --- Pre-warming --------------------------------------------------------------
# Loading the Bioconductor stack in a worker costs ~16 s. Paid on the first
# analysis, that turns a 4-second MDS into a 20-second one. So pay it in the
# background as soon as the user has data, while they are still looking at it.
#
# Handles are kept so the tasks are not garbage collected before they run.
m4a_warm_workers <- function(app_dir = getwd()) {
  if (!m4a_ensure_workers()) return(invisible(FALSE))
  if (isTRUE(getOption("m4a.workers_warmed"))) return(invisible(TRUE))
  options(m4a.workers_warmed = TRUE)

  # Warm ONE daemon, not the whole pool. A daemon holding the Bioconductor stack
  # costs ~2 GB resident, and warming every slot spent that before any analysis
  # started. One is enough to hide the start-up latency from the first job; the
  # rest initialise only if a second user actually runs something concurrently.
  handles <- lapply(seq_len(m4a_env_int("M4A_WARM_WORKERS", 1L)), function(i) {
    mirai::mirai(
      {
        source(file.path(app_dir, "modules/common/worker_init.R"), local = FALSE)
        # Warming is always the full stack: that is the cost being paid up front.
        get("m4a_worker_init", envir = globalenv())(app_dir, heavy = TRUE)
        # Hold the slot briefly so the next warm-up lands on a different daemon.
        Sys.sleep(1)
        TRUE
      },
      app_dir = app_dir
    )
  })

  assign(".M4A_WARM_HANDLES", handles, envir = globalenv())
  message("[workers] warming ", length(handles), " of ", m4a_max_jobs(),
          " worker(s) in the background")
  invisible(TRUE)
}
