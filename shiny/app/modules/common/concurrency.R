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
# Sizing rule for memory: RAM ~= 2 GB + M4A_MAX_JOBS * 5 GB.

m4a_env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name)))
  if (is.na(value) || value < 1L) default else value
}

m4a_max_jobs <- function() m4a_env_int("M4A_MAX_JOBS", 2L)

m4a_threads_per_job <- function() m4a_env_int("M4A_THREADS_PER_JOB", 2L)

m4a_min_free_gb <- function() m4a_env_int("M4A_MIN_FREE_GB", 15L)


# --- Disk headroom ----------------------------------------------------------
# A single 70-sample EPIC run leaves roughly 7-13 GB behind (raw upload, sorted
# copy, RGChannelSet, MethylSet, beta matrix, exports). Nothing used to check
# for space, so a full volume surfaced as an opaque failure mid-analysis.

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

# Apply the per-job thread cap to every library that would otherwise size itself
# from detectCores(). Call once at process start and once inside each worker.
# OMP/BLAS are also set in the Dockerfile, because OpenMP reads its environment
# when the pool is first created and may ignore a later Sys.setenv.
m4a_apply_thread_caps <- function(threads = m4a_threads_per_job()) {
  Sys.setenv(OMP_NUM_THREADS = threads, OPENBLAS_NUM_THREADS = threads)

  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(threads)
  }

  # fgsea and friends fall back to bpparam(), which defaults to detectCores() - 2.
  if (requireNamespace("BiocParallel", quietly = TRUE)) {
    BiocParallel::register(
      if (threads > 1L) {
        BiocParallel::MulticoreParam(workers = threads)
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
    mirai::daemons(n)
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
m4a_submit <- function(fn_name, args, app_dir = getwd()) {
  stopifnot(is.character(fn_name), length(fn_name) == 1L, is.list(args))

  if (m4a_ensure_workers()) {
    return(mirai::mirai(
      {
        source(file.path(app_dir, "modules/common/worker_init.R"),
               local = FALSE)
        # source() defines into the worker's globalenv, but a mirai expression is
        # not evaluated in a child of globalenv, so the lookup must be explicit —
        # otherwise this fails with "could not find function".
        get("m4a_worker_init", envir = globalenv())(app_dir)
        # Resolve to the function object before calling. Passing the name to
        # do.call() relies on lookup rules that do not hold inside a mirai
        # expression, which surfaces as "could not find function".
        fun <- get(fn_name, envir = globalenv(), mode = "function")
        do.call(fun, args)
      },
      fn_name = fn_name,
      args    = args,
      app_dir = app_dir
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
