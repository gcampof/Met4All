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
