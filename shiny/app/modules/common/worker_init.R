# Bootstrap run inside each analysis worker.
#
# Workers are plain R processes with none of the app loaded, so they set their
# own thread caps and load their own packages. Everything here is memoised per
# worker via an option, so the (slow) Bioconductor load is paid once per worker
# rather than once per job — which is why the pool uses persistent daemons
# instead of spawning a process per analysis.
#
# Nothing in this file, or in anything it sources, may call Shiny session
# functions (showNotification, shiny::validate, req): there is no session here.

m4a_worker_init <- function(app_dir) {
  # The "already initialised" marker must live in globalenv, alongside the
  # functions it is guarding. mirai clears a daemon's globalenv between tasks
  # but R options persist, so an option-based flag would survive while the
  # definitions it vouched for were gone — the second task on a warm daemon then
  # skips loading and fails with "could not find function". Keeping both in the
  # same environment makes them consistent by construction.
  if (exists(".m4a_worker_ready", envir = globalenv(), inherits = FALSE)) {
    return(invisible(TRUE))
  }

  setwd(app_dir)

  source("modules/common/concurrency.R")
  m4a_apply_thread_caps()

  suppressPackageStartupMessages({
    source("modules/common/all_imports.R")

    # all_imports.R covers the Bioconductor stack only. These are attached by
    # app.R in the main process, which a worker never runs — without them
    # tidy_fgsea() (%>% and dplyr) and the xlsx writers fail here.
    library(dplyr)
    library(tibble)
    library(data.table)
    library(matrixStats)
    library(openxlsx)
    library(ggplot2)
    library(RColorBrewer)
    library(viridis)
    library(colorspace)

    source("modules/common/utils.R")
    source("modules/primary_analysis/annotations.R")
    source("modules/primary_analysis/utils.R")
    source("modules/primary_analysis/cnv/cnv_utils.R")
    source("modules/primary_analysis/differential/differential_utils.R")
  })

  assign(".m4a_worker_ready", TRUE, envir = globalenv())
  invisible(TRUE)
}
