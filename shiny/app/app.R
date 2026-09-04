source("modules/common/concurrency.R")
source("modules/common/utils.R")
source("modules/load_data/load_data.R")
source("modules/primary_analysis/primary_analysis_ui.R")
source("modules/primary_analysis/primary_analysis_server.R")
library(config)
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(archive)
library(readxl)
library(readr)
library(openxlsx)
library(readr)
library(DT)
library(data.table)
library(plotly)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(colorspace)
library(svglite)
library(data.table)
library(tools)
library(matrixStats)
library(ggpubr)

# reactlog is deliberately OFF: it grows without bound for the life of the shared
# R process and its /reactlog endpoint exposes every session's reactive graph.
# Set M4A_REACTLOG=1 locally if you need it for debugging.
options(shiny.reactlog = nzchar(Sys.getenv("M4A_REACTLOG")))
options(shiny.maxRequestSize = 10 * 1024^3)

# Stop data.table/BLAS/BiocParallel from each sizing themselves to the whole host.
m4a_apply_thread_caps()

# The worker pool starts lazily on the first heavy analysis; make sure it does
# not outlive the app.
onStop(function() m4a_stop_workers())

# JavaScript reset code
jsResetCode <- "shinyjs.resetPage = function() {window.location.href = window.location.pathname;}"

# UI
ui <- fluidPage(
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#2c7fb8"
  ),
  
  shinyjs::useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = "resetPage"),
  
  tags$head(
    tags$style(HTML("
      /* App shell: the viewport is the frame. The navbar keeps its natural
         height and the content area takes the rest, so the page itself never
         scrolls — only the panels that are meant to. */
      html, body { height: 100%; }
      body { margin: 0; overflow: hidden; }

      body > .container-fluid {
        height: 100%;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        padding: 0;
      }

      .content-container {
        flex: 1 1 auto;
        min-height: 0;
      }

      /* Loading view is a long form and scrolls as a whole; the analysis view
         manages its own internal scrolling. */
      #view_load { height: 100%; overflow-y: auto; }
      #view_primary { height: 100%; min-height: 0; }

      /* Below the width this layout is designed for there is no sensible way to
         fit a 250px nav plus a 280px control column plus a plot, so fall back to
         ordinary page scrolling rather than clipping content. */
      @media (max-width: 900px), (max-height: 520px) {
        body { overflow: auto; }
        body > .container-fluid { height: auto; overflow: visible; }
        #view_load, #view_primary { height: auto; }
      }

      /* Custom navbar styling */
      .custom-navbar {
        flex: 0 0 auto;
        background-color: #ffffff;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-bottom: 1px solid #e9ecef;
        padding: 10px 20px;
        margin-bottom: 20px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      
      .navbar-brand-custom {
        display: flex;
        align-items: center;
        font-size: 1.2rem;
        font-weight: 600;
        color: #2c7fb8;
      }
      
      .navbar-right {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      .session-id-badge {
        display: flex;
        align-items: center;
        gap: 5px;
        padding: 6px 12px;
        background-color: #f8f9fa;
        border-radius: 20px;
        font-size: 0.9rem;
        color: #6c757d;
      }
      
      .btn-new-analysis {
        border-radius: 20px;
        padding: 6px 18px;
        font-size: 0.9rem;
        transition: all 0.2s ease;
      }
      
      .btn-new-analysis:hover {
        background-color: #dc3545;
        color: white;
        border-color: #dc3545;
      }
      
      /* Content container */
      .content-container {
        padding: 0 20px;
      }
    "))
  ),
  
  # Custom navbar
  div(
    class = "custom-navbar",
    div(
      class = "navbar-brand-custom",
      icon("dna", style = "color: #2c7fb8; margin-right: 10px;"),
      span("Met4All")
    ),
    div(
      class = "navbar-right",
      actionButton(
        "restart_session",
        tagList(icon("rotate-right"), " New Analysis"),
        class = "btn btn-outline-danger btn-new-analysis"
      ),
      uiOutput("session_id_display")
    )
  ),
  
  # Main content area - switches between views
  div(
    class = "content-container",
    
    # Both views always in the DOM
    div(id = "view_load",
        load_data_ui("load_data")
    ),
    
    shinyjs::hidden(
      div(id = "view_primary",
          primary_analysis_ui("primary_analysis")  # see note below
      )
    )
  )
)


# SERVER 
server <- function(input, output, session) {
  # Initial setup
  cfg  <- config::get()
  DIRS <- setup_common_dirs(cfg)

  # ?analysis=<id> in the URL identifies work to resume. The id is validated
  # against a strict pattern before it is ever used as a path.
  resume_id <- isolate(parseQueryString(session$clientData$url_search)$analysis)
  DIRS <- setup_analysis_dir(DIRS, cfg, session, resume_id = resume_id)

  # Publish the id so the browser URL is the bookmark. Nothing else is needed to
  # come back to this analysis later.
  updateQueryString(paste0("?analysis=", DIRS$analysis_id), mode = "replace")

  APP_CACHE <- reactiveVal(NULL)
  
  # Display session ID in navbar
  output$session_id_display <- renderUI({
    session_id <- session$token
    short_id <- substr(session_id, 1, 8)
    
    tags$div(
      class = "session-id-badge",
      icon("fingerprint"),
      tags$span(
        paste0("Session:", short_id),
        title = paste0("Full Session ID: ", session_id)
      )
    )
  })
  
  # Initialize data loading
  load_data_return <- load_data_server("load_data", DIRS, cfg)

  # Resume: rehydrate from disk rather than making the user start over. Only a
  # finished analysis has a manifest, so anything half-done starts fresh.
  if (isTRUE(DIRS$resumed)) {
    manifest <- read_analysis_manifest(DIRS$analysis)

    if (is.null(manifest)) {
      # No manifest at all just means the user came back before finishing the
      # upload; that is ordinary and needs no alarm. Warn only when a manifest
      # exists but its artifacts have gone.
      if (file.exists(manifest_path(DIRS$analysis))) {
        showNotification(
          "That analysis could not be restored - its files are no longer available.",
          type = "warning", duration = 8
        )
      }
    } else {
      tryCatch({
        load_data_return$type_selected(manifest$type)
        load_data_return$array_names_ld(manifest$array_names)
        load_data_return$mSetSq_list_ld(manifest$mset_paths)
        load_data_return$targets_merged_ld(readRDS(manifest$targets_path))
        # Set last: the view switch below keys off it.
        load_data_return$beta_merged_ld(readRDS(manifest$beta_path))

        showNotification("Welcome back - your previous analysis has been restored.",
                         type = "message", duration = 6)
      }, error = function(e) {
        showNotification(paste("Could not restore the previous analysis:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    }
  }

  # Keep the samplesheet on disk current. It is edited in-session (cell edits,
  # consensus clusters written back), so without this a resumed analysis would
  # silently lose those changes.
  observeEvent(load_data_return$targets_merged_ld(), {
    req(load_data_return$targets_merged_ld())
    path <- file.path(DIRS$beta, "merged", "targets_merged.rds")
    if (dir.exists(dirname(path))) {
      try(saveRDS(load_data_return$targets_merged_ld(), path), silent = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Server also initialized at start
  primary_analysis_server("primary_analysis", load_data_return, DIRS, APP_CACHE, cfg)
  
  # Simple view switching — just show/hide
  observeEvent(load_data_return$beta_merged_ld(), {
    req(load_data_return$beta_merged_ld())
    load_heavy_components(session, DIRS, cfg, APP_CACHE)
    shinyjs::hide("view_load")
    shinyjs::show("view_primary")
  })
  
  # Restart session with new analysis
  observeEvent(input$restart_session, {
    showModal(modalDialog(
      title = "Start New Analysis",
      "This will reset the entire application. All unsaved data will be lost. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, Start Fresh", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  # Manage analysis reset
  observeEvent(input$confirm_reset, {
    removeModal()
    js$resetPage()
  })
  
}

shinyApp(ui, server)