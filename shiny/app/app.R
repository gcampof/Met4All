source("modules/common/utils.R")
source("modules/load_data/load_data.R")
source("modules/primary_analysis/primary_analysis_ui.R")
source("modules/primary_analysis/primary_analysis_server.R")
library(config)
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(HDF5Array)
library(DelayedArray)
library(DelayedMatrixStats)
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
library(qs2)
library(data.table)
library(tools)
library(matrixStats)
library(ggpubr)

options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 10 * 1024^3)

# JavaScript reset code
jsResetCode <- "shinyjs.resetPage = function() {history.go(0)}"

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
      /* Custom navbar styling */
      .custom-navbar {
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
      span("Methylation4All")
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
    uiOutput("main_view")
  )
)


# SERVER 
server <- function(input, output, session) {
  # Initial setup
  cfg  <- config::get()
  DIRS <- setup_common_dirs(cfg)
  DIRS <- setup_analysis_dir(DIRS, cfg, session)
  
  APP_CACHE <- NULL
  
  # Flags
  module_initialized <- reactiveVal(FALSE)
  heavy_components_loaded <- reactiveVal(FALSE)
  
  # Track current view
  current_view <- reactiveVal("load")
  
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
  
  observeEvent(input$confirm_reset, {
    removeModal()
    js$resetPage()
  })
  
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
  
  # Cleanup on start
  cleanup_old_analysis_dirs(DIRS$data, max_age_hours = 24)
  
  # Initialize data loading
  load_data_return <- load_data_server("load_data", DIRS, cfg)
  
  # Create a reactive flag to track when data is fully ready
  data_ready <- reactive({
    !is.null(load_data_return$beta_merged()) &&
      !is.null(load_data_return$targets_merged())
  })
  
  # Main view switcher
  output$main_view <- renderUI({
    if (current_view() == "load") {
      load_data_ui("load_data")
    } else if (current_view() == "primary") {
      if (data_ready()) {
        req(data_ready(), load_data_return$type_selected())
        primary_analysis_ui("primary_analysis", load_data_return$type_selected())
      } else {
        div(
          class = "text-center p-5",
          div(class = "spinner-border text-primary", role = "status",
              style = "width: 3rem; height: 3rem; margin-bottom: 20px;"),
          h4("Loading analysis module...", class = "text-muted")
        )
      }
    }
  })
  
  # Only call server module ONCE when data is ready
  observe({
    req(data_ready())
    if (!module_initialized()) {
      if (!heavy_components_loaded()) {
        APP_CACHE <<- load_heavy_components(session, DIRS, cfg)
        heavy_components_loaded(TRUE)
      }
      
      message("[App] Initializing primary analysis server module...")
      primary_analysis_server("primary_analysis", load_data_return, DIRS, APP_CACHE)
      module_initialized(TRUE)
      
      # Switch to primary analysis view
      current_view("primary")
    }
  })
}

shinyApp(ui, server)