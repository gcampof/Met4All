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
  DIRS <- setup_analysis_dir(DIRS, cfg, session)
  
  APP_CACHE <- reactiveVal(NULL)
  
  # Flags
  primary_ui_initialized <- reactiveVal(FALSE)
  primary_server_initialized <- reactiveVal(FALSE)
  heavy_components_loaded <- reactiveVal(FALSE)
  
  # Cleanup on start
  cleanup_old_analysis_dirs(DIRS$data, max_age_hours = 24)
  
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
  
  # Server also initialized at start
  primary_analysis_server("primary_analysis", load_data_return, DIRS, APP_CACHE)
  
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