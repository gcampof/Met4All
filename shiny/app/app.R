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

# UI
ui <- navbarPage(
  id = "main_navbar", 
  title = div(
    class = "d-flex align-items-center",
    icon("dna", class = "me-2", style = "color: #2c7fb8;"),
    span("Methylation4All", style = "font-weight: 600;")
  ),
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#2c7fb8",
    `navbar-bg` = "#ffffff",
    `navbar-height` = "2px"
  ),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      /* Navbar styling */
      .navbar {
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-bottom: 1px solid #e9ecef;
        min-height: 2px !important;
      }
      .navbar-brand {
        font-size: 1.2rem;
        padding: 0.3rem 1rem;
        margin-right: 40px !important;
      }
      .navbar-nav > li > a {
        font-weight: 500;
        font-size: 1rem;
        padding: 0.6rem 1.2rem !important;
        margin: 0 6px;
        border-radius: 8px;
        transition: all 0.2s ease;
      }
      .navbar-nav > li > a:hover {
        background-color: #f0f7fc;
        color: #2c7fb8 !important;
      }
      .navbar-nav > .active > a {
        background-color: #2c7fb8 !important;
        color: white !important;
        box-shadow: 0 2px 6px rgba(44,127,184,0.3);
      }
      .navbar-right-logs {
        float: right;
        margin: 8px 15px 0 0;
      }
      @media (max-width: 768px) {
        .navbar-right-logs {
          margin: 6px 10px 0 0;
        }
      }
      .btn-logout {
        border-radius: 20px;
        padding: 6px 18px;
        font-size: 0.9rem;
        transition: all 0.2s ease;
      }
      .btn-logout:hover {
        background-color: #dc3545;
        color: white;
        border-color: #dc3545;
      }
      .navbar-nav {
        margin-left: 20px;
      }
    ")),
    div(
      class = "navbar-right-logs",
      uiOutput("session_id_display"),
    )
  ),
  tabPanel(
    "New Analysis", 
    value = "load", 
    load_data_ui("load_data")
  ),
  tabPanel(
    "Primary Analysis",
    value = "primary",
    uiOutput("primary_analysis_ui_container")
  )
)


# SERVER 
server <- function(input, output, session) {
  # Set up session configurations
  cfg  <- config::get()
  DIRS <- setup_common_dirs(cfg)
  DIRS <- setup_analysis_dir(DIRS, cfg, session)
  APP_CACHE <- NULL
  
  # Display session ID in navbar
  output$session_id_display <- renderUI({
    session_id <- session$token
    # Get a shorter version (first 8 characters)
    short_id <- substr(session_id, 1, 8)
    
    tags$div(
      class = "session-id-badge",
      icon("fingerprint"),
      tags$span(
        paste0("Session: ", short_id),
        title = paste0("Full Session ID: ", session_id)  # Show full ID on hover
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
  
  # Flags
  module_initialized <- reactiveVal(FALSE)
  heavy_components_loaded <- reactiveVal(FALSE)
  
  # Render UI conditionally
  output$primary_analysis_ui_container <- renderUI({
    if (data_ready()) {
      primary_analysis_ui("primary_analysis", load_data_return$type_selected())
    } else {
      div(
        class = "text-center p-5",
        div(class = "spinner-border text-primary", role = "status",
            style = "width: 3rem; height: 3rem; margin-bottom: 20px;"),
        h4("Loading analysis module...", class = "text-muted")
      )
    }
  })
  
  # Only call server module ONCE when data is ready
  observe({
    req(data_ready())
    # Only initialize once
    if (!module_initialized()) {
      if (!heavy_components_loaded()) {
        APP_CACHE <<- load_heavy_components(session, DIRS, cfg)
        heavy_components_loaded(TRUE)
      }
      
      message("[App] Initializing primary analysis server module...")
      primary_analysis_server("primary_analysis", load_data_return, DIRS, APP_CACHE)
      module_initialized(TRUE)
      # Switch tabs after module is initialized
      updateNavbarPage(session, "main_navbar", selected = "primary")
    }
  })
}

shinyApp(ui, server)
