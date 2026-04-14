source("modules/primary_analysis/mds/mds_ui.R")
source("modules/primary_analysis/pca/pca_ui.R")
source("modules/primary_analysis/umap/umap_ui.R")
source("modules/primary_analysis/heatmap/heatmap_ui.R")
source("modules/primary_analysis/global_met/global_met_ui.R")
source("modules/primary_analysis/differential/differential_met_ui.R")
source("modules/primary_analysis/cnv/cnv_ui.R")

# UI
primary_analysis_ui <- function(id, type_selected) {
  ns <- NS(id)
  is_idats <- type_selected == "IDATS"
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Custom CSS
    tags$style(HTML("
      :root {
        --sidebar-width: 250px;
        --param-panel-width: 280px;
        --sample-panel-width: 220px;
        --header-offset: 400px;
        --main-offset: 100px;
        --plot-offset: 230px;
      }
      
      .param-panel {
        width: var(--param-panel-width);
        min-width: var(--param-panel-width);
        max-height: calc(100vh - var(--header-offset));
        overflow-y: auto;
      }
      
      .plot-card {
        max-height: calc(100vh - var(--plot-offset));
      }
      
      .plot-card > div {
        max-height: calc(100vh - var(--plot-offset));
        width: 100%;
      }
      
      .btn:disabled {
        opacity: 0.6;
        cursor: not-allowed;
      }
      
      .btn-disabled-tooltip {
        position: relative;
      }
      
      .btn-disabled-tooltip:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        bottom: 125%;
        left: 50%;
        transform: translateX(-50%);
        background-color: #333;
        color: white;
        padding: 8px 12px;
        border-radius: 4px;
        white-space: nowrap;
        font-size: 12px;
        z-index: 1000;
        pointer-events: none;
      }
      
      .btn-disabled-tooltip:hover::before {
        content: '';
        position: absolute;
        bottom: 115%;
        left: 50%;
        transform: translateX(-50%);
        border: 6px solid transparent;
        border-top-color: #333;
        z-index: 1000;
        pointer-events: none;
      }
    ")),
    
    div(
      class = "d-flex",
      style = "min-height: calc(100vh - var(--main-offset))",
      
      # --- SIDEBAR NAVIGATION ---
      div(
        id = ns("sidebar"),
        class = "bg-light border-right p-3",
        style = "width: var(--sidebar-width); overflow-y: auto; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
        
        h4("Primary Analysis", class = "mb-4 text-center"),
        
        # Navigation buttons
        div(
          class = "d-flex flex-column gap-2",
          div(
            id = ns("nav_beta_matrix_wrapper"),
            class = if (!is_idats) "nav-btn-wrapper btn-disabled-tooltip" else "nav-btn-wrapper",
            `data-tooltip` = if (!is_idats) "Only available when loading from IDATs" else NULL,
            actionButton(
              ns("nav_beta_matrix"),
              "Beta Matrix",
              class = "btn btn-outline-primary w-100 text-start",
              style = "justify-content: flex-start;",
              disabled = if (!is_idats) TRUE else NULL
            )
          ),
          div(
            id = ns("nav_qc_wrapper"),
            class = if (!is_idats) "nav-btn-wrapper btn-disabled-tooltip" else "nav-btn-wrapper",
            `data-tooltip` = if (!is_idats) "Only available when loading from IDATs" else NULL,
            actionButton(
              ns("nav_qc"),
              "QC Report",
              class = "btn btn-outline-primary w-100 text-start",
              style = "justify-content: flex-start;",
              disabled = if (!is_idats) TRUE else NULL
            )
          ),
          actionButton(
            ns("nav_mds"),
            "Multidimensional Scaling",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          actionButton(
            ns("nav_pca"),
            "Principal Component Analyisis",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          actionButton(
            ns("nav_umap"),
            "UMAP",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          actionButton(
            ns("nav_heatmap"),
            "Heatmap",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          actionButton(
            ns("nav_global"),
            "Global Methylation",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          actionButton(
            ns("nav_differential"),
            "Differential Methylation",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
          ),
          div(
            id = ns("nav_cnv_wrapper"),
            class = if (!is_idats) "nav-btn-wrapper btn-disabled-tooltip" else "nav-btn-wrapper",
            `data-tooltip` = if (!is_idats) "Only available when loading from IDATs" else NULL,
            actionButton(
              ns("nav_cnv"),
              "CNV Analysis",
              class = "btn btn-outline-primary w-100 text-start",
              style = "justify-content: flex-start;",
              disabled = if (!is_idats) TRUE else NULL
            )
          ),

          # Custom palette button input
          hr(style = "margin: 8px 0;"),
          fileInput(
            inputId     = ns("custom_palette_file"),
            label       = tagList(icon("palette"), " Add Color Palette",
                                  tags$span(
                                    style = "cursor: help; color: #6c757d; flex-shrink: 0;",
                                    title = paste0(
                                      "Upload a .txt file with one hex colour per line.\n",
                                      "Example:\n#E41A1C\n#377EB8\n#4DAF4A\n#984EA3"
                                    ),
                                    icon("circle-info")
                                  )
            ),
            accept      = ".txt",
            buttonLabel = "Browse"
          ),
          
          # --- DOWNLOAD SECTION ---
          hr(style = "margin: 8px 0;"),
          div(
            tags$label(
              tagList(icon("download"), " Export Data"),
              style = "font-weight: 600; margin-bottom: 4px;"
            ),
            div(
              id = ns("download_beta_wrapper"),
              downloadButton(
                ns("download_beta"),
                "Beta Matrix (.csv)",
                class = "btn btn-outline-primary w-100 text-start",
                style = "justify-content: flex-start;",
                disabled = TRUE
              )
            ),
            br(),
            div(
              id = ns("download_targets_wrapper"),
              downloadButton(
                ns("download_targets"),
                "Samplesheet (.csv)",
                class = "btn btn-outline-primary w-100 text-start",
                style = "justify-content: flex-start;",
                disabled = TRUE
              )
            )
            
          ),
        )
      ),
      
      # --- MAIN CONTENT AREA ---
      div(
        class = "flex-grow-1 p-4",
        style = "overflow-y: auto; background-color: #f8f9fa;",
        # Title
        div(style = "margin-bottom: 30px;",
            h2(textOutput(ns("view_title")), class = "mb-3"), hr()
        ),
        
        # --- BETA MATRIX VIEW ---
        shinyjs::hidden(
          div(id = ns("view_beta_matrix"), class = "content-section",
              uiOutput(ns("beta_matrix_tabs")),
          )
        ),
        
        # --- QC REPORT VIEW ---
        shinyjs::hidden(
          div(id = ns("view_qc"),
              class = "content-section",
              uiOutput(ns("qc_pdf_tabs")),
          )
        ),
        
        # --- MDS VIEW ---
        shinyjs::hidden(
          div(id = ns("view_mds"), class = "content-section",
              mds_ui(ns)
          )
        ),
        
        # --- PCA VIEW ---
        shinyjs::hidden(
          div(id = ns("view_pca"), class = "content-section",
              pca_ui(ns)
          )
        ),
        
        # --- UMAP VIEW ---
        shinyjs::hidden(
          div(id = ns("view_umap"), class = "content-section",
              umap_ui(ns)
          )
        ),
        
        # --- HEATMAP VIEW ---
        shinyjs::hidden(
          div(id = ns("view_heatmap"), class = "content-section",
              heatmap_ui(ns)
          )
        ),
        
        # --- GLOBAL METHYLATION VIEW ---
        shinyjs::hidden(
          div(id = ns("view_global_met"), class = "content-section",
              global_met_ui(ns)
          )
        ),
        
        # --- DIFFERENTIAL METHYLATION VIEW ---
        shinyjs::hidden(
          div(id = ns("view_differential"), class = "content-section",
              differential_met_ui(ns)
          )
        ),
        
        # --- CNVS VIEW ---
        shinyjs::hidden(
          div(id = ns("view_cnv"), class = "content-section",
              cnv_ui(ns)
          )
        )
      )
    )
  )
}

