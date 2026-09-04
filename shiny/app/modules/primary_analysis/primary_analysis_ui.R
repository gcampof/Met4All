source("modules/primary_analysis/mds/mds_ui.R")
source("modules/primary_analysis/pca/pca_ui.R")
source("modules/primary_analysis/umap/umap_ui.R")
source("modules/primary_analysis/heatmap/heatmap_ui.R")
source("modules/primary_analysis/global_met/global_met_ui.R")
source("modules/primary_analysis/differential/differential_met_ui.R")
source("modules/primary_analysis/cnv/cnv_ui.R")

# UI
primary_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Custom CSS
    tags$style(HTML("
      :root {
        --sidebar-width: 250px;
        --param-panel-width: 280px;
        --sample-panel-width: 220px;
      }

      /* Layout is a single flex chain from the viewport down to the plot, so
         heights are derived rather than guessed. Every link needs min-height:0,
         otherwise a flex item refuses to shrink below its content and pushes the
         page into a second scrollbar. Only .param-panel and the sidebar scroll. */

      .pa-shell {
        height: 100%;
        min-height: 0;
      }

      .pa-sidebar {
        flex: 0 0 var(--sidebar-width);
        overflow-y: auto;
      }

      .pa-main {
        display: flex;
        flex-direction: column;
        min-width: 0;
        min-height: 0;
        overflow: hidden;
      }

      .pa-title { flex: 0 0 auto; }

      .content-section {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      /* Views that are not an analysis layout (beta matrix, QC, samplesheet)
         scroll as one block inside the frame rather than growing the page. */
      .content-section > .shiny-html-output,
      .content-section > div:not(.analysis-row) {
        flex: 1 1 auto;
        min-height: 0;
        overflow-y: auto;
      }

      .analysis-row {
        display: flex;
        gap: 1rem;
        flex: 1 1 auto;
        min-height: 0;
        align-items: stretch;
      }

      .analysis-side {
        display: flex;
        flex-direction: column;
        gap: 12px;
        flex: 0 0 var(--param-panel-width);
        /* Without min-width:0 a flex item cannot shrink below its min-content
           width, and the range-slider tick labels alone demand ~580px — which
           is what used to blow the parameter column out past the plot. */
        min-width: 0;
        min-height: 0;
      }

      /* Keep sliders inside the column and drop the dense tick labels, which
         are unreadable at this width anyway (the handle shows the value). */
      .analysis-side .irs { width: 100%; }
      .analysis-side .irs-grid { display: none; }
      .analysis-side .irs-with-grid { padding-bottom: 0; }

      .analysis-main {
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-width: 0;
        min-height: 0;
      }

      /* The scrollable parameter list: takes whatever height is left over. */
      .param-panel {
        flex: 1 1 auto;
        min-height: 0;
        overflow-y: auto;
        overflow-x: hidden;
      }

      .plot-card {
        flex: 1 1 auto;
        min-height: 320px;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }

      .plot-card > div,
      .plot-card > .shiny-plot-output {
        flex: 1 1 auto;
        min-height: 0;
        width: 100%;
      }

      /* Pre-rendered PNGs (beta boxplot, CNV) scale to the card instead of
         overflowing it at their native size. */
      .plot-card img {
        max-width: 100%;
        max-height: 100%;
        height: auto;
        object-fit: contain;
      }

      /* Tabbed views (differential, CNV) must pass the height down too. */
      .analysis-main > .tabbable,
      .analysis-main .tab-content {
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: 0;
      }

      .analysis-main .tab-content > .tab-pane.active {
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: 0;
      }

      /* Compact the control column so more parameters fit without scrolling,
         and stop the scrollbar shifting content sideways as it appears. */
      .param-panel { scrollbar-gutter: stable; }

      .analysis-side .card { padding: 0.75rem !important; }

      .analysis-side .shiny-input-container,
      .analysis-side .form-group {
        width: 100% !important;
        margin-bottom: 0.6rem;
        min-width: 0;
      }

      .analysis-side label,
      .analysis-side .control-label {
        margin-bottom: 0.15rem;
        font-size: 0.85rem;
        font-weight: 500;
      }

      .analysis-side .form-control,
      .analysis-side .selectize-input,
      .analysis-side .form-select {
        font-size: 0.85rem;
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
        min-height: 0;
      }

      /* Sidebar: keep the file picker inside 250px instead of clipping its
         filename field. */
      .pa-sidebar .shiny-input-container { width: 100% !important; }
      .pa-sidebar .input-group { flex-wrap: nowrap; }
      .pa-sidebar .form-control {
        font-size: 0.78rem;
        min-width: 0;
        text-overflow: ellipsis;
      }
      .pa-sidebar .btn-file { white-space: nowrap; }

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
      class = "d-flex pa-shell",

      # --- SIDEBAR NAVIGATION ---
      div(
        id = ns("sidebar"),
        class = "bg-light border-right p-3 pa-sidebar",
        style = "box-shadow: 0 0 10px rgba(0,0,0,0.1);",
        
        h4("Primary Analysis", class = "mb-4 text-center"),
        
        # Navigation buttons
        div(
          class = "d-flex flex-column gap-2",
          div(
            id = ns("nav_beta_matrix_wrapper"),
            class = "nav-btn-wrapper btn-disabled-tooltip",
            `data-tooltip` = "Only available when loading from IDATs",
            actionButton(
              ns("nav_beta_matrix"), "Beta Matrix",
              class = "btn btn-outline-primary w-100 text-start",
              disabled = TRUE 
            )
          ),
          div(
            id = ns("nav_qc_wrapper"),
            class = "nav-btn-wrapper btn-disabled-tooltip",
            `data-tooltip` = "Only available when loading from IDATs",
            actionButton(
              ns("nav_qc"), "QC Report",
              class = "btn btn-outline-primary w-100 text-start",
              disabled = TRUE
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
            "Principal Component Analysis",
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
            class = "nav-btn-wrapper btn-disabled-tooltip",
            `data-tooltip` = "Only available when loading from IDATs",
            actionButton(
              ns("nav_cnv"), "CNV Analysis",
              class = "btn btn-outline-primary w-100 text-start",
              disabled = TRUE
            )
          ),
          actionButton(
            ns("nav_samplesheet"),
            "Explore Samplesheet",
            class = "btn btn-outline-primary w-100 text-start",
            style = "justify-content: flex-start;"
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
            ),
            br(),
            # Everything the analyses printed, for when something looks wrong.
            downloadButton(
              ns("download_log"),
              "Analysis log (.txt)",
              class = "btn btn-outline-secondary w-100 text-start",
              style = "justify-content: flex-start;"
            )

          ),
        )
      ),
      
      # --- MAIN CONTENT AREA ---
      div(
        class = "flex-grow-1 p-4 pa-main",
        style = "background-color: #f8f9fa;",
        # Title
        div(class = "pa-title",
            h2(textOutput(ns("view_title")), class = "mb-2"), hr(class = "mt-2 mb-3")
        ),

        # Live progress for whichever analysis this session is running.
        m4a_progress_output(ns),
        
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
        ),
        
        # --- SAMPLESHEET VIEW ---
        shinyjs::hidden(
          div(id = ns("view_samplesheet"), class = "content-section",
              samplesheet_ui(ns)
          )
        )
      )
    )
  )
}

