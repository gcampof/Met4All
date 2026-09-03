# Module UI function
cnv_ui <- function(ns) {
  div(
    class = "analysis-row",
    
    # LEFT PANEL
    div(
      class = "analysis-side",
      
      # ---- TOP ACTION BAR ----
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        
        # Run Analysis Button
        div(
          style = "margin-bottom: 12px;",
          actionButton(
            ns("cnv_run_analysis"),
            " Run Analysis",
            class = "btn btn-primary w-100",
            icon = icon("play"),
            style = "font-weight: bold;"
          )
        ),
        
        # Dynamic Export Buttons (changes based on active tab)
        div(
          style = "border-top: 1px solid #dee2e6; padding-top: 12px;",
          uiOutput(ns("cnv_export_buttons"))
        )
      ),
      
      # ---- SCROLLABLE PARAMETER PANEL ----
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        # BED File Upload
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("file", style = "font-size: 0.75rem;"), " BED File"),
          
          fileInput(
            inputId     = ns("cnv_bed_file"),
            label       = tagList(icon("upload"), " Upload BED file",
                                  tags$span(
                                    style = "cursor: help; color: #6c757d; flex-shrink: 0;",
                                    title = "Upload a .bed or .bed.gz file with genomic regions",
                                    icon("circle-info")
                                  )
            ),
            accept      = c(".bed", ".bed.gz"),
            buttonLabel = "Browse"
          ),
          
          # Status indicator
          uiOutput(ns("cnv_bed_status"))
        ),
        
        # Array type selector
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("microchip", style = "font-size: 0.75rem;"), " Array Type"),
          
          selectInput(
            ns("cnv_array_select"),
            "Array platform:",
            choices = NULL
          ),
          
          p(class = "text-muted",
            style = "font-size: 0.7rem; margin-top: -6px; margin-bottom: 4px;",
            "Select the array type to configure CNV analysis")
        ),
        
        # Comparison Setup
        div(
          style = "border-left: 3px solid #6f42c1; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6f42c1;",
            icon("code-branch", style = "font-size: 0.75rem;"), " Comparison Setup"),
          
          selectInput(ns("cnv_comparison_col"), "Compare by column:", choices = NULL),
          div(
            id = ns("cnv_custom_groups"),
            p(class = "text-muted mb-1", style = "font-size: 0.75rem;",
              "Assign each level to a group:"),
            div(class = "mb-2",
                tags$label("Baseline",
                           style = "font-size: 0.78rem; font-weight: 600; color: #0d6efd;"),
                checkboxGroupInput(ns("cnv_baseline"), label = NULL, choices = NULL)),
            div(class = "mb-2",
                tags$label("Comparison",
                           style = "font-size: 0.78rem; font-weight: 600; color: #dc3545;"),
                checkboxGroupInput(ns("cnv_comparison"), label = NULL, choices = NULL))
          )
        ),
        
        # Advanced Options
        div(
          style = "border-left: 3px solid #6c757d; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6c757d;",
            icon("gears", style = "font-size: 0.75rem;"), " Advanced Options"),
          
          div(class = "d-flex align-items-center justify-content-between mb-1",
              tags$label("Include XY", style = "font-size: 0.78rem;",
                         `for` = ns("cnv_include_xy")),
              shinyWidgets::materialSwitch(inputId = ns("cnv_include_xy"),
                                           label   = NULL,
                                           value   = FALSE,
                                           status  = "primary")),
          p(class = "text-muted",
            style = "font-size: 0.7rem; margin-top: -6px; margin-bottom: 10px;",
            "Include X and Y chromosomes in the analysis")
        ),
        
        # Appearance
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("cnv_color_palette"), "Palette:", choices = NULL)
        )
      )
    ),
    
    # ================================================================
    # RIGHT PANEL — tabset with pile-up and per-sample views
    # ================================================================
    div(
      class = "analysis-main",
      
      tabsetPanel(
        id   = ns("cnv_tabset"),
        type = "tabs",
        
        # Tab 1: Pile-up / Summary
        tabPanel(
          title = tagList(icon("chart-bar"), " Pile-up"),
          value = "pileup",
          br(),
          div(
            class = "card p-3 plot-card",
            style = "height: calc(100vh - 200px);",
            plotOutput(ns("cnv_pile_up_plot"), height = "100%")
          )
        ),
        
        # Tab 2: Per-Sample
        tabPanel(
          title = tagList(icon("microscope"), " Per Sample"),
          value = "persample",
          br(),
          div(
            class = "d-flex gap-3",
            style = "align-items: flex-start;",
            
            # Sample selector sidebar (like differential's left panel)
            div(
              style = "width: 280px; min-width: 280px;",
              div(
                class = "card p-3",
                style = "height: calc(100vh - 200px); overflow-y: auto;",
                
                p(class = "text-uppercase fw-bold mb-3",
                  style = "font-size: 0.75rem; letter-spacing: 0.08em; color: #495057;",
                  icon("list", style = "font-size: 0.75rem;"), " Sample Selection"),
                
                p(class = "text-muted mb-2", style = "font-size: 0.75rem;",
                  "Select one sample to display:"),
                
                # Radio buttons for sample selection
                radioButtons(
                  ns("cnv_selected_sample"),
                  label = NULL,
                  choices = c("No samples available" = ""),
                  selected = NULL
                ),
                
                hr(),
              )
            ),
            
            # Per-sample plot area
            div(
              class = "analysis-main",
              div(
                class = "card p-3 plot-card",
                style = "height: calc(100vh - 200px);",
                plotOutput(ns("cnv_per_sample_plot"), height = "100%")
              )
            )
          )
        )
      )
    )
  )
}