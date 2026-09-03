# Module UI function - Improved version matching heatmap aesthetics
differential_met_ui <- function(ns){
  div(
    class = "analysis-row",
    
    # LEFT PANEL
    div(
      class = "analysis-side",
      
      # ---- TOP ACTION BAR (Always visible) ----
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        
        # Run Analysis Button
        div(
          style = "margin-bottom: 12px;",
          actionButton(
            ns("diff_met_run_analysis"),
            " Run Analysis",
            class = "btn btn-primary w-100",
            icon = icon("play"),
            style = "font-weight: bold;"
          )
        ),
        
        # Dynamic Export Buttons (changes based on active tab)
        div(
          style = "border-top: 1px solid #dee2e6; padding-top: 12px;",
          uiOutput(ns("diff_met_export_buttons"))
        )
      ),
      
      # ---- SCROLLABLE PARAMETER PANEL ----
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        # Comparison Setup
        div(
          style = "border-left: 3px solid #6f42c1; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6f42c1;",
            icon("code-branch", style = "font-size: 0.75rem;"), " Comparison Setup"),
          
          selectInput(ns("diff_met_comparison_col"), "Compare by column:", choices = NULL),
          div(
            id = ns("diff_met_custom_groups"),
            p(class = "text-muted mb-1", style = "font-size: 0.75rem;",
              "Assign each level to a group:"),
            div(class = "mb-2",
                tags$label("Baseline",
                           style = "font-size: 0.78rem; font-weight: 600; color: #0d6efd;"),
                checkboxGroupInput(ns("diff_met_baseline"), label = NULL, choices = NULL)),
            div(class = "mb-2",
                tags$label("Comparison",
                           style = "font-size: 0.78rem; font-weight: 600; color: #dc3545;"),
                checkboxGroupInput(ns("diff_met_comparison"), label = NULL, choices = NULL))
          )
        ),
        
        # Statistical Thresholds
        div(
          style = "border-left: 3px solid #dc3545; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #dc3545;",
            icon("sliders", style = "font-size: 0.75rem;"), " Statistical Thresholds"),
          
          tags$label("FDR cutoff", style = "font-size: 0.78rem;"),
          sliderInput(ns("diff_met_fdr_cut"), label = NULL,
                      min = 0.001, max = 0.2, value = 0.05, step = 0.001, ticks = FALSE),
          
          tags$label("logFC cutoff", style = "font-size: 0.78rem;"),
          sliderInput(ns("diff_met_lfc_cut"), label = NULL,
                      min = 0, max = 1, value = 0.2, step = 0.05, ticks = FALSE)
        ),
        
        # Advanced Options
        div(
          style = "border-left: 3px solid #6c757d; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6c757d;",
            icon("gears", style = "font-size: 0.75rem;"), " Advanced Options"),
          
          div(class = "d-flex align-items-center justify-content-between mb-1",
              tags$label("Run ChAMP", style = "font-size: 0.78rem;",
                         `for` = ns("diff_met_run_champ")),
              shinyWidgets::materialSwitch(inputId = ns("diff_met_run_champ"),
                                           label   = NULL,
                                           value   = FALSE,
                                           status  = "primary")),
          p(class = "text-muted",
            style = "font-size: 0.7rem; margin-top: -6px; margin-bottom: 10px;",
            "DMR/DMP via ChAMP (slow)")
        ),
        
        # Appearance
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("diff_met_id_col"), "Sample ID:",  choices = NULL),
          selectInput(ns("diff_met_color_palette"), "Palette:", choices = NULL)
        )
      )
    ),
    
    # ================================================================
    # RIGHT PANEL — tabset results
    # ================================================================
    div(
      class = "analysis-main",
      
      tabsetPanel(
        id   = ns("diff_met_tabset"),
        type = "tabs",
        
        # Density Plot Tab
        tabPanel(
          title = tagList(icon("chart-area"), " Density Plot"),
          value = "density",
          br(),
          div(
            class = "card p-3 plot-card",
            plotOutput(ns("diff_met_density_plot"), height = "100%")
          )
        ),
        
        # DMPs Tab
        tabPanel(
          title = tagList(icon("table"), " DMPs"),
          value = "dmps",
          br(),
          div(
            style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
            p(class = "text-uppercase fw-bold mb-2",
              style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
              icon("filter", style = "font-size: 0.75rem;"), " Top CpGs displayed:"),
            sliderInput(ns("diff_dmps_top_cpgs"), label = NULL,
                        min = 50, max = 50000, value = 1000, step = 50, width = "100%")
          ),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 400px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_dmp_table"), height = "100%")
          )
        ),
        
        # DMRs Tab
        tabPanel(
          title = tagList(icon("table"), " DMRs"),
          value = "dmrs",
          br(),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_dmr_table"), height = "100%")
          )
        ),
        
        # DMGs Tab
        tabPanel(
          title = tagList(icon("table"), " DMGs"),
          value = "dmgs",
          br(),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_dmg_table"), height = "100%")
          )
        ),
        
        # FGSEA - GOBP Tab
        tabPanel(
          title = tagList(icon("dna"), " FGSEA \u2014 GOBP"),
          value = "fgsea_gobp",
          br(),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_fgsea_gobp_table"), height = "100%")
          )
        ),
        
        # FGSEA - KEGG Tab
        tabPanel(
          title = tagList(icon("dna"), " FGSEA \u2014 KEGG"),
          value = "fgsea_kegg",
          br(),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_fgsea_kegg_table"), height = "100%")
          )
        ),
        
        # FGSEA - Hallmark Tab
        tabPanel(
          title = tagList(icon("dna"), " FGSEA \u2014 Hallmark"),
          value = "fgsea_hallmark",
          br(),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_fgsea_hallmark_table"), height = "100%")
          )
        )
      )
    )
  )
}