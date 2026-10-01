# Ceiling of the FDR slider. The DMP fit runs once at this value in the worker so
# that the user's FDR choice is a cheap post-filter; the two must stay in step.
DIFF_FDR_MAX <- 0.2

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
                      min = 0.001, max = DIFF_FDR_MAX, value = 0.05, step = 0.001, ticks = FALSE),
          
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
          
          div(id = ns("diff_met_champ_opt"),
              div(class = "d-flex align-items-center justify-content-between mb-1",
                  tags$label("Run ChAMP", style = "font-size: 0.78rem;",
                             `for` = ns("diff_met_run_champ")),
                  shinyWidgets::materialSwitch(inputId = ns("diff_met_run_champ"),
                                               label   = NULL,
                                               value   = FALSE,
                                               status  = "primary")),
              p(class = "text-muted",
                style = "font-size: 0.7rem; margin-top: -6px; margin-bottom: 10px;",
                "DMR/DMP via ChAMP (slow)")),
          # EPICv2 only: ChAMP does not support it, so DMRs come from DMRcate.
          shinyjs::hidden(div(id = ns("diff_met_dmrcate_opt"),
              div(class = "d-flex align-items-center justify-content-between mb-1",
                  tags$label("Run DMRcate", style = "font-size: 0.78rem;",
                             `for` = ns("diff_met_run_dmrcate")),
                  shinyWidgets::materialSwitch(inputId = ns("diff_met_run_dmrcate"),
                                               label   = NULL,
                                               value   = FALSE,
                                               status  = "primary")),
              p(class = "text-muted",
                style = "font-size: 0.7rem; margin-top: -6px; margin-bottom: 10px;",
                "DMRs via DMRcate, EPICv2 hg38 (slow)")))
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
            numericInput(ns("diff_dmps_top_cpgs"), label = NULL,
                         value = 1000, min = 10, max = 10000, step = 100, width = "100%")
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
          uiOutput(ns("dmg_source_note")),
          div(
            style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
            p(class = "text-uppercase fw-bold mb-2",
              style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
              icon("filter", style = "font-size: 0.75rem;"), " Minimum |logFC| at gene level:"),
            numericInput(ns("diff_met_dmg_lfc_cut"), label = NULL,
                         value = 0, min = 0, max = 1, step = 0.01, width = "100%"),
            p(class = "text-muted mb-0", style = "font-size: 0.72rem;",
              "Gene values are medians over the region's probes, so their logFC is smaller ",
              "than a single CpG's. This threshold is separate from the CpG one in the sidebar.")
          ),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 350px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_dmg_table"), height = "100%")
          )
        ),
        
        # Gene-set enrichment: two methods over the same collections.
        # FGSEA ranks every gene by its promoter-level logFC; missMethyl tests
        # the significant CpGs, correcting for probes per gene. The methods
        # answer different questions, so the choice is a control here rather
        # than two separate tabs a reader might take for the same thing.
        tabPanel(
          title = tagList(icon("dna"), " Gene-set enrichment"),
          value = "enrichment",
          br(),
          tags$style(HTML("
            .m4a-route { border: 1px solid var(--bs-border-color, #dee2e6); border-radius: .5rem;
                         padding: .8rem .95rem; }
            /* Only the two side-by-side cards stretch to match each other. */
            .m4a-route-equal { height: 100%; }
            .m4a-route-on  { border-color: #6f42c1; background: rgba(111,66,193,.05); }
            .m4a-route-off { opacity: .5; }
            .m4a-route-title { font-weight: 600; font-size: .95rem; }
            .m4a-route-sub { font-size: .8rem; color: var(--bs-secondary-color, #6c757d); }
            .m4a-flow { display: flex; flex-wrap: wrap; align-items: center; gap: .3rem; margin: .55rem 0; }
            .m4a-chip { font-size: .78rem; line-height: 1.5; padding: .1rem .55rem; border-radius: 999px;
                        background: rgba(111,66,193,.12); white-space: nowrap; }
            .m4a-route-off .m4a-chip { background: rgba(108,117,125,.15); }
            .m4a-arrow { color: var(--bs-secondary-color, #6c757d); font-size: .8rem; }
            .m4a-route-body { font-size: .86rem; line-height: 1.5; margin: 0; }
            .m4a-foot { font-size: .84rem; font-style: italic; }
            .m4a-gene { display: flex; gap: 2px; align-items: stretch; margin: .15rem 0 .35rem; }
            .m4a-seg { flex-basis: 0; padding: .3rem .25rem; font-size: .72rem; text-align: center;
                       border-radius: .25rem; background: rgba(108,117,125,.12);
                       color: var(--bs-secondary-color, #6c757d); white-space: nowrap;
                       overflow: hidden; text-overflow: ellipsis; }
            .m4a-seg-on { background: rgba(111,66,193,.2); color: var(--bs-body-color, #212529);
                          font-weight: 600; }
            .m4a-ticks { display: flex; gap: 2px; font-size: .68rem;
                         color: var(--bs-secondary-color, #6c757d); }
            .m4a-tick { flex-basis: 0; }
            .m4a-tick-tss { text-align: right; color: #6f42c1; font-weight: 600; }
            @media (max-width: 575px) { .m4a-seg { font-size: .64rem; padding: .3rem .1rem; } }
          ")),
          div(
            class = "d-flex flex-wrap gap-3 align-items-end mb-2",
            selectInput(ns("enr_method"), "CpGs used:",
                        choices = c("All CpGs \u2014 gene medians, FGSEA" = "fgsea",
                                    "Significant CpGs \u2014 missMethyl" = "missmethyl"),
                        selected = "fgsea", width = "290px"),
            selectInput(ns("enr_region"), "Gene region:",
                        choices = c("Promoter" = "promoter",
                                    "Extended promoter" = "promoter1500",
                                    "Gene body" = "body",
                                    "Whole gene" = "all"),
                        selected = "promoter", width = "190px"),
            selectInput(ns("enr_collection"), "Gene sets:",
                        choices = c("GO" = "gobp", "KEGG" = "kegg", "Hallmark" = "hallmark"),
                        selected = "gobp", width = "150px"),
            actionButton(ns("enr_run"), " Run gene-set analysis",
                         class = "btn btn-outline-primary", icon = icon("play")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'missmethyl'", ns("enr_method")),
              div(
                class = "pb-2",
                checkboxInput(ns("gst_sig_genes"),
                              label = "List significant genes per set (slower)",
                              value = FALSE, width = "260px")
              )
            )
          ),
          uiOutput(ns("enr_region_note")),
          uiOutput(ns("enr_explainer")),
          uiOutput(ns("gst_summary")),
          div(
            class = "dt-container",
            style = "width: 100%; height: calc(100vh - 470px); overflow: auto;",
            DT::dataTableOutput(ns("diff_met_enrichment_table"), height = "100%")
          )
        )
      )
    )
  )
}