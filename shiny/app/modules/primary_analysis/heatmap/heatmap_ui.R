# Module UI function
heatmap_ui <- function(ns){
  div(
    class = "d-flex gap-3 mt-3",
    style = "align-items: flex-start;",
    
    # ---- LEFT PANEL ----
    div(
      style = "display: flex; flex-direction: column; gap: 12px; width: var(--param-panel-width); min-width: var(--param-panel-width);",
      
      # ---- TOP ACTION BAR  ----
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        
        # Run Analysis Button
        div(
          style = "margin-bottom: 12px;",
          actionButton(
            ns("heatmap_run_analysis"),
            " Run Analysis",
            class = "btn btn-primary w-100",
            icon = icon("play"),
            style = "font-weight: bold;"
          )
        ),
        
        # Export Buttons
        div(
          style = "border-top: 1px solid #dee2e6; padding-top: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
            icon("download", style = "font-size: 0.75rem;"), " Export"),
          div(
            class = "d-flex gap-2",
            downloadButton(ns("heatmap_download_png"), " PNG",
                           class = "btn btn-sm btn-outline-secondary flex-grow-1"),
            downloadButton(ns("heatmap_download_pdf"), " PDF",
                           class = "btn btn-sm btn-outline-secondary flex-grow-1"),
            downloadButton(ns("heatmap_download_consensus"), 
                           "TSV (CC Clust)",
                           class = "btn btn-sm btn-outline-secondary"
            )
          )
        )
      ),
      
      # ---- SCROLLABLE PARAMETER PANEL ----
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        # CpG Selection
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("dna", style = "font-size: 0.75rem;"), " CpG Selection"),
          sliderInput(ns("heatmap_top"), "Top CpGs:", min = 0, max = 1000, value = 250, step = 25),
          selectInput(ns("heatmap_annotation_cols"), "Annotation columns:", choices = NULL)
        ),
        
        # Clustering
        div(
          style = "border-left: 3px solid #6f42c1; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6f42c1;",
            icon("sitemap", style = "font-size: 0.75rem;"), " Clustering"),
          sliderInput(ns("heatmap_row_k"), "Row K:", min = 1, max = 15, value = 5, step = 1),
          sliderInput(ns("heatmap_col_k"), "Col K:", min = 1, max = 15, value = 7, step = 1),
          sliderInput(ns("heatmap_cc_kmax"), "Max K (cc_kmax):", min = 2, max = 20, value = 9, step = 1),
          sliderInput(ns("heatmap_cc_reps"), "Repetitions (cc_reps):", min = 50, max = 2000, value = 500, step = 50),
          sliderInput(ns("heatmap_cc_pItem"), "Item sampling (cc_pItem):", min = 0.5, max = 1, value = 0.8, step = 0.05),
          numericInput(ns("heatmap_cc_seed"), "Random seed:", value = 42, min = 1, max = 99999, step = 1)
        ),
        
        # Appearance
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("heatmap_id_col"), "Sample ID:", choices = NULL),
          selectInput(ns("heatmap_color_palette"), "Palette:", choices = NULL),
          selectInput(ns("heatmap_legend_position"), "Legend:",     
                      choices = c("bottom", "top", "left", "right"), selected = "right"),
          selectInput(ns("heatmap_annotation_legend_position"), "Annotation Legend:",     
                      choices = c("bottom", "top", "left", "right"), selected = "bottom"),
          div(
            class = "form-check form-switch",
            tags$input(class = "form-check-input", type = "checkbox", role = "switch",
                       id = ns("heatmap_show_row_names")),
            tags$label(class = "form-check-label small", `for` = ns("heatmap_show_row_names"), "Show row names")
          ),
          div(
            class = "form-check form-switch",
            tags$input(class = "form-check-input", type = "checkbox", role = "switch",
                       id = ns("heatmap_show_col_names")),
            tags$label(class = "form-check-label small", `for` = ns("heatmap_show_col_names"), "Show col names")
          )
        )
      )
    ),
    
    # ---- RIGHT: Plot ----
    div(
      class = "flex-grow-1",
      div(
        class = "card p-3 plot-card",
        plotOutput(ns("heatmap_plot"), height = "100%", width = "100%")
      )
    )
  )
}