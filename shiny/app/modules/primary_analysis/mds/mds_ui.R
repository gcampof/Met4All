# Module UI function
mds_ui <- function(ns) {
  div(
    class = "d-flex gap-3 mt-3",
    style = "align-items: flex-start;",
    
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
            ns("mds_run_analysis"),
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
            downloadButton(ns("mds_download_png"), " PNG",class = "btn btn-sm btn-outline-secondary flex-grow-1"),
            downloadButton(ns("mds_download_pdf"), " PDF", class = "btn btn-sm btn-outline-secondary flex-grow-1"),
          )
        )
      ),
      
      # Parameters panel
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("calculator", style = "font-size: 0.75rem;"), " MDS Parameters"),
          sliderInput(ns("mds_top_cpgs"), "Top CpGs (MAD):", min = 1000, max = 30000, value = 10000, step = 1000)
        ),
        
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("mds_id_col"), "Sample ID:", choices = NULL),
          selectInput(ns("mds_color_by"), "Color by:", choices = NULL),
          selectInput(ns("mds_color_palette"), "Palette:", choices = NULL)
        )
      )
    ),
    
    # RIGHT: Plot
    div(
      class = "flex-grow-1",
      style = "min-width: 0;",
      div(
        class = "card p-3 plot-card",
        style = "height: 100%;",
        plotOutput(ns("mds_plot"), height = "100%",  width = "100%")     
      )
    )
  )
}