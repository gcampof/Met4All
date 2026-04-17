# Module UI function
umap_ui <- function(ns) {
  div(
    class = "d-flex gap-3 mt-3",
    style = "align-items: flex-start;",
    
    # LEFT: Parameter panel
    div(
      style = "display: flex; flex-direction: column; gap: 12px; width: var(--param-panel-width); min-width: var(--param-panel-width);",
      
      # Run Analysis button at top
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("play", style = "font-size: 0.75rem;"), " Analysis"),
          actionButton(ns("umap_run_analysis"), "Run UMAP Analysis", 
                       class = "btn btn-primary w-100",
                       icon = icon("play"))
        )
      ),
      
      # Export buttons
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        div(
          style = "border-left: 3px solid #fd7e14; padding-left: 10px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
            icon("download", style = "font-size: 0.75rem;"), " Export Plot"),
          div(class = "d-flex gap-2",
              downloadButton(ns("umap_download_png"), " PNG", class = "btn btn-sm btn-outline-secondary flex-grow-1"),
              downloadButton(ns("umap_download_pdf"), " PDF", class = "btn btn-sm btn-outline-secondary flex-grow-1"))
        )
      ),
      
      # Parameters panel (scrollable)
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        # UMAP Computation
        div(
          style = "border-left: 3px solid #6f42c1; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6f42c1;",
            icon("calculator", style = "font-size: 0.75rem;"), " UMAP Parameters"),
          sliderInput(ns("umap_top_cpgs"), "Top CpGs (MAD):", 
                      min = 1000, max = 30000, value = 10000, step = 1000),
          sliderInput(ns("umap_min_dist"), "Min distance:",  
                      min = 0.01, max = 0.99, value = 0.1, step = 0.01),
          sliderInput(ns("umap_n_neighbors"), "N neighbors:",  
                      min = 2, max = 50, value = 15, step = 1),
          selectInput(ns("umap_metric"), "Distance metric:", 
                      choices = c("euclidean", "manhattan", "cosine")),
          numericInput(ns("umap_seed"), "Random seed:", 
                       value = 42, min = 1, max = 99999, step = 1)
        ),
        
        # Clustering
        div(
          style = "border-left: 3px solid #20c997; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #20c997;",
            icon("sitemap", style = "font-size: 0.75rem;"), " Clustering"),
          sliderInput(ns("umap_knn"), "KNN:", 
                      min = 2, max = 50, value = 7, step = 1),
          sliderInput(ns("umap_consensus_k_max"), "K max:",  
                      min = 2, max = 50, value = 20, step = 1)
        ),
        
        # Appearance
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("umap_id_col"), "Sample ID:", choices = NULL),
          selectInput(ns("umap_color_by"), "Color by:", choices = NULL),
          selectInput(ns("umap_legend_position"), "Legend position:",
                      choices = c("right", "left", "top", "bottom", "none"),
                      selected = "right"),
          selectInput(ns("umap_color_palette"), "Palette:", choices = NULL),
          div(
            class = "d-flex flex-column gap-2 mt-1",
            div(
              class = "form-check form-switch",
              tags$input(class = "form-check-input", type = "checkbox", role = "switch",
                         id = ns("umap_show_summary"), checked = TRUE),
              tags$label(class = "form-check-label small", `for` = ns("umap_show_summary"), "Show summary")
            ),
            div(
              class = "form-check form-switch",
              tags$input(class = "form-check-input", type = "checkbox", role = "switch",
                         id = ns("umap_show_labels")),
              tags$label(class = "form-check-label small", `for` = ns("umap_show_labels"), "Show sample labels")
            )
          )
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
        plotOutput(ns("umap_plot"), height = "100%",  width = "100%")     
      )
    )
  )
}