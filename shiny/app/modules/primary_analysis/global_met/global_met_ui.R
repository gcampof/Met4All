# Module UI function
global_met_ui <- function(ns) {
  div(
    class = "d-flex gap-3 mt-3",
    style = "align-items: flex-start;",
    
    # ---- LEFT PANEL ----
    div(
      style = "display: flex; flex-direction: column; gap: 12px; width: var(--param-panel-width); min-width: var(--param-panel-width);",
      
      # ---- TOP ACTION BAR ----
      div(
        class = "card p-3",
        style = "flex-shrink: 0;",
        
        # Run Analysis Button
        div(
          style = "margin-bottom: 12px;",
          actionButton(
            ns("global_met_run_analysis"),
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
            downloadButton(ns("global_met_download_png"), " PNG",
                           class = "btn btn-sm btn-outline-secondary flex-grow-1"),
            downloadButton(ns("global_met_download_pdf"), " PDF",
                           class = "btn btn-sm btn-outline-secondary flex-grow-1")
          )
        )
      ),
      
      # ---- SCROLLABLE PARAMETER PANEL ----
      div(
        class = "card p-3 param-panel",
        style = "flex-grow: 1; overflow-y: auto;",
        
        # Sample Setup
        div(
          style = "border-left: 3px solid #0d6efd; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2 mt-1",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
            icon("users", style = "font-size: 0.75rem;"), " Sample Setup"),
          selectInput(ns("global_met_id_col"), "Sample ID:", choices = NULL),
        ),
        
        # Comparison Setup
        div(
          style = "border-left: 3px solid #6f42c1; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #6f42c1;",
            icon("code-branch", style = "font-size: 0.75rem;"), " Comparison Setup"),
          
          selectInput(ns("global_met_comparison_col"), "Compare by column:", choices = NULL),
          
          radioButtons(
            ns("global_met_comparison_type"),
            label = NULL,
            choices = c("Between all groups" = "between", "Custom two-group" = "custom"),
            selected = "between"
          ),
          
          # Custom two-group builder — only shown when custom is selected
          shinyjs::hidden(
            div(
              id = ns("global_met_custom_groups"),
              
              p(class = "text-muted mb-1", style = "font-size: 0.75rem;",
                "Assign each level to a group:"),
              
              # Group 1
              div(
                class = "mb-2",
                tags$label("Group 1", style = "font-size: 0.78rem; font-weight: 600; color: #0d6efd;"),
                checkboxGroupInput(ns("global_met_group1"), label = NULL, choices = NULL)
              ),
              
              # Group 2
              div(
                class = "mb-2",
                tags$label("Group 2", style = "font-size: 0.78rem; font-weight: 600; color: #dc3545;"),
                checkboxGroupInput(ns("global_met_group2"), label = NULL, choices = NULL)
              )
            )
          )
        ),
        
        # Appearance
        div(
          style = "border-left: 3px solid #198754; padding-left: 10px; margin-bottom: 12px;",
          p(class = "text-uppercase fw-bold mb-2",
            style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #198754;",
            icon("palette", style = "font-size: 0.75rem;"), " Appearance"),
          selectInput(ns("global_met_color_palette"), "Palette:", choices = NULL),
          selectInput(ns("global_met_legend_position"), "Legend:", 
                      choices = c("bottom", "top", "left", "right"), selected = "right")
        )
      )
    ),
    
    # ---- RIGHT: Plot ----
    div(
      class = "flex-grow-1",
      style = "min-width: 0;",
      div(
        class = "card p-3 plot-card",
        style = "height: 100%;",
        plotOutput(ns("global_met_plot"), height = "100%", width = "100%")
      )
    )
  )
}