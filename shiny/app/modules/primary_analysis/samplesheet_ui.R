samplesheet_ui <- function(ns) {
  div(
    class = "mt-3",
    p(style = "font-size: 0.85rem;",
    "Click any cell to edit it. Changes are applied immediately to the samplesheet used by all analyses."),
    DT::dataTableOutput(ns("samplesheet_table"))
  )
}