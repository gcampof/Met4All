samplesheet_ui <- function(ns) {
  div(
    class = "mt-3",

    # ---- ADD METADATA FROM A NEW SAMPLESHEET ----
    div(
      class = "card p-3 mb-3",
      p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #0d6efd;",
        icon("file-circle-plus", style = "font-size: 0.75rem;"), " Add metadata"),
      p(class = "text-muted", style = "font-size: 0.85rem;",
        "Upload a samplesheet with extra columns to add them to the current one. ",
        "It needs at least one column (e.g. Sample_Name or ID) whose values match the samples below. ",
        "Samples not present in the uploaded file keep their current values."),

      div(
        class = "d-flex gap-3 align-items-end flex-wrap",
        div(
          style = "flex: 1 1 300px;",
          fileInput(ns("samplesheet_upload"), NULL,
                    accept = c(".csv", ".xlsx"), buttonLabel = "Browse",
                    placeholder = "No samplesheet selected", width = "100%")
        ),
        div(
          style = "flex: 0 1 220px;",
          selectInput(ns("samplesheet_key_col"), "Match samples by:", choices = NULL, width = "100%")
        ),
        div(
          class = "mb-3",
          checkboxInput(ns("samplesheet_overwrite"), "Overwrite existing columns", value = FALSE)
        ),
        div(
          class = "mb-3",
          actionButton(ns("samplesheet_apply"), " Add columns",
                       class = "btn btn-primary", icon = icon("plus"))
        )
      ),
      uiOutput(ns("samplesheet_upload_status"))
    ),

    p(style = "font-size: 0.85rem;",
    "Click any cell to edit it. Changes are applied immediately to the samplesheet used by all analyses."),
    DT::dataTableOutput(ns("samplesheet_table"))
  )
}
