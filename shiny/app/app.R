source("all_imports.R")
source("modules/load_data/load_data.R")
source("modules/primary_analysis.R")

options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 5 * 1024^3)
options(
  ExperimentHub.cache = "/home/rstudio/.cache/R/ExperimentHub",
  ask = FALSE
)

ui <- navbarPage(
  title = "My Pipeline",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  header = tagList(shinyjs::useShinyjs()),
  
  tabPanel("Load Data", load_data_ui("load_data")),
  
  tabPanel("Primary analysis",
           value = "primary",
           primary_analysis_ui("primary_analysis")
  )
)

server <- function(input, output, session) {
  load_data_return <- load_data_server("load_data")
  primary_analysis_server("primary_analysis", load_data_return = load_data_return)
}

shinyApp(ui, server)
