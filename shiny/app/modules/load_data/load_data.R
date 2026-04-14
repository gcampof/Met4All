source("modules/load_data/load_data_helper.R")

load_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Custom CSS for better styling
    tags$style(HTML("
      .load-data-container {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        min-height: calc(100vh - 100px);
      }
      .load-card {
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        cursor: pointer;
      }
      .load-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 30px rgba(0,0,0,0.15) !important;
      }
      .load-card.selected {
        border: 2px solid #28a745 !important;
        background-color: #f0fff4 !important;
      }
      .step-indicator {
        display: flex;
        justify-content: center;
        margin-bottom: 30px;
      }
      .step {
        text-align: center;
        flex: 1;
        position: relative;
      }
      .step-number {
        width: 40px;
        height: 40px;
        background: #dee2e6;
        border-radius: 50%;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .step.active .step-number {
        background: #0d6efd;
        color: white;
      }
      .step.completed .step-number {
        background: #28a745;
        color: white;
      }
      .step-label {
        font-size: 14px;
        color: #6c757d;
      }
      .step.active .step-label {
        color: #0d6efd;
        font-weight: bold;
      }
      .step.completed .step-label {
        color: #28a745;
      }
      .step:not(:last-child):before {
        content: '';
        position: absolute;
        top: 20px;
        left: 50%;
        width: 100%;
        height: 2px;
        background: #dee2e6;
        z-index: -1;
      }
      .step.completed:not(:last-child):before {
        background: #28a745;
      }
      .console-log {
        background: #1e1e1e;
        color: #d4d4d4;
        font-family: 'Courier New', monospace;
        font-size: 12px;
        border-radius: 8px;
        text-align: left;
      }
    ")),
    
    div(
      class = "load-data-container p-5",
      
      # Title
      div(
        class = "text-center mb-5",
        h2(icon("database"), " Load Your Data", class = "mb-2"),
        p("Upload methylation data to begin analysis", class = "text-muted")
      ),
      
      # Step indicators
      div(
        class = "step-indicator mb-5",
        div(
          class = "step active",
          div(class = "step-number", "1"),
          div(class = "step-label", "Select Type")
        ),
        div(
          id = ns("step2"),
          class = "step",
          div(class = "step-number", "2"),
          div(class = "step-label", "Upload Files")
        ),
        div(
          id = ns("step3"),
          class = "step",
          div(class = "step-number", "3"),
          div(class = "step-label", "Process Data")
        )
      ),
      
      # Message Alerts
      uiOutput(ns("alert_message")),
      
      # Main view container
      div(
        id = ns("ld_main_view"),
        class = "card shadow-lg",
        style = "max-width: 900px; margin: 0 auto; border-radius: 12px;",
        
        div(
          class = "card-body p-5",
          
          p("Select the type of files you want to load:", 
            class = "text-center text-muted mb-4"),
          
          # Selection cards
          div(
            class = "d-flex justify-content-center gap-4 mb-5",
            style = "flex-wrap: wrap;",
            
            # Beta Matrix card
            div(
              id = ns("card_beta"),
              class = "load-card card text-center p-4",
              style = "width: 280px; cursor: pointer; border-radius: 12px;",
              onclick = sprintf("Shiny.setInputValue('%s', 'beta', {priority: 'event'})", ns("select_type")),
              div(
                class = "card-body",
                icon("table", class = "fa-4x mb-3", style = "color: #0d6efd;"),
                h4("Beta Matrix", class = "card-title mb-2"),
                p("Upload pre-computed beta matrices", class = "text-muted small"),
                p("+ Targets file", class = "text-muted small mt-2")
              )
            ),
            
            # IDATs card
            div(
              id = ns("card_idats"),
              class = "load-card card text-center p-4",
              style = "width: 280px; cursor: pointer; border-radius: 12px;",
              onclick = sprintf("Shiny.setInputValue('%s', 'idats', {priority: 'event'})", ns("select_type")),
              div(
                class = "card-body",
                icon("microchip", class = "fa-4x mb-3", style = "color: #6f42c1;"),
                h4("IDAT Files", class = "card-title mb-2"),
                p("Upload raw IDAT files", class = "text-muted small"),
                p("+ Sample sheet", class = "text-muted small mt-2")
              )
            )
          ),
          
          # File upload section
          div(
            id = ns("upload_section"),
            class = "text-center",
            style = "display: none;",
            div(
              class = "border-top pt-4",
              h5(icon("upload"), " Upload ZIP files", class = "mb-3"),
              div(
                style = "display: flex; justify-content: center; width: 100%;",
                div(
                  style = "width: auto;",
                  uiOutput(ns('zipfile_ui'))
                )
              ),
              p(class = "text-muted small mt-2", 
                "Supported formats: .zip files containing either:",
                tags$br(),
                "- For Beta mode: CSV files with beta values + targets file",
                tags$br(),
                "- For IDATs mode: Raw IDAT files + sample sheet"
              )
            )
          ),
          
          # Load Data button
          div(
            class = "text-center mt-4",
            actionButton(
              inputId = ns("confirm_load"),
              label = tagList(icon("play"), " Load Data"),
              class = "btn btn-success btn-lg px-5",
              style = "border-radius: 25px; font-weight: bold;",
              disabled = TRUE
            )
          )
        )
      ),
      
      # IDATS View (hidden initially)
      shinyjs::hidden(
        div(
          id = ns("ld_idats_view"),
          class = "card shadow-lg",
          style = "max-width: 1200px; margin: 0 auto; border-radius: 12px;",
          
          div(
            class = "card-body p-4",
            h3(icon("microchip"), " Detected IDAT Samples", class = "mb-4"),
            
            # Select all checkbox
            div(
              class = "mb-3",
              checkboxInput(ns("select_all"), "Select all samples", value = FALSE)
            ),
            
            # DataTable
            DTOutput(ns("idat_table")),
            br(),
            
            # Configuration panel
            div(
              class = "bg-light p-4 rounded",
              h5(icon("sliders-h"), " Configuration", class = "mb-3"),
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    ns("normalization"), 
                    "Normalization method:", 
                    choices = c("ssnoob", "raw", "illumina", "quantile", "funnorm"), 
                    selected = "ssnoob"
                  )
                ),
                column(
                  width = 6,
                  uiOutput(ns("cpu_selector"))
                )
              )
            ),
            br(),
            
            # Run QC button
            div(
              class = "text-center",
              actionButton(
                ns("run_qc"), 
                tagList(icon("chart-line"), " Run QC"), 
                class = "btn btn-primary btn-lg px-5",
                style = "border-radius: 25px; font-weight: bold;"
              )
            )
          )
        )
      ),
      
      # QC VIEW (hidden initially)
      shinyjs::hidden(
        div(
          id = ns("ld_qc_view"),
          class = "card shadow-lg",
          style = "border-radius: 12px;",
          
          div(
            class = "card-body p-4",
            h3(icon("check-circle"), " Quality Control", class = "mb-4"),
            uiOutput(ns("qc_threshold_tabs")),
            br(),
            div(
              class = "text-center",
              actionButton(
                ns("qc_continue"),
                tagList(icon("arrow-right"), " Generate Beta Matrix"),
                class = "btn btn-success btn-lg px-5",
                style = "border-radius: 25px; font-weight: bold;"
              )
            )
          )
        )
      ),
      
      # LOADING View
      shinyjs::hidden(
        div(
          id = ns("ld_loading_view"),
          class = "card shadow-lg text-center p-5",
          style = "max-width: 800px; margin: 0 auto; border-radius: 12px;",
          
          div(
            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 400px;",
            
            div(
              class = "spinner-border text-primary",
              role = "status",
              style = "width: 4rem; height: 4rem; margin-bottom: 20px;"
            ),
            h4("Processing...", class = "mt-3 mb-3")
          )
        )
      )
    )
  )
}


# --- SERVER ---
load_data_server <- function(id, DIRS, cfg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- REACTIVE VALUES ---
    samples_df <- reactiveVal(data.frame())
    type_selected <- reactiveVal(NULL)
    alert_message <- reactiveVal(NULL)
    
    # QC reactive values
    qc_results <- reactiveVal(list())
    array_names <- reactiveVal(list())
    
    # Beta reactive values
    mSetSq_list <- reactiveVal(list())
    beta_merged <- reactiveVal(NULL)
    targets_merged <- reactiveVal(NULL)
    
    # --- RENDER ZIP FILE ---
    output$zipfile_ui <- renderUI({
      fileInput(
        inputId = ns("zipfile"),
        label = "Upload your ZIP files",
        accept = ".zip",
        multiple = TRUE,
        width = "280px"
      )
    })
    
    # --- ALERT MESSAGE ---
    output$alert_message <- renderUI({
      msg <- alert_message()
      if (is.null(msg)) return(NULL)
      alert_class <- if (msg$type == "error") "alert-danger" else "alert-warning"
      div(
        class = paste("alert", alert_class, "alert-dismissible fade show"),
        role = "alert",
        msg$text,
      )
    })
    
    # Handle card selection
    observeEvent(input$select_type, {
      if (input$select_type == "beta") {
        type_selected("BETA")
        shinyjs::removeClass("card_beta", "selected")
        shinyjs::addClass("card_beta", "selected")
        shinyjs::removeClass("card_idats", "selected")
      } else if (input$select_type == "idats") {
        type_selected("IDATS")
        shinyjs::removeClass("card_idats", "selected")
        shinyjs::addClass("card_idats", "selected")
        shinyjs::removeClass("card_beta", "selected")
      }
      
      # Show upload section
      shinyjs::show("upload_section")
      
      # Update step indicators
      shinyjs::addClass("step2", "active")
      shinyjs::removeClass("step", "active", selector = ".step:first-child")
    })
    
    
    # --- IDAT TABLE ---
    output$idat_table <- DT::renderDataTable({
      req(samples_df())
      datatable(
        samples_df(),
        selection = list(mode = "multiple"),
        filter = "top",
        options = list(pageLength = 10)
      )
    })
    
    # --- TYPE SELECTOR ---
    observeEvent(input$load_beta, {
      type_selected("BETA")
      alert_message(NULL)
      shinyjs::removeClass("load_beta", "btn-primary")
      shinyjs::addClass("load_beta", "btn-success")
      shinyjs::removeClass("load_idats", "btn-success")
      shinyjs::addClass("load_idats", "btn-primary")
    })
    
    observeEvent(input$load_idats, {
      type_selected("IDATS")
      alert_message(NULL)
      shinyjs::removeClass("load_idats", "btn-primary")
      shinyjs::addClass("load_idats", "btn-success")
      shinyjs::removeClass("load_beta", "btn-success")
      shinyjs::addClass("load_beta", "btn-primary")
    })
    
    # --- ENABLE LOAD DATA BUTTON ---
    observe({
      type_ok <- !is.null(type_selected())
      zip_ok <- !is.null(input$zipfile) && nrow(input$zipfile) > 0
      
      if (type_ok && zip_ok) {
        shinyjs::enable("confirm_load")
        shinyjs::addClass("confirm_load", "btn-success")
        shinyjs::removeClass("confirm_load", "btn-secondary")
      } else {
        shinyjs::disable("confirm_load")
        shinyjs::removeClass("confirm_load", "btn-success")
        shinyjs::addClass("confirm_load", "btn-secondary")
      }
    })
    
    # --- FILE UPLOAD ---
    observeEvent(input$confirm_load, {
      # Show loading view
      shinyjs::hide("ld_main_view")
      shinyjs::show("ld_loading_view")
      
      tryCatch({
        notification_id <- showNotification("Extracting files from zip...", type="message", duration=0)
        zip_paths <- input$zipfile$datapath
        archive::archive_extract(zip_paths, dir = DIRS$input)
        
        if (type_selected() == "IDATS") {
          removeNotification(notification_id)
          notification_id <- showNotification("Organizing IDAT files...", type="message", duration=0)

          # Create idats folder and copy all IDATs there
          idats_dir <- parse_idat_files(DIRS$input, DIRS$preprocessing)
          
          removeNotification(notification_id)
          notification_id <- showNotification("Classifying by array type...", type="message", duration=0)

          # Now organize by array type
          order_idat_per_array(idats_dir, DIRS$preprocessing)
          
          removeNotification(notification_id)
          notification_id <- showNotification("Generating sample summary...", type="message", duration=0)

          temp_df <- generate_idat_dataframe(DIRS$preprocessing)
          
          if (nrow(temp_df) == 0) {
            stop("No valid IDAT samples detected after classification")
          }
          
          samples_df(temp_df)
          
          removeNotification(notification_id)
          notification_id <- showNotification("Complete!", type="message", duration=3)
          
        } else if (type_selected() == "BETA") {
          removeNotification(notification_id)
          notification_id <- showNotification("Extracting BETA files...", type="message", duration=0)
          beta_and_targets <- extract_beta_and_targets(DIRS$input, DIRS$beta, notification_id)
          beta_merged(beta_and_targets$beta)
          targets_merged(beta_and_targets$targets)
          
          notification_id <- showNotification("Complete!", type="message", duration=3)
          
          shinyjs::hide("ld_loading_view")
          updateNavbarPage(session, "main_navbar", selected = "primary")
          
          # Return only using beta and targets
          return(list(
            array_names_ld = array_names,
            mSetSq_list_ld = mSetSq_list,
            beta_merged_ld= beta_merged,
            targets_merged_ld = targets_merged
          ))
        }
        
        # Only hide loading and show next view on success
        if (type_selected() == "IDATS") {
          shinyjs::hide("ld_loading_view")
          shinyjs::show("ld_idats_view")
        }
        
      }, error = function(e) {
        message("Error during data loading: ", e$message)
        
        # Clean all files inside input
        if (!is.null(DIRS$input) && dir.exists(DIRS$input)) {
          tryCatch({
            unlink(file.path(DIRS$input, "*"), recursive = TRUE, force = TRUE)
            message("DIRS$input cleaned successfully")
          }, warning = function(w) {
            message("Warning while cleaning DIRS$input: ", w$message)
          }, error = function(e2) {
            message("Failed to clean DIRS$input: ", e2$message)
          })
        }
        
        # Reset UI to main view
        shinyjs::hide("ld_loading_view")
        shinyjs::hide("ld_idats_view")
        shinyjs::hide("ld_qc_view")
        shinyjs::show("ld_main_view")
        
        # Reset type selection
        type_selected(NULL)
        shinyjs::removeClass("card_beta", "selected")
        shinyjs::removeClass("card_idats", "selected")
        shinyjs::hide("upload_section")
        
        # Reset file input
        output$zipfile_ui <- renderUI({
          fileInput(
            inputId = ns("zipfile"),
            label = "Upload your ZIP files",
            accept = ".zip",
            multiple = TRUE,
            width = "280px"
          )
        })
        
        # Show error message
        alert_message(list(
          type = "error",
          text = paste("Processing failed:", e$message)
        ))
      })
    })
    
    # --- SELECT ALL ---
    observeEvent(input$select_all, {
      df <- samples_df()
      if (nrow(df) == 0) return()
      
      proxy <- DT::dataTableProxy("idat_table", session)
      
      if (isTRUE(input$select_all)) {
        proxy %>% DT::selectRows(seq_len(nrow(df)))
      } else {
        proxy %>% DT::selectRows(NULL)
      }
    })
    
    
    # --- RUN QC ---
    observeEvent(input$run_qc, {
      shinyjs::hide("ld_idats_view")
      shinyjs::show("ld_loading_view")
        tryCatch({
          notification_id <- showNotification("Separating unused IDATs...", type="message", duration=0)
          selected_idats <- input$idat_table_rows_selected
          separate_unselected_idats(samples_df(), selected_idats, DIRS$preprocessing)
          
          removeNotification(notification_id)
          notification_id <- showNotification("Loading Sample Sheet...", type="message", duration=0)
          parse_samplesheets(DIRS$input, DIRS$preprocessing)
          
          # Mid processing clean up
          unlink(DIRS$input)
          
          # Run QC loading
          qc_res_temp <- load_qc_data_for_arrays_batch(DIRS$preprocessing, DIRS$qc)
          
          qc_results(qc_res_temp$qc_results)
          array_names(qc_res_temp$arrays_used)
          
          notification_id <- showNotification("Complete!", type="message", duration=3)
          shinyjs::hide("ld_loading_view")
          shinyjs::show("ld_qc_view")
        }, error = function(e) {
          alert_message(list(
            type = "error",
            text = paste("Running QC failed:", e$message)
          ))
        })
    })
    
    
    
    
    # --- QC TABS VIEW UI ---
    output$qc_threshold_tabs <- renderUI({
      req(array_names())
      
      tabs <- lapply(array_names(), function(arr) {
        tabPanel(
          title = arr,
          br(),
          
          radioButtons(
            ns(paste0("ld_qc_threshold_", arr)),
            "Probe detection-P threshold:",
            choices = c(
              "0.01 (strict)" = 0.01,
              "0.05 (default)" = 0.05,
              "0.10 (relaxed)" = 0.10
            ),
            selected = 0.05,
            inline = TRUE
          ),
          
          fluidRow(
            column(
              width = 9,
              plotOutput(  # Changed from plotlyOutput to plotOutput
                outputId = ns(paste0("ld_qc_barplot_", arr)),
                height = "600px"
              )
            ),
            column(
              width = 3,
              uiOutput(ns(paste0("ld_qc_stats_", arr)))
            )
          )
        )
      })
      
      do.call(
        tabsetPanel,
        c(
          list(
            id = ns("qc_threshold_tabset"),
            type = "tabs"
          ),
          tabs
        )
      )
    })
    
    
    # --- QC TABS VIEW LOGIC (Load on demand) ---
    # Create reactive values to store loaded data for each array
    loaded_qc_data <- reactiveValues()
    
    observe({
      req(qc_results(), array_names())
      
      # Observe tab changes
      observeEvent(input$qc_threshold_tabset, {
        current_array <- input$qc_threshold_tabset
        req(current_array)
        
        # Check if this array's data is already loaded
        if (is.null(loaded_qc_data[[current_array]])) {
          message("Loading ", current_array, " data from disk...")
          notification_id <- showNotification(paste0("Loading ", current_array, " data from disk..."), type = "message", duration = 0)
          
          # Load from disk
          rgset_path <- qc_results()$rgsets[[current_array]]
          detp_path <- qc_results()$detections[[current_array]]
          
          full_rgSet <- readRDS(rgset_path)
          full_detP <- readRDS(detp_path)
          
          # Calculate mean detection p-values for all samples
          mean_detP <- colMeans(full_detP, na.rm = TRUE)
          
          # Get indices of top 100 samples with highest mean detection p-values
          top_n <- min(100, ncol(full_detP))
          top_indices <- order(mean_detP, decreasing = TRUE)[1:top_n]
          
          # Subset both objects to only keep the top 50 samples
          rgSet_subset <- full_rgSet[, top_indices]
          detP_subset <- full_detP[, top_indices]
          
          # Store ONLY the subsetted data in reactiveValues
          loaded_qc_data[[current_array]] <- list(
            rgSet = rgSet_subset,
            detP = detP_subset,
            total_samples = ncol(full_rgSet),  # Store total count for reference
            kept_samples = top_n
          )
          
          # Clean up full objects to free memory immediately
          rm(full_rgSet, full_detP, mean_detP)
          gc()
          
          removeNotification(notification_id)
          showNotification("Data loaded", type = "message", duration = 2)
          message("Loaded and subsetted ", current_array, " - RGSet size: ", 
                  format(object.size(loaded_qc_data[[current_array]]$rgSet), units = "auto"),
                  " (kept ", top_n, " of ", loaded_qc_data[[current_array]]$total_samples, " samples)")
        }
        
        # Get the data for current array
        current_data <- loaded_qc_data[[current_array]]
        
        # Plot output
        plot_id <- paste0("ld_qc_barplot_", current_array)
        output[[plot_id]] <- renderPlot({
          thr_id <- paste0("ld_qc_threshold_", current_array)
          req(input[[thr_id]], current_data$rgSet, current_data$detP)
          
          thr <- as.numeric(input[[thr_id]])
          
          generate_detection_p_barplot_subset(
            array     = current_array,
            rgSet     = current_data$rgSet,
            detP      = current_data$detP,
            threshold = thr,
            total_samples = current_data$total_samples,
            kept_samples = current_data$kept_samples
          )
        })
        
        # Stats output
        stats_id <- paste0("ld_qc_stats_", current_array)
        output[[stats_id]] <- renderUI({
          thr_id <- paste0("ld_qc_threshold_", current_array)
          req(input[[thr_id]], current_data$detP)
          
          thr <- as.numeric(input[[thr_id]])
          
          failed_perc <- colSums(current_data$detP > thr) / nrow(current_data$detP) * 100
          keep <- colMeans(current_data$detP) < thr
          
          tagList(
            div(
              class = "border rounded p-3 bg-light",
              
              h5("QC summary"),
              
              p(strong("Threshold: "), thr),
              p(
                strong("Mean failed probes (%): "),
                sprintf("%.2f", mean(failed_perc))
              ),
              p(
                strong("Max failed probes (%): "),
                sprintf("%.2f", max(failed_perc))
              ),
              p(
                strong("Samples failing QC (in top 100): "),
                sum(!keep), " / ", length(keep)
              ),
              hr(),
              p(
                em(sprintf("Showing top %d samples with highest mean detection p-values (out of %d total samples)", 
                           current_data$kept_samples, current_data$total_samples))
              )
            )
          )  
        })
      })
    })
    
    
    # --- QC CONTINUE ---
    observeEvent(input$qc_continue, {
      shinyjs::hide("ld_qc_view")
      shinyjs::show("ld_loading_view")
      
      arrays <- array_names()
      
      # Store paths to beta matrices instead of objects
      beta_paths <- c()
      processed_results_paths <- list()
      
      tryCatch({
        for (array in arrays) {
          message("\n=== Processing beta for ", array, " ===")
          
          array_qc_dir <- file.path(DIRS$qc, array)
          
          # Get threshold selected for THIS array
          thr_id <- paste0("ld_qc_threshold_", array)
          thr <- as.numeric(input[[thr_id]])
          
          # LOAD FROM DISK instead of using stored objects
          rgset_path <- qc_results()$rgsets[[array]]
          detp_path <- qc_results()$detections[[array]]
          
          notification_id <- showNotification("Loading RGset and DetP...", type = "message", duration = 0)
        
          message("Loading from disk: ", basename(rgset_path))
          rgSet <- readRDS(rgset_path)
          detP <- readRDS(detp_path)
          
          message("Loaded RGSet size: ", format(object.size(rgSet), units = "auto"))
          
          # Save QC files (percentage failed, plot)
          failed_perc <- colSums(detP > thr) / nrow(detP) * 100
          
          csv_file <- file.path(
            array_qc_dir,
            sprintf("1.1-Percentage_of_failed_probes_by_sample_detection_p_%.2f_%s.csv",
                    thr, array)
          )
          write.csv(failed_perc, file = csv_file, quote = FALSE, row.names = TRUE)
 
          
          # Generate and save QC plot
          p <- generate_detection_p_barplot(
            array     = array,
            rgSet     = rgSet,
            detP      = detP,
            threshold = thr
          )
          
          plot_file <- file.path(
            array_qc_dir,
            sprintf("1.0-Detection_P_barplot_threshold_%.2f_%s.png", thr, array)
          )
          ggsave(filename = plot_file, plot = p, width = 12, height = 6, dpi = 300)
          rm(p, failed_perc)
          
          # Generate Beta matrix
          message("Generating beta matrix for ", array, "...")
          removeNotification(notification_id)
          notification_id <- showNotification(paste0("Generating beta matrix for ",
                                                     array, "..."), type = "message", duration = 2)
          result_paths <- generate_beta_matrix(
            array       = array,
            rgSet       = rgSet,
            detP        = detP,
            norm_method = input$normalization,
            threshold   = thr,
            filter_dir  = DIRS$filter,
            beta_dir    = DIRS$beta
          )
          
          # Store the paths
          beta_paths <- c(beta_paths, result_paths$beta_path)
          current_list <- mSetSq_list()
          current_list[[array]] <- result_paths$mset_path
          mSetSq_list(current_list)
        }
        
        # Create merge directory
        beta_merge_dir <- create_dir(file.path(DIRS$beta, "merged"))
        
        # Merge Beta matrices from disk (only loads one at a time)
        notification_id <- showNotification("Merging beta matrices from disk...", type = "message", duration = 0)
        message("\nMerging beta matrices from disk...")
        beta_mg <- merge_beta_matrix_from_disk(beta_paths, beta_merge_dir)
        beta_merged(beta_mg)
        
        # Merge targets file
        removeNotification(notification_id)
        notification_id <- showNotification("Merging sample sheets...", type = "message", duration = 0)
        message("\nMerging sample sheets...")
        targets_result <- merge_samplesheets(arrays, DIRS$preprocessing, beta_merge_dir)
        targets_merged(targets_result$targets_merged)
        
        removeNotification(notification_id)
        notification_id <- showNotification("QC and beta generation completed successfully", type = "message", duration = 2)
        message("QC and beta generation completed successfully")
        
      }, error = function(e) {
        alert_message(list(
          type = "error",
          text = paste("Generating Beta matrix failed:", e$message)
        ))
      })
      
      # Clean up loaded QC data to free memory
      loaded_qc_data <- reactiveValues()
      
      shinyjs::hide("ld_loading_view")
      updateNavbarPage(session, "main_navbar", selected = "primary")
    })
    
    return(list(
      array_names_ld = array_names,
      mSetSq_list_ld = mSetSq_list,
      beta_merged_ld= beta_merged,
      targets_merged_ld = targets_merged,
      type_selected = type_selected
    ))
  })
}
