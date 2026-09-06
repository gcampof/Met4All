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

      # If something fails during loading the user never reaches the analysis
      # view, so the log has to be reachable from here too.
      div(
        class = "text-center mb-3",
        downloadButton(
          ns("download_log"),
          "Download log",
          class = "btn btn-sm btn-outline-secondary"
        )
      ),
      
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

          # Live progress for IDAT QC and beta-matrix generation, the two
          # longest steps in the app.
          m4a_progress_output(ns),
          
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
    
    # Put the upload screen back to a clean state. Shared by the synchronous
    # failure path below and by the worker-error observers, so a failure looks
    # the same however far into the ingest it happened.
    reset_upload <- function(msg) {
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

      shinyjs::hide("ld_loading_view")
      shinyjs::hide("ld_idats_view")
      shinyjs::hide("ld_qc_view")
      shinyjs::show("ld_main_view")

      type_selected(NULL)
      shinyjs::removeClass("card_beta", "selected")
      shinyjs::removeClass("card_idats", "selected")
      shinyjs::hide("upload_section")

      output$zipfile_ui <- renderUI({
        fileInput(
          inputId = ns("zipfile"),
          label = "Upload your ZIP files",
          accept = ".zip",
          multiple = TRUE,
          width = "280px"
        )
      })

      alert_message(list(type = "error", text = msg))
    }

    # --- FILE UPLOAD ---
    observeEvent(input$confirm_load, {
      # Show loading view
      shinyjs::hide("ld_main_view")
      shinyjs::show("ld_loading_view")
      
      tryCatch({
        # Fail fast on a full volume rather than part-way through a long run.
        m4a_check_disk(DIRS$data, upload_bytes = sum(input$zipfile$size))

        zip_paths <- input$zipfile$datapath
        # Validate here, in the request that accepted the upload, so a bad
        # archive is rejected immediately instead of after a queue wait. The
        # worker checks again on its own.
        for (zp in zip_paths) validate_archive(zp)

        queued <- m4a_queue_message()

        if (type_selected() == "IDATS") {
          # Unzipping several GB and walking every IDAT is minutes of work, so it
          # goes to a worker rather than running here and freezing every other
          # connected session. Picked up by the idat_ingest_task observer below.
          showNotification(
            if (is.null(queued)) "Extracting and organizing IDAT files..." else queued,
            type = "message", duration = 5
          )

          idat_ingest_task$invoke(
            args = list(zip_paths        = zip_paths,
                        input_dir        = DIRS$input,
                        preprocessing_dir = DIRS$preprocessing),
            app_dir = normalizePath(getwd())
          )

        } else if (type_selected() == "BETA") {
          # Reading a full beta-matrix CSV takes minutes on a real dataset, so it
          # goes to a worker like every other long step. The result is picked up
          # by the observer on beta_upload_task$status() below. The worker does
          # its own extraction, so the archives are handed over unopened.
          showNotification(
            if (is.null(queued)) "Reading the beta matrix..." else queued,
            type = "message", duration = 5
          )

          beta_upload_task$invoke(
            args = list(zip_paths = zip_paths,
                        input_dir = DIRS$input, beta_dir = DIRS$beta),
            app_dir = normalizePath(getwd())
          )
        }

      }, error = function(e) {
        message("Error during data loading: ", e$message)
        reset_upload(paste("Processing failed:", e$message))
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
    
    
    output$download_log <- m4a_log_download_handler(DIRS$analysis, DIRS$analysis_id)

    # IDAT upload: unzip, organise and classify. Minutes of disk work on a real
    # dataset, so it runs in a worker and the views switch when it reports back.
    idat_ingest_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_idat_ingest", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(idat_ingest_task$status(), {
      status <- idat_ingest_task$status()

      if (identical(status, "success")) {
        samples_df(idat_ingest_task$result())
        showNotification("Complete!", type = "message", duration = 3)
        shinyjs::hide("ld_loading_view")
        shinyjs::show("ld_idats_view")

      } else if (identical(status, "error")) {
        msg <- tryCatch({ idat_ingest_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        reset_upload(paste("Processing the IDAT upload failed:", msg))
      }
    })

    # Beta-matrix upload: the one long step on this path.
    beta_upload_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("extract_beta_and_targets", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(beta_upload_task$status(), {
      status <- beta_upload_task$status()

      if (identical(status, "success")) {
        res <- beta_upload_task$result()

        # Only a descriptor is kept in memory; the matrix stays on disk for the
        # analysis workers.
        beta_merged(list(path = res$beta_path, samples = res$samples,
                         n_probes = res$n_probes))
        targets_merged(res$targets)

        snapshot_analysis(DIRS, type = "BETA", array_names = NULL,
                          mset_paths = list(), targets = res$targets)

        showNotification("Complete!", type = "message", duration = 3)

      } else if (identical(status, "error")) {
        msg <- tryCatch({ beta_upload_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        reset_upload(paste("Loading the beta matrix failed:", msg))
      }
    })

    # --- RUN QC ---
    # The longest blocking step in the app (10-25 min for 70 EPIC samples) and it
    # runs on every IDAT analysis, so it goes to a worker. Only the samples table
    # goes in and a list of file paths comes back; the multi-GB RGChannelSets are
    # written straight to disk by the worker.
    qc_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_qc_ingest", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$run_qc, {
      shinyjs::hide("ld_idats_view")
      shinyjs::show("ld_loading_view")

      queued <- m4a_queue_message()
      if (!is.null(queued)) showNotification(queued, type = "message", duration = 8)

      qc_task$invoke(
        args = list(
          samples_df        = samples_df(),
          selected_idats    = input$idat_table_rows_selected,
          input_dir         = DIRS$input,
          preprocessing_dir = DIRS$preprocessing,
          qc_dir            = DIRS$qc
        ),
        app_dir = normalizePath(getwd())
      )
    })

    observe({
      running <- Filter(function(t) identical(t$status(), "running"),
                        list(idat_ingest_task, qc_task, beta_task, beta_upload_task))
      req(length(running) > 0)
      m4a_render_progress(output, session, running[[1]], DIRS$analysis)
    })

    # Pick the result up when the worker finishes.
    observeEvent(qc_task$status(), {
      status <- qc_task$status()

      if (identical(status, "success")) {
        res <- qc_task$result()
        qc_results(res$qc_results)
        array_names(res$arrays_used)
        showNotification("Complete!", type = "message", duration = 3)
        shinyjs::hide("ld_loading_view")
        shinyjs::show("ld_qc_view")

      } else if (identical(status, "error")) {
        msg <- tryCatch({ qc_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        shinyjs::hide("ld_loading_view")
        shinyjs::show("ld_idats_view")
        alert_message(list(type = "error", text = paste("Running QC failed:", msg)))
      }
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
    # Beta-matrix generation: the last long blocking stage (15-40 min) and, like
    # QC, one that runs on every IDAT analysis. The worker reads the RGChannelSets
    # QC left on disk and returns paths, not the ~450 MB matrix.
    beta_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_beta_generation", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$qc_continue, {
      shinyjs::hide("ld_qc_view")
      shinyjs::show("ld_loading_view")

      arrays <- array_names()

      # Snapshot the per-array thresholds now: ExtendedTask must not read inputs.
      thresholds <- setNames(
        lapply(arrays, function(a) input[[paste0("ld_qc_threshold_", a)]]),
        arrays
      )

      queued <- m4a_queue_message()
      if (!is.null(queued)) showNotification(queued, type = "message", duration = 8)

      beta_task$invoke(
        args = list(
          arrays            = arrays,
          thresholds        = thresholds,
          qc_results        = qc_results(),
          norm_method       = input$normalization,
          qc_dir            = DIRS$qc,
          filter_dir        = DIRS$filter,
          beta_dir          = DIRS$beta,
          preprocessing_dir = DIRS$preprocessing,
          analysis_dir      = DIRS$analysis
        ),
        app_dir = normalizePath(getwd())
      )
    })

    observeEvent(beta_task$status(), {
      status <- beta_task$status()

      if (identical(status, "success")) {
        res <- beta_task$result()

        # The matrix stays on disk in the worker and is loaded here once, because
        # the interactive analyses that still run in-process need it in memory.
        beta_merged(beta_descriptor(res$beta_path))
        targets_merged(res$targets_merged)
        mSetSq_list(res$mset_paths)

        snapshot_analysis(DIRS, type = "IDATS", array_names = array_names(),
                          mset_paths = res$mset_paths, targets = res$targets_merged)

        showNotification("QC and beta generation completed successfully",
                         type = "message", duration = 3)
        message("QC and beta generation completed successfully")

      } else if (identical(status, "error")) {
        msg <- tryCatch({ beta_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        shinyjs::hide("ld_loading_view")
        shinyjs::show("ld_qc_view")
        alert_message(list(type = "error",
                           text = paste("Generating Beta matrix failed:", msg)))
      }
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
