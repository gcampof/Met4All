source("modules/primary_analysis/utils.R")
source("modules/primary_analysis/annotations.R")
source("modules/primary_analysis/mds/mds_utils.R")
source("modules/primary_analysis/pca/pca_utils.R")
source("modules/primary_analysis/umap/umap_utils.R")
source("modules/primary_analysis/heatmap/heatmap_utils.R")
source("modules/primary_analysis/global_met/global_utils.R")
source("modules/primary_analysis/differential/differential_utils.R")
source("modules/primary_analysis/cnv/cnv_utils.R")
source("modules/primary_analysis/samplesheet_ui.R")

primary_analysis_server <- function(id, load_data_return, DIRS, APP_CACHE, cfg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Unpack results form load data
    view_initialized <- reactiveVal(FALSE)
    array_names <- load_data_return$array_names_ld
    mSetSq_list <- load_data_return$mSetSq_list_ld
    beta_merged <- load_data_return$beta_merged_ld
    targets_merged <- load_data_return$targets_merged_ld
    
    # Reactive states
    umap_data <- reactiveVal(NULL)
    current_view <- reactiveVal(NULL)

    # Path to the beta matrix on disk, used by every worker-backed analysis.
    # beta_merged is only ever assigned at load time so the file and the reactive
    # cannot diverge; targets, by contrast, is edited in-session and so is always
    # passed by value.
    beta_rds_path <- reactive({
      file.path(DIRS$beta, "merged", "beta_merged.rds")
    })

    # EPICv2-only analyses keep native EPICv2 probe IDs (hg38); any merge is in
    # EPICv1/450K IDs (hg19). Uploaded beta matrices are recognised by their IDs.
    is_epicv2 <- reactive({
      identical(unlist(array_names()), "EPIC_V2") || isTRUE(beta_merged()$epicv2)
    })
    annotation_pkg <- reactive({
      if (is_epicv2()) cfg$annotation_pkg_epicv2 else cfg$annotation_pkg
    })

    # ChAMP only knows 450K/EPICv1: EPICv2 DMPs use limma, and its DMRs DMRcate.
    observe(shinyjs::toggle("dmp_method_opt", condition = !is_epicv2()))

    # Uploaded palettes live in this session's own directory, so one user's
    # upload does not turn up in every other user's dropdowns.
    session_palette_dir <- file.path(DIRS$analysis, "palettes")
    palette_dirs <- function() c(DIRS$custom_color_palette, session_palette_dir)
    palettes_version <- reactiveVal(0)

    PALETTES <- reactive({
      palettes_version()
      do.call(reactiveValues, prepare_color_palettes(palette_dirs()))
    })

    # The "Add Color Palette" input previously had no observer at all: uploading a
    # file silently did nothing.
    observeEvent(input$custom_palette_file, {
      req(input$custom_palette_file)
      dir.create(session_palette_dir, showWarnings = FALSE, recursive = TRUE)

      res <- load_new_palette(
        file_path    = input$custom_palette_file$datapath,
        palette_name = tools::file_path_sans_ext(input$custom_palette_file$name),
        palette_dir  = session_palette_dir
      )

      if (isTRUE(res$success)) {
        palettes_version(palettes_version() + 1)
        showNotification(res$message, type = "message", duration = 4)
      } else {
        showNotification(paste("Could not add palette:", res$message),
                         type = "error", duration = 8)
      }
    })
    
    # Enable IDAT-only controls if type is IDATS
    observe({
      req(load_data_return$type_selected())
      
      if (load_data_return$type_selected() == "IDATS") {
        # Enable buttons
        shinyjs::enable("nav_beta_matrix")
        shinyjs::enable("nav_qc")
        shinyjs::enable("nav_cnv")
        
        # Remove tooltip wrapper class so hover tip disappears too
        shinyjs::removeClass("nav_beta_matrix_wrapper", "btn-disabled-tooltip")
        shinyjs::removeClass("nav_qc_wrapper", "btn-disabled-tooltip")
        shinyjs::removeClass("nav_cnv_wrapper", "btn-disabled-tooltip")
      }
    })

    # The two data downloads ship disabled and nothing was ever turning them
    # back on, so clicking them followed an empty href and returned the app's own
    # HTML page instead of a file. Enable them once there is data to download.
    observe({
      if (!is.null(beta_merged())) {
        shinyjs::enable("download_beta")
        shinyjs::removeClass("download_beta_wrapper", "btn-disabled-tooltip")
      }
      if (!is.null(targets_merged())) {
        shinyjs::enable("download_targets")
        shinyjs::removeClass("download_targets_wrapper", "btn-disabled-tooltip")
      }
    })
    message("[PRIMARY_ANALYSIS] Setup complete!")
    
    # Set qc path.
    # addResourcePath registers PROCESS-GLOBALLY while DIRS$qc is per session, so a
    # shared "qc_reports" prefix means the last session to connect owns it and every
    # other user's iframe is served that session's reports. The prefix is therefore
    # session-scoped, registered once, and released when the session ends.
    qc_resource_prefix <- paste0("qc_reports_", substr(session$token, 1, 8))
    observeEvent(DIRS$qc, once = TRUE, {
      # Guarded: normalizePath() on a missing directory makes addResourcePath
      # throw, and an error here takes the whole session down at start-up. QC
      # reports simply will not be served if the directory is absent.
      if (dir.exists(DIRS$qc)) {
        try(addResourcePath(prefix = qc_resource_prefix,
                            directoryPath = normalizePath(DIRS$qc)), silent = TRUE)
      } else {
        warning("QC directory missing, QC reports will not be served: ", DIRS$qc)
      }
    })
    session$onSessionEnded(function() {
      suppressWarnings(try(removeResourcePath(qc_resource_prefix), silent = TRUE))
    })
    
    # Disable/enable buttons based on data type (beta or idats)
    observe({
      req(length(names(input)) > 0)
      # `|` here required numeric/logical operands. It worked only while
      # beta_merged() held the matrix itself; it is now a small descriptor list,
      # which made this throw and take the session down as soon as data loaded.
      req(!is.null(beta_merged()) || !is.null(targets_merged()))
      
      if (!view_initialized()) {
        if (load_data_return$type_selected() == "IDATS") {
          update_active_button("nav_beta_matrix")
          show_view("view_beta_matrix", "Beta Matrix")
          current_view("beta_matrix")
        } else {
          update_active_button("nav_mds")
          show_view("view_mds", "Multidimensional Scaling (MDS)")
          current_view("mds")
        }
        
        view_initialized(TRUE)
      }
    })
    
    # Update palettes
    observe({
      req(PALETTES())
      req(length(names(input)) > 0)
      update_all_palettes(session, PALETTES())
    })
    
    # Initialize metadata choices — re-runs whenever the samplesheet gains columns,
    # so the current selection is kept when it is still a valid column
    observe({
      req(length(names(input)) > 0)
      req(targets_merged())
      meta_cols <- colnames(targets_merged())
      
      update_meta_cols <- function(input_ids, default) {
        for (input_id in input_ids) {
          current <- isolate(input[[input_id]])
          updateSelectInput(session, input_id, choices = meta_cols,
                            selected = if (isTRUE(current %in% meta_cols)) current else default)
        }
      }
      
      update_meta_cols(c("pca_color_by", "mds_color_by", "heatmap_annotation_cols"), meta_cols[1])
      
      # Initializa sample ID choices dinamically (default "ID")
      default_id <- if ("ID" %in% meta_cols) "ID" else meta_cols[1]
      update_meta_cols(c("mds_id_col", "pca_id_col", "umap_id_col", "heatmap_id_col",
                         "global_met_id_col", "diff_met_id_col", "cnv_id_col"), default_id)
      
      # Initialize comparison ID choices dinamically (default Sample_Group)
      default_group <- if ("Sample_Group" %in% meta_cols) "Sample_Group" else meta_cols[1]
      update_meta_cols(c("global_met_comparison_col", "diff_met_comparison_col"), default_group)
      
      # Initialize array type choices dinamically for CNV
      if(!is.null(array_names())){
        updateSelectInput(session, "cnv_array_select", choices = array_names(), selected = array_names()[1])
      }
    })
    
    # --- NAVIGATION LOGIC ---
    observeEvent(input$nav_beta_matrix, {
      current_view("beta_matrix")
      update_active_button("nav_beta_matrix")
      show_view("view_beta_matrix", "Beta Matrix")
    })
    
    observeEvent(input$nav_qc, {
      has_array_data <- !is.null(array_names()) && length(array_names()) > 0
      current_view("qc")
      update_active_button("nav_qc")
      show_view("view_qc", "QC Report")
    })
    
    observeEvent(input$nav_mds, {
      current_view("mds")
      update_active_button("nav_mds")
      show_view("view_mds", "Multidimensional Scaling (MDS)")
    })
    
    observeEvent(input$nav_pca, {
      current_view("pca")
      update_active_button("nav_pca")
      show_view("view_pca", "Principal Component Analysis (PCA)")
    })
    
    observeEvent(input$nav_umap, {
      current_view("umap")
      update_active_button("nav_umap")
      show_view("view_umap", "UMAP")
    })
    
    observeEvent(input$nav_heatmap, {
      current_view("heatmap")
      update_active_button("nav_heatmap")
      show_view("view_heatmap", "Heatmap")
    })
    
    observeEvent(input$nav_global, {
      current_view("global")
      update_active_button("nav_global")
      show_view("view_global_met", "Global Methylation")
    })
    
    observeEvent(input$nav_differential, {
      current_view("differential")
      update_active_button("nav_differential")
      show_view("view_differential", "Differential Methylation")
    })
    
    observeEvent(input$nav_cnv, {
      current_view("cnv")
      update_active_button("nav_cnv")
      show_view("view_cnv", "CNV")
    }) 
    observeEvent(input$nav_samplesheet, {
      current_view("samplesheet")
      update_active_button("nav_samplesheet")
      show_view("view_samplesheet", "Explore Samplesheet")
    })
    
    # --- DOWNLOAD BUTTONS BETA/TARGETS ---
    output$download_beta <- downloadHandler(
      filename = function() {
        "beta_merged.csv"
      },
      content = function(file) {
        src <- file.path(DIRS$beta, "merged", "beta_merged.csv")
        if (file.exists(src)) {
          file.copy(src, file)
          return(invisible(NULL))
        }

        rds <- file.path(DIRS$beta, "merged", "beta_merged.rds")
        if (!file.exists(rds)) {
          # validate()/req() here would hand the browser an HTML error page
          # instead of a download, which looks like a broken button.
          writeLines("The beta matrix is not available yet. Load or generate it first.", file)
          return(invisible(NULL))
        }
        beta <- readRDS(rds)
        data.table::fwrite(
          data.table::data.table(CpG = rownames(beta), beta),
          file
        )
      }
    )
    
    # --- DOWNLOAD TARGETS ---
    output$download_targets <- downloadHandler(
      filename = function() "targets_merged.csv",
      content = function(file) {
        tg <- targets_merged()
        if (is.null(tg)) {
          writeLines("No samplesheet loaded yet.", file)
          return(invisible(NULL))
        }
        write.csv(tg, file, row.names = TRUE)
      }
    )
    
    # --- BETA MATRIX BOXPLOT UI ---
    output$beta_matrix_tabs <- renderUI({
      req(array_names())
      
      tabs <- lapply(array_names(), function(arr) {
        tabPanel(
          title = arr,
          br(),
          div(class = "d-flex gap-2 justify-content-end mb-2",
              lapply(c("png", "pdf", "svg"), function(ext) {
                downloadButton(ns(paste0("beta_boxplot_", arr, "_", ext)), paste0(" ", toupper(ext)),
                               class = "btn btn-sm btn-outline-secondary")
              })),
          # Fixed height, natural width: the bars keep the same size whatever the
          # number of samples, and large cohorts scroll sideways.
          div(style = "overflow-x: auto;",
              imageOutput(outputId = ns(paste0("pa_beta_boxplot_", arr)), height = "600px")),
          br()
        )
      })
      
      do.call(
        tabsetPanel,
        c(
          list(id = ns("beta_matrix_tabset"), type = "tabs"),
          tabs
        )
      )
    })
    
    # --- BETA MATRIX BOXPLOT LOGIC ---
    observeEvent(input$beta_matrix_tabset, {
      if (is.null(array_names())) {
        shiny::need(FALSE, "Beta matrix distribution cannot be shown from BetaMatrix only")
      } else {
        arr <- input$beta_matrix_tabset
        req(arr)
        
        # Get the boxplot path from stored results
        boxplot_path <- file.path(DIRS$beta, arr, paste0("beta_boxplot_", arr, ".png"))
        output_id <- paste0("pa_beta_boxplot_", arr)
        output[[output_id]] <- renderImage({
          list(
            src = boxplot_path,
            contentType = "image/png",
            height = "600px"
          )
        }, deleteFile = FALSE)
      }
    })
    
    
    # --- BETA MATRIX BOXPLOT EXPORTS ---
    # Files written at beta generation; SVG only exists for analyses run since it
    # was added.
    observe({
      for (arr in unlist(array_names())) local({
        a <- arr
        for (ext in c("png", "pdf", "svg")) local({
          e <- ext
          output[[paste0("beta_boxplot_", a, "_", e)]] <- downloadHandler(
            filename = function() paste0("beta_boxplot_", a, ".", e),
            content  = function(file) {
              src <- file.path(DIRS$beta, a, paste0("beta_boxplot_", a, ".", e))
              validate(need(file.exists(src),
                            paste0("No ", toupper(e), " for this analysis. Re-run the beta matrix generation to create it.")))
              file.copy(src, file)
            }
          )
        })
      })
    })


    # --- QC PDF VIEWER UI ---
    output$qc_pdf_tabs <- renderUI({
      tabs <- lapply(array_names(), function(arr) {
        tabPanel( title = arr, htmlOutput(outputId = ns(paste0("pa_qc_viewer", arr)),
                                          height = "600px")
        )
      })
      
      do.call(
        tabsetPanel,
        c(
          list(id = ns("qc_pdf_tabset"), type = "tabs"),
          tabs
        )
      )
    })
    
    # --- QC PDF VIEWER LOGIC ---
    observeEvent(input$qc_pdf_tabset, {
      if(is.null(array_names())){
        shiny::need(FALSE, "QC can not be calcualted from BetaMatrix only, 
                    it requires load form IDATs")
      } else {
        arr <- input$qc_pdf_tabset
        req(arr)
        
        # Build URL using this session's registered alias, not the filesystem path
        src <- paste0(qc_resource_prefix, "/", arr, "/2.0-QC_Report_", arr, ".pdf")
        
        output_id <- paste0("pa_qc_viewer", arr)
        output[[output_id]] <- renderText({
          tryCatch({
            return(paste('<iframe style="height:600px; width:100%" src="', src, '"></iframe>', sep = ""))
          }, error = function(e) {
            error_msg <- e$message
            shiny::validate(
              shiny::need(FALSE, paste0("Error loading QC report: ", error_msg))
            )
          })
        })
      }
    })
    
    # --- MDS PLOT LOGIC ---
    # Reactive trigger for analysis
    cached_mds_plot <- reactiveVal(NULL)
    
    mds_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_mds_data", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$mds_run_analysis, {
      req(targets_merged())
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      queued <- m4a_queue_message()
      showNotification(if (is.null(queued)) "Running MDS analysis..." else queued,
                       type = "message", duration = 5)

      mds_task$invoke(
        args = list(
          beta_path = beta_rds_path(),
          targets   = targets_merged(),
          id_col    = input$mds_id_col,
          top_cpgs  = input$mds_top_cpgs
        ),
        app_dir = app_dir
      )
    })

    observe({
      if (identical(mds_task$status(), "running")) shinyjs::disable("mds_run_analysis")
      else shinyjs::enable("mds_run_analysis")
    })

    mds_data <- reactive({
      status <- mds_task$status()
      validate(need(status != "initial", "Press Run Analysis to start."))
      validate(need(status != "running", "Running MDS analysis..."))
      tryCatch(mds_task$result(),
               error = function(e) {
                 validate(need(FALSE, paste0("Error: ", m4a_error_text(e))))
                 NULL
               })
    })
    
    # Update color_by choices whenever mds_data recomputes
    observe({
      req(mds_data())
      
      # Exclude coordinate and sample ID columns
      exclude_cols <- c("Sample", "Dim1", "Dim2")
      color_cols <- setdiff(colnames(mds_data()), exclude_cols)
      current <- isolate(input$mds_color_by)
      updateSelectInput(session, "mds_color_by", 
                        choices = color_cols,
                        selected = if (current %in% color_cols) current else color_cols[1])
    })
    
    output$mds_plot <- renderPlot({
      # Show placeholder if analysis hasn't been run
      if (identical(mds_task$status(), "initial")) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1, 
                              label = "Click 'Run MDS Analysis' to generate plot",
                              size = 6, color = "gray50") +
            ggplot2::theme_void()
        )
      }
      
      req(mds_data(), input$mds_color_by, input$mds_color_palette)
      validate(need(input$mds_color_palette %in% names(PALETTES()$all_palettes), "Invalid palette"))
      
      tryCatch({
        p <- plot_mds(mds_data(), input$mds_color_by, PALETTES()$all_palettes[[input$mds_color_palette]])
        cached_mds_plot(p)
        p
      }, error = function(e) {
        shiny::validate(shiny::need(FALSE, paste0("Error: ", e$message)))
      })
    })
    
    output$mds_download_png <- downloadHandler(
      filename = function() paste0("mds_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(cached_mds_plot(), input$mds_export_width, input$mds_export_height)
        ggplot2::ggsave(file, cached_mds_plot(), width = input$mds_export_width,
                        height = input$mds_export_height, dpi = 150, bg = "white", device = "png")
      }
    )
    
    output$mds_download_pdf <- downloadHandler(
      filename = function() paste0("mds_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(cached_mds_plot(), input$mds_export_width, input$mds_export_height)
        ggplot2::ggsave(file, cached_mds_plot(), width = input$mds_export_width,
                        height = input$mds_export_height, bg = "white", device = "pdf")
      }
    )
    
    output$mds_download_svg <- downloadHandler(
      filename = function() paste0("pca_plot_", Sys.Date(), ".svg"),
      content = function(file) {
        req(cached_mds_plot(), input$mds_export_width, input$mds_export_height)
        ggplot2::ggsave(file, cached_mds_plot(), width = input$mds_export_width,
                        height = input$mds_export_height, bg = "white", device = svglite::svglite)
      }
    )
    
    # --- PCA PLOT LOGIC
    # Reactive trigger for analysis
    cached_pca_plot <- reactiveVal(NULL)
    
    # Prepare PCA data (only runs when trigger changes)
    pca_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_pca_data", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$pca_run_analysis, {
      req(targets_merged())
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      queued <- m4a_queue_message()
      showNotification(if (is.null(queued)) "Running PCA analysis..." else queued,
                       type = "message", duration = 5)

      pca_task$invoke(
        args = list(
          beta_path = beta_rds_path(),
          targets   = targets_merged(),
          id_col    = input$pca_id_col,
          top_cpgs  = input$pca_top_cpgs
        ),
        app_dir = app_dir
      )
    })

    observe({
      if (identical(pca_task$status(), "running")) shinyjs::disable("pca_run_analysis")
      else shinyjs::enable("pca_run_analysis")
    })

    pca_data <- reactive({
      status <- pca_task$status()
      validate(need(status != "initial", "Press Run Analysis to start."))
      validate(need(status != "running", "Running PCA analysis..."))
      tryCatch(pca_task$result(),
               error = function(e) {
                 validate(need(FALSE, paste0("Error: ", m4a_error_text(e))))
                 NULL
               })
    })
    
    # Update color_by choices whenever pca_data recomputes
    observe({
      req(pca_data())
      
      # Exclude coordinate and sample ID columns
      exclude_cols <- c("Sample", "PC1", "PC2", "PC3")
      color_cols <- setdiff(colnames(pca_data()), exclude_cols)
      current <- isolate(input$pca_color_by)
      updateSelectInput(session, "pca_color_by", 
                        choices = color_cols,
                        selected = if (current %in% color_cols) current else color_cols[1])
    })
    
    output$pca_plot <- renderPlot({
      # Show placeholder if analysis hasn't been run
      if (identical(pca_task$status(), "initial")) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1, 
                              label = "Click 'Run PCA Analysis' to generate plot",
                              size = 6, color = "gray50") +
            ggplot2::theme_void()
        )
      }
      
      req(pca_data(), input$pca_color_by, input$pca_color_palette, input$pca_dims)
      
      validate(need(
        input$pca_color_palette %in% names(PALETTES()$all_palettes),
        "Invalid palette"
      ))
      
      tryCatch({
        p <- plot_pca(
          pca_data(),
          input$pca_color_by,
          input$pca_dims,
          PALETTES()$all_palettes[[input$pca_color_palette]]
        )
        cached_pca_plot(p)
        p
      }, error = function(e) {
        shiny::validate(
          shiny::need(FALSE, paste0("Error rendering PCA plot: ", e$message))
        )
      })
    })
    
    output$pca_download_png <- downloadHandler(
      filename = function() paste0("pca_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(cached_pca_plot(), input$pca_export_width, input$pca_export_height)
        ggplot2::ggsave(file, cached_pca_plot(), width = input$pca_export_width,
                        height = input$pca_export_height, dpi = 150, bg = "white", device = "png")
      }
    )
    
    output$pca_download_pdf <- downloadHandler(
      filename = function() paste0("pca_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(cached_pca_plot(), input$pca_export_width, input$pca_export_height)
        ggplot2::ggsave(file, cached_pca_plot(), width = input$pca_export_width,
                        height = input$pca_export_height, bg = "white", device = "pdf")
      }
    )
    
    output$pca_download_svg <- downloadHandler(
      filename = function() paste0("pca_plot_", Sys.Date(), ".svg"),
      content = function(file) {
        req(cached_pca_plot(), input$pca_export_width, input$pca_export_height)
        ggplot2::ggsave(file, cached_pca_plot(), width = input$pca_export_width,
                        height = input$pca_export_height, bg = "white", device = svglite::svglite)
      }
    )
    
    # --- UMAP PLOT LOGIC ---
    # Reactive trigger for analysis
    cached_umap_plot <- reactiveVal(NULL)
    cached_umap_model <- reactiveVal(NULL)
    
    # Prepare UMAP data (only runs when trigger changes)
    umap_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_umap_data", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$umap_run_analysis, {
      req(targets_merged(),
          input$umap_top_cpgs, input$umap_min_dist,
          input$umap_n_neighbors, input$umap_metric,
          input$umap_knn, input$umap_consensus_k_max,
          input$umap_id_col, input$umap_seed)
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      queued <- m4a_queue_message()
      showNotification(if (is.null(queued)) "Running UMAP analysis..." else queued,
                       type = "message", duration = 5)

      umap_task$invoke(
        args = list(
          beta_path       = beta_rds_path(),
          targets         = targets_merged(),
          top_cpgs        = input$umap_top_cpgs,
          min_dist        = input$umap_min_dist,
          n_neighbors     = input$umap_n_neighbors,
          metric          = input$umap_metric,
          knn             = input$umap_knn,
          consensus_k_max = input$umap_consensus_k_max,
          id_col          = input$umap_id_col,
          seed            = input$umap_seed
        ),
        app_dir = app_dir
      )
    })

    observe({
      if (identical(umap_task$status(), "running")) shinyjs::disable("umap_run_analysis")
      else shinyjs::enable("umap_run_analysis")
    })

    umap_result <- reactive({
      status <- umap_task$status()
      validate(need(status != "initial", "Press Run Analysis to start."))
      validate(need(status != "running", "Running UMAP analysis..."))
      tryCatch(umap_task$result(),
               error = function(e) {
                 validate(need(FALSE, paste0("Error preparing UMAP data: ",
                                             m4a_error_text(e))))
                 NULL
               })
    })

    # Side effects belong here, not inside the reactive above: writing the
    # clusters back and stashing the model must happen once per completed run.
    observeEvent(umap_task$status(), {
      req(identical(umap_task$status(), "success"))
      res <- tryCatch(umap_task$result(), error = function(e) NULL)
      req(!is.null(res))
      targets_merged(res$targets_updated)
      cached_umap_model(res$um_model)
    })

    umap_data <- reactive({
      req(umap_result())
      umap_result()$umap_df
    })
    
    # Run analysis when button is clicked
    
    # Update color_by choices whenever umap_data recomputes
    observe({
      req(umap_data())
      
      # Exclude coordinate and sample ID columns
      exclude_cols <- c("Sample", "UMAP1", "UMAP2")
      color_cols <- setdiff(colnames(umap_data()), exclude_cols)
      current <- isolate(input$umap_color_by)
      updateSelectInput(session, "umap_color_by", 
                        choices = color_cols,
                        selected = if (current %in% color_cols) current else color_cols[1])
    })
   
    
    # UMAP PREDICT
    # Download UMAP model
    output$umap_download_model <- downloadHandler(
      filename = function() paste0("umap_model_", Sys.Date(), ".rds"),
      content = function(file) {
        req(cached_umap_model())
        saveRDS(cached_umap_model(), file)
      }
    )
    
    # upload UMAP model
    observeEvent(input$umap_upload_model, {
      req(input$umap_upload_model)
      
      tryCatch({
        file_info <- input$umap_upload_model
        in_path   <- file_info$datapath
        file_name <- file_info$name
        
        # Validate it's an RDS file
        if (!grepl("\\.rds$", file_name, ignore.case = TRUE)) {
          showNotification("Please upload a valid .rds file", type = "error", duration = 5)
          return()
        }
        
        out_path <- file.path(DIRS$umap, file_name)
        file.copy(from = in_path, to = out_path, overwrite = TRUE)
        
        # Load and store the model. readRDS on an uploaded file is only as safe as
        # the file, so check it really is a UMAP model before anything uses it.
        model <- readRDS(out_path)
        if (!is.list(model) || is.null(model$layout) || is.null(model$config)) {
          stop("This file is not a UMAP model (expected 'layout' and 'config').")
        }
        cached_umap_model(model)
        
        showNotification("UMAP model loaded successfully", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error loading UMAP model:", e$message), type = "error", duration = 5)
      })
    })
    
    # Predict UMAP
    # Reactive to store predicted umap df
    predicted_umap_df <- reactiveVal(NULL)
    
    # umap mode flag
    umap_mode <- reactiveVal("training")
    
    # Sync color_by choices to match whichever df is currently active
    observe({
      if (umap_mode() == "training") {
        req(umap_data())
        exclude_cols <- c("Sample", "UMAP1", "UMAP2")
        color_cols <- setdiff(colnames(umap_data()), exclude_cols)
        current <- isolate(input$umap_color_by)
        updateSelectInput(session, "umap_color_by",
                          choices = color_cols,
                          selected = if (current %in% color_cols) current else color_cols[1])
      }
    })
    
    # Run prediction when button is clicked
    observeEvent(input$umap_run_predict, {
      
      # Check model exists — either uploaded or trained in session
      if (is.null(cached_umap_model())) {
        showNotification("No UMAP model found. Please upload a model first.", 
                         type = "error", duration = 5)
        return()
      }
      
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      queued <- m4a_queue_message()
      showNotification(if (is.null(queued)) "Running UMAP projection..." else queued,
                       type = "message", duration = 5)

      predict_task$invoke(
        args = list(beta_path = beta_rds_path(), umap_model = cached_umap_model()),
        app_dir = app_dir
      )
    })

    predict_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("predict_umap", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(predict_task$status(), {
      status <- predict_task$status()

      if (identical(status, "success")) {
        df <- tryCatch(predict_task$result(), error = function(e) NULL)

        if (is.null(df)) {
          showNotification("No new samples found, all samples already exist in the model.",
                           type = "warning", duration = 5)
          return()
        }

        predicted_umap_df(df)
        umap_mode("predicted")

        color_cols <- setdiff(colnames(df), c("Sample", "UMAP1", "UMAP2"))
        updateSelectInput(session, "umap_color_by",
                          choices = color_cols, selected = "sample_origin")
        showNotification("Projection complete.", type = "message", duration = 3)

      } else if (identical(status, "error")) {
        msg <- tryCatch({ predict_task$result(); "unknown error" },
                        error = function(e) m4a_error_text(e))
        showNotification(paste("Error during UMAP projection:", msg),
                         type = "error", duration = 8)
      }
    })
    
    # Unified umap_plot — switches between training and predicted mode
    output$umap_plot <- renderPlot({
      
      if (umap_mode() == "predicted") {
        req(predicted_umap_df(), input$umap_color_by)
        tryCatch({
          p <- plot_umap(
            umap_df         = predicted_umap_df(),
            color_by        = input$umap_color_by,
            legend_position = input$umap_legend_position,
            color_palette   = PALETTES()$all_palettes[[input$umap_color_palette]],
            show_summary    = input$umap_show_summary,
            show_labels     = input$umap_show_labels,
            is_predicted    = TRUE
          )
          cached_umap_plot(p)
          p
        }, error = function(e) {
          shiny::validate(shiny::need(FALSE, paste0("Error rendering predicted UMAP: ", e$message)))
        })
        
      } else {
        # Show placeholder if analysis hasn't been run yet
        if (identical(umap_task$status(), "initial")) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 1, y = 1,
                                label = "Click 'Run Analysis' to generate plot",
                                size = 6, color = "gray50") +
              ggplot2::theme_void()
          )
        }
        
        req(umap_data(), input$umap_color_by)
        tryCatch({
          p <- plot_umap(
            umap_df         = umap_data(),
            color_by        = input$umap_color_by,
            legend_position = input$umap_legend_position,
            color_palette   = PALETTES()$all_palettes[[input$umap_color_palette]],
            show_summary    = input$umap_show_summary,
            show_labels     = input$umap_show_labels,
            top_cpgs        = input$umap_top_cpgs,
            min_dist        = input$umap_min_dist,
            n_neighbors     = input$umap_n_neighbors,
            knn             = input$umap_knn,
            consensus_k_max = input$umap_consensus_k_max,
            metric          = input$umap_metric,
            is_predicted    = FALSE
          )
          cached_umap_plot(p)
          p
        }, error = function(e) {
          shiny::validate(shiny::need(FALSE, paste0("Error rendering UMAP plot: ", e$message)))
        })
      }
      
    })
    
    # Download handlers
    output$umap_download_png <- downloadHandler(
      filename = function() paste0("umap_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(cached_umap_plot(), input$umap_export_width, input$umap_export_height)
        ggplot2::ggsave(file, cached_umap_plot(), width = input$umap_export_width,
                        height = input$umap_export_height, dpi = 150, bg = "white", device = "png")
      }
    )
    
    output$umap_download_pdf <- downloadHandler(
      filename = function() paste0("umap_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(cached_umap_plot(), input$umap_export_width, input$umap_export_height)
        ggplot2::ggsave(file, cached_umap_plot(), width = input$umap_export_width,
                        height = input$umap_export_height, bg = "white", device = "pdf")
      }
    )
    
    output$umap_download_svg <- downloadHandler(
      filename = function() paste0("umap_plot_", Sys.Date(), ".svg"),
      content = function(file) {
        req(cached_umap_plot(), input$umap_export_width, input$umap_export_height)
        ggplot2::ggsave(file, cached_umap_plot(), width = input$umap_export_width,
                        height = input$umap_export_height, bg = "white", device = svglite::svglite)
      }
    )
    
    
    # --- HEATMAP PLOT LOGIC ---
    # Reactive trigger for analysis
    heatmap_analysis_trigger <- reactiveVal(0)
    
    # Store the rendered heatmap plot for downloads
    heatmap_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_heatmap_cc", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$heatmap_run_analysis, {
      req(beta_merged(), targets_merged(), input$heatmap_id_col,
          input$heatmap_cc_kmax, input$heatmap_cc_reps,
          input$heatmap_cc_pItem, input$heatmap_cc_seed,
          input$heatmap_color_palette)
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      queued <- m4a_queue_message()
      showNotification(
        if (is.null(queued)) "Running heatmap analysis..." else queued,
        type = "message", duration = 5
      )

      heatmap_task$invoke(
        args = list(
          beta_path       = beta_rds_path(),
          targets         = targets_merged(),
          id_col          = input$heatmap_id_col,
          annotation_cols = input$heatmap_annotation_cols,
          palette_dir     = palette_dirs(),
          palette_name    = input$heatmap_color_palette,
          top_cpgs        = input$heatmap_top,
          cc_kmax         = input$heatmap_cc_kmax,
          cc_reps         = input$heatmap_cc_reps,
          cc_pItem        = input$heatmap_cc_pItem,
          cc_seed         = input$heatmap_cc_seed
        ),
        app_dir = app_dir
      )
    })

    observe({
      if (identical(heatmap_task$status(), "running")) {
        shinyjs::disable("heatmap_run_analysis")
      } else {
        shinyjs::enable("heatmap_run_analysis")
      }
    })

    heatmap_cc_data <- reactive({
      status <- heatmap_task$status()
      validate(need(status != "initial", "Configure the parameters and press Run Analysis."))
      validate(need(status != "running", "Consensus clustering running..."))
      tryCatch(
        heatmap_task$result(),
        error = function(e) {
          validate(need(FALSE, paste0("Error: ", m4a_error_text(e))))
          NULL
        }
      )
    })

    # Appearance changes rebuild only the Heatmap object, which is now cheap
    # because the clustering arrived with the worker result
    cached_heatmap_result <- reactive({
      req(heatmap_cc_data())
      tryCatch(
        plot_heatmap(
          cc_data        = heatmap_cc_data(),
          rowK           = input$heatmap_row_k,
          colK           = input$heatmap_col_k,
          show_row_names = input$heatmap_show_row_names,
          show_col_names = input$heatmap_show_col_names
        ),
        error = function(e) {
          validate(need(FALSE, paste0("Error rendering heatmap: ", m4a_error_text(e))))
          NULL
        }
      )
    })
    
    # Helper accessors
    cached_heatmap_ht <- reactive({
      req(cached_heatmap_result())
      cached_heatmap_result()$ht
    })
    
    # Store consensus cluster properly into targets_merged
    observeEvent(cached_heatmap_result(), {
      req(cached_heatmap_result(), input$heatmap_id_col)
      col_class <- cached_heatmap_result()$col_class

      try(write.table(
        data.frame(sample = names(col_class), CCP_cluster = col_class),
        file = file.path(DIRS$heatmap,
                         paste0("ConsensusClass_k", input$heatmap_col_k, ".tsv")),
        sep = "\t", quote = FALSE, row.names = FALSE
      ), silent = TRUE)

      updated <- targets_merged()
      # Match by ID
      updated$consensus_cluster <- factor(paste0("CC", col_class[updated[[input$heatmap_id_col]]]))
      targets_merged(updated)
      showNotification("Consensus clusters saved to samplesheet.", type = "message", duration = 3)
    })
    
    # Plot output
    output$heatmap_plot <- renderPlot({
      req(cached_heatmap_ht())
      showNotification("Generating plot, please wait...", type = "message", duration = NULL, id = "ht_render")
      ComplexHeatmap::draw(
        cached_heatmap_ht(),
        merge_legends          = TRUE,
        heatmap_legend_side    = isolate(input$heatmap_legend_position),
        annotation_legend_side = isolate(input$heatmap_annotation_legend_position),
        padding                = grid::unit(c(10, 10, 10, 10), "mm")
      )
      removeNotification("ht_render")
    })
    
    # Download handlers — notify user since redraw is needed for base graphics
    output$heatmap_download_png <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".png"),
      content = function(file) {
        req(cached_heatmap_ht(), input$heatmap_export_width, input$heatmap_export_height)
        showNotification("Generating PNG, please wait...", type = "message", duration = NULL, id = "ht_png")
        w_px <- round(input$heatmap_export_width * 150)
        h_px <- round(input$heatmap_export_height * 150)
        png(file, width = w_px, height = h_px, res = 150)
        ComplexHeatmap::draw(
          cached_heatmap_ht(),
          merge_legends          = TRUE,
          heatmap_legend_side    = isolate(input$heatmap_legend_position),
          annotation_legend_side = isolate(input$heatmap_annotation_legend_position),
          padding                = grid::unit(c(10, 10, 10, 10), "mm")
        )
        dev.off()
        removeNotification("ht_png")
      }
    )
    
    output$heatmap_download_pdf <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(cached_heatmap_ht(), input$heatmap_export_width, input$heatmap_export_height)
        showNotification("Generating PDF, please wait...", type = "message", duration = NULL, id = "ht_pdf")
        pdf(file, width = input$heatmap_export_width, height = input$heatmap_export_height)
        ComplexHeatmap::draw(
          cached_heatmap_ht(),
          merge_legends          = TRUE,
          heatmap_legend_side    = isolate(input$heatmap_legend_position),
          annotation_legend_side = isolate(input$heatmap_annotation_legend_position),
          padding                = grid::unit(c(10, 10, 10, 10), "mm")
        )
        dev.off()
        removeNotification("ht_pdf")
      }
    )
    
    output$heatmap_download_svg <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".svg"),
      content = function(file) {
        req(cached_heatmap_ht(), input$heatmap_export_width, input$heatmap_export_height)
        showNotification("Generating SVG, please wait...", type = "message", duration = NULL, id = "ht_svg")
        svglite::svglite(file, width = input$heatmap_export_width, height = input$heatmap_export_height)
        ComplexHeatmap::draw(
          cached_heatmap_ht(),
          merge_legends          = TRUE,
          heatmap_legend_side    = isolate(input$heatmap_legend_position),
          annotation_legend_side = isolate(input$heatmap_annotation_legend_position),
          padding                = grid::unit(c(10, 10, 10, 10), "mm")
        )
        dev.off()
        removeNotification("ht_svg")
      }
    )
    
    # Download handler for consensus class TSV
    output$heatmap_download_consensus <- downloadHandler(
      filename = function() {
        paste0("ConsensusClass_k", input$heatmap_col_k, ".tsv")  # ✅
      },
      content = function(file) {
        consensus_file <- file.path(
          DIRS$heatmap,
          paste0("ConsensusClass_k", input$heatmap_col_k, ".tsv")
        )
        validate(
          need(file.exists(consensus_file),
               paste0("Consensus class file not found. Please run analysis first."))
        )
        file.copy(consensus_file, file)
      }
    )
    
    
    # --- GLOBAL METHYLATION LOGIC ---
    # Store the rendered plot for downloads
    cached_global_met_plot <- reactiveVal(NULL)
    
    # Populate group checkboxes when comparison column changes
    observeEvent(input$global_met_comparison_col, {
      req(input$global_met_comparison_col)
      
      raw_vals <- na.omit(as.character(targets_merged()[[input$global_met_comparison_col]]))
      
      validate(
        need(length(raw_vals) > 0, 
             paste0("Column '", input$global_met_comparison_col, "' has no non-missing values. Please choose a different column."))
      )
      
      counts <- table(raw_vals)
      levels <- sort(names(counts))
      labels <- paste0(levels, " (", as.numeric(counts[levels]), " ",
                       ifelse(as.numeric(counts[levels]) == 1, "sample", "samples"), ")")
      
      updateCheckboxGroupInput(session, "global_met_group1", choices = setNames(levels, labels), selected = NULL)
      updateCheckboxGroupInput(session, "global_met_group2", choices = setNames(levels, labels), selected = NULL)
    })
    
    # Show/hide custom group builder
    observeEvent(input$global_met_comparison_type, {
      if (input$global_met_comparison_type == "custom") {
        shinyjs::show("global_met_custom_groups")
      } else {
        shinyjs::hide("global_met_custom_groups")
      }
    })
    
    # Use eventReactive directly
    global_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_global_methylation", args, app_dir, session_dir = DIRS$analysis)
    })

    observeEvent(input$global_met_run_analysis, {
      req(targets_merged(), input$global_met_id_col,
          input$global_met_comparison_col, input$global_met_color_palette,
          input$global_met_comparison_type)
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      if (identical(input$global_met_comparison_type, "custom")) {
        validate(
          need(length(input$global_met_group1) > 0, "Please select at least one level for Group 1"),
          need(length(input$global_met_group2) > 0, "Please select at least one level for Group 2")
        )
      }

      queued <- m4a_queue_message()
      showNotification(
        if (is.null(queued)) "Running global methylation analysis..." else queued,
        type = "message", duration = 5
      )

      global_task$invoke(
        args = list(
          beta_path       = beta_rds_path(),
          targets         = targets_merged(),
          id_col          = input$global_met_id_col,
          comparison_col  = input$global_met_comparison_col,
          comparison_type = input$global_met_comparison_type,
          group1          = input$global_met_group1,
          group2          = input$global_met_group2,
          cache_dir       = DIRS$cache,
          annotation_pkg  = annotation_pkg(),
          palette_dir     = palette_dirs(),
          palette_name    = input$global_met_color_palette
        ),
        app_dir = app_dir
      )
    })

    observe({
      if (identical(global_task$status(), "running")) {
        shinyjs::disable("global_met_run_analysis")
      } else {
        shinyjs::enable("global_met_run_analysis")
      }
    })

    global_met_data <- reactive({
      status <- global_task$status()
      validate(need(status != "initial", "Press Run Analysis to start."))
      validate(need(status != "running", "Running global methylation analysis..."))
      tryCatch(global_task$result(),
               error = function(e) {
                 validate(need(FALSE, paste0("Error: ", m4a_error_text(e))))
                 NULL
               })
    })

    # The ggplot is assembled here, from the small summary the worker returned.
    observeEvent(global_met_data(), {
      req(global_met_data())
      tryCatch({
        cached_global_met_plot(plot_global_methylation(global_met_data()))
      }, error = function(e) {
        cached_global_met_plot(NULL)
        showNotification(paste("Error rendering global methylation plot:", e$message),
                         type = "error", duration = 8)
      })
    })
    
    # Plot output - just renders the cached plot
    output$global_met_plot <- renderPlot({
      req(cached_global_met_plot())
    })
    
    # Download handlers using cached plot
    output$global_met_download_png <- downloadHandler(
      filename = function() paste0("global_methylation_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(cached_global_met_plot(), input$global_met_export_width, input$global_met_export_height)
        ggplot2::ggsave(file, cached_global_met_plot(), width = input$global_met_export_width,
                        height = input$global_met_export_height, dpi = 150, bg = "white", device = "png")
      }
    )
    
    output$global_met_download_pdf <- downloadHandler(
      filename = function() paste0("global_methylation_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(cached_global_met_plot(), input$global_met_export_width, input$global_met_export_height)
        ggplot2::ggsave(file, cached_global_met_plot(), width = input$global_met_export_width,
                        height = input$global_met_export_height, bg = "white", device = "pdf")
      }
    )
    
    output$global_met_download_svg <- downloadHandler(
      filename = function() paste0("global_plot_", Sys.Date(), ".svg"),
      content = function(file) {
        req(cached_global_met_plot(), input$global_met_export_width, input$global_met_export_height)
        ggplot2::ggsave(file, cached_global_met_plot(), width = input$global_met_export_width,
                        height = input$global_met_export_height, bg = "white", device = svglite::svglite)
      }
    )
    
    # --- DIFFERENTIAL METHYLATION LOGIC ---
    # Run Analysis sets up the comparison and draws the density plot. DMPs, DMRs,
    # DMGs and gene sets are separate worker jobs started from their own tabs, all
    # on the comparison frozen here, so the user only waits for what they ask for.
    diff_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_diff_setup", args, app_dir, session_dir = DIRS$analysis)
    })
    dmp_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_diff_dmps", args, app_dir, session_dir = DIRS$analysis)
    })
    dmr_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_diff_dmrs", args, app_dir, session_dir = DIRS$analysis)
    })
    dmg_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_diff_dmgs", args, app_dir, session_dir = DIRS$analysis)
    })

    # What the last Run Analysis compared. Every step runs on it, and a step's
    # result is hidden once a newer Run Analysis replaces it.
    diff_snapshot <- reactiveVal(NULL)

    # What every step needs to rebuild the two groups in the worker.
    diff_base_args <- function(snap) {
      list(
        beta_path      = beta_rds_path(),
        targets        = snap$targets,
        cache_dir      = DIRS$cache,
        pathways_dir   = DIRS$pathways,
        annotation_pkg = annotation_pkg(),
        gene_set       = cfg$gene_set,
        id_col         = snap$id_col,
        comparison_col = snap$comparison_col,
        baseline       = snap$baseline,
        comparison     = snap$comparison,
        out_dir        = DIRS$differential
      )
    }

    observeEvent(input$diff_met_run_analysis, {
      req(beta_merged(), targets_merged(), input$diff_met_id_col)
      validate(need(file.exists(beta_rds_path()),
                    "Beta matrix file not found on disk; please reload the data."))

      # Checked here rather than in the worker, so the user is told at once.
      missing <- c(
        if (length(input$diff_met_comparison_col) == 0) "a comparison column",
        if (length(input$diff_met_baseline) == 0)       "at least one Baseline level",
        if (length(input$diff_met_comparison) == 0)     "at least one Comparison level"
      )
      if (length(missing) > 0) {
        showNotification(
          paste0("Select ", paste(missing, collapse = ", "), " before running."),
          type = "warning", duration = 8
        )
        return()
      }

      queued <- m4a_queue_message()
      showNotification(
        if (is.null(queued)) "Setting up the comparison..." else queued,
        type = "message", duration = 5
      )

      snap <- list(
        run            = (diff_snapshot()$run %||% 0L) + 1L,
        targets        = targets_merged(),
        id_col         = input$diff_met_id_col,
        comparison_col = input$diff_met_comparison_col,
        baseline       = input$diff_met_baseline,
        comparison     = input$diff_met_comparison
      )
      diff_snapshot(snap)
      diff_task$invoke(
        args = c(diff_base_args(snap),
                 list(palette_dir  = palette_dirs(),
                      palette_name = input$diff_met_color_palette)),
        app_dir = app_dir
      )
    })

    # Which comparison run each step's result was made for.
    step_run <- list(dmp = reactiveVal(NULL), dmr = reactiveVal(NULL), dmg = reactiveVal(NULL))

    diff_step_invoke <- function(task, step, extra, msg) {
      if (!identical(diff_task$status(), "success")) {
        showNotification("Press Run Analysis first to set up the comparison.",
                         type = "warning", duration = 6)
        return(invisible(FALSE))
      }
      queued <- m4a_queue_message()
      showNotification(if (is.null(queued)) msg else queued, type = "message", duration = 5)
      snap <- diff_snapshot()
      if (is.null(step)) enr_params(list(run = snap$run)) else step_run[[step]](snap$run)
      task$invoke(args = c(diff_base_args(snap), extra), app_dir = app_dir)
      invisible(TRUE)
    }

    observeEvent(input$dmp_run, diff_step_invoke(
      dmp_task, "dmp",
      list(method = if (is_epicv2()) "limma" else input$dmp_method, fdr_max = DIFF_FDR_MAX),
      "Computing DMPs..."))
    observeEvent(input$dmr_run, diff_step_invoke(
      dmr_task, "dmr",
      list(method = if (is_epicv2()) "dmrcate" else "champ"),
      "Computing DMRs..."))
    observeEvent(input$dmg_run, diff_step_invoke(
      dmg_task, "dmg",
      list(region = M4A_REGIONS[[input$dmg_region]]$group),
      "Computing DMGs..."))

    # A button is disabled while its own job runs.
    for (b in list(list("diff_met_run_analysis", diff_task), list("dmp_run", dmp_task),
                   list("dmr_run", dmr_task), list("dmg_run", dmg_task))) {
      local({
        id <- b[[1]]; task <- b[[2]]
        observe({
          if (identical(task$status(), "running")) shinyjs::disable(id) else shinyjs::enable(id)
        })
      })
    }

    # One place decides what a step's tab shows: a hint before it runs, a spinner
    # while it runs, the error if it failed, a prompt if the comparison changed
    # since, and otherwise the result.
    step_state <- function(task, run_id, what, button) {
      status <- task$status()
      if (identical(status, "running")) {
        return(list(state = "running", msg = paste0("Computing ", what, "...")))
      }
      setup <- diff_task$status()
      if (identical(setup, "initial")) {
        return(list(state = "idle",
                    msg = "Choose the groups in the sidebar and press Run Analysis first."))
      }
      if (identical(setup, "running")) {
        return(list(state = "idle", msg = paste0("Setting up the comparison. Then press ", button, ".")))
      }
      if (identical(setup, "error")) {
        return(list(state = "idle", msg = "Run Analysis failed; see the Density Plot tab."))
      }
      if (identical(status, "initial")) {
        return(list(state = "idle", msg = paste0("Press ", button, " to compute ", what, ".")))
      }
      res <- tryCatch(task$result(), error = function(e) e)
      if (inherits(res, "error")) {
        return(list(state = "error", msg = paste0(what, " failed: ", m4a_error_text(res))))
      }
      if (!identical(run_id, diff_snapshot()$run)) {
        return(list(state = "stale",
                    msg = paste0("The comparison changed since these ", what,
                                 " were computed. Press ", button, " again.")))
      }
      list(state = "done", result = res)
    }

    setup_state <- reactive({
      status <- diff_task$status()
      if (identical(status, "initial")) {
        return(list(state = "idle", msg = "Choose the groups in the sidebar and press Run Analysis."))
      }
      if (identical(status, "running")) {
        return(list(state = "running",
                    msg = "Setting up the comparison and drawing the density plot..."))
      }
      res <- tryCatch(diff_task$result(), error = function(e) e)
      if (inherits(res, "error")) {
        return(list(state = "error", msg = paste("Run Analysis failed:", m4a_error_text(res))))
      }
      list(state = "done", result = res)
    })

    # Nothing while running or done: the progress bar covers a running job.
    step_status_ui <- function(st) {
      if (st$state %in% c("done", "running")) return(NULL)
      col <- switch(st$state, error = "#dc3545", stale = "#fd7e14", "#6c757d")
      div(
        class = "card p-3 mb-3",
        style = paste0("border-left: 3px solid ", col, ";"),
        span(style = "font-size: 0.9rem;", icon("circle-info"), " ", st$msg)
      )
    }

    step_result <- function(st) {
      req(identical(st$state, "done"))
      st$result
    }

    dmp_state <- reactive(step_state(dmp_task, step_run$dmp(), "DMPs", "Run DMPs"))
    dmr_state <- reactive(step_state(dmr_task, step_run$dmr(), "DMRs", "Run DMRs"))
    dmg_state <- reactive({
      st <- step_state(dmg_task, step_run$dmg(), "DMGs", "Run DMGs")
      if (identical(st$state, "done") &&
          !identical(st$result$region, M4A_REGIONS[[input$dmg_region]]$group)) {
        st <- list(state = "stale", msg = "Press Run DMGs to compute the genes for the selected region.")
      }
      st
    })

    output$diff_setup_status <- renderUI(step_status_ui(setup_state()))
    output$dmp_status        <- renderUI(step_status_ui(dmp_state()))
    output$dmr_status        <- renderUI(step_status_ui(dmr_state()))
    output$dmg_status        <- renderUI(step_status_ui(dmg_state()))

    output$dmr_method_note <- renderUI({
      if (is_epicv2()) {
        span(class = "text-muted", "Method: DMRcate on hg38, with EPICv2 replicate probes remapped. Takes several minutes.")
      } else {
        span(class = "text-muted", "Method: ChAMP ProbeLasso. Slow, often tens of minutes.")
      }
    })

    # Dynamic Export Buttons based on active tab
    output$diff_met_export_buttons <- renderUI({
      req(input$diff_met_tabset)
      
      active_tab <- input$diff_met_tabset
      
      switch(active_tab,
             "density" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export Plot"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_density_png"), " PNG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_density_pdf"), " PDF",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_density_svg"), " SVG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "dmps" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export DMPs"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_dmps_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_dmps_xlsx"), " Excel",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "dmrs" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export DMRs"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_dmrs_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_dmrs_xlsx"), " Excel",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "dmgs" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export DMGs"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_dmgs_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_dmgs_xlsx"), " Excel",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "enrichment" = {
               ids <- if (identical(input$enr_method, "missmethyl")) {
                 c("diff_met_download_gst_csv", "diff_met_download_gst_xlsx")
               } else {
                 coll <- if (is.null(input$enr_collection)) "gobp" else input$enr_collection
                 paste0("diff_met_download_", coll, c("_csv", "_xlsx"))
               }
               div(
                 p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                   icon("download", style = "font-size: 0.75rem;"), " Export Enrichment Results"),
                 div(
                   class = "d-flex gap-2",
                   downloadButton(ns(ids[1]), " CSV",
                                  class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                   downloadButton(ns(ids[2]), " Excel",
                                  class = "btn btn-sm btn-outline-secondary flex-grow-1")
                 )
               )
             },
             
             # Default fallback
             div(
               p(class = "text-muted mb-2", style = "font-size: 0.75rem;",
                 "Select a tab to see export options")
             )
      )
    })
    
    # Density Plot Exports - Fetch pre-saved files
    output$diff_met_download_density_png <- downloadHandler(
      filename = function() {
        paste0("density_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("density_plot_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_density_pdf <- downloadHandler(
      filename = function() {
        paste0("density_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("density_plot_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )

    output$diff_met_download_density_svg <- downloadHandler(
      filename = function() {
        paste0("density_plot_", Sys.Date(), ".svg")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("density_plot_", Sys.Date(), ".svg"))
        validate(need(file.exists(src), "SVG file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    # DMP Exports - Fetch pre-saved files
    output$diff_met_download_dmps_csv <- downloadHandler(
      filename = function() {
        paste0("dmps_", step_result(dmp_state())$method, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmps_", step_result(dmp_state())$method, "_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_dmps_xlsx <- downloadHandler(
      filename = function() {
        paste0("dmps_", step_result(dmp_state())$method, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmps_", step_result(dmp_state())$method, "_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    # DMR Exports - Fetch pre-saved files
    output$diff_met_download_dmrs_csv <- downloadHandler(
      filename = function() {
        paste0("dmrs_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmrs_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_dmrs_xlsx <- downloadHandler(
      filename = function() {
        paste0("dmrs_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmrs_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    # DMG Exports - Fetch pre-saved files
    output$diff_met_download_dmgs_csv <- downloadHandler(
      filename = function() {
        paste0("dmgs_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmgs_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_dmgs_xlsx <- downloadHandler(
      filename = function() {
        paste0("dmgs_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmgs_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    # FGSEA GOBP Exports - Fetch pre-saved files
    output$diff_met_download_gobp_csv <- downloadHandler(
      filename = function() {
        paste0("fgsea_gobp_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_gobp_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_gobp_xlsx <- downloadHandler(
      filename = function() {
        paste0("fgsea_gobp_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_gobp_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    # FGSEA KEGG Exports - Fetch pre-saved files
    output$diff_met_download_kegg_csv <- downloadHandler(
      filename = function() {
        paste0("fgsea_kegg_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_kegg_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_kegg_xlsx <- downloadHandler(
      filename = function() {
        paste0("fgsea_kegg_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_kegg_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    # FGSEA Hallmark Exports - Fetch pre-saved files
    output$diff_met_download_hallmark_csv <- downloadHandler(
      filename = function() {
        paste0("fgsea_hallmark_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_hallmark_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_hallmark_xlsx <- downloadHandler(
      filename = function() {
        paste0("fgsea_hallmark_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("fgsea_hallmark_", Sys.Date(), ".xlsx"))
        validate(need(file.exists(src), "XLSX file not ready. Please run FGSEA analysis first."))
        file.copy(src, file)
      }
    )
    
    # Moving the FDR / logFC / row sliders only re-filters: the fit already ran
    # once at DIFF_FDR_MAX, so limma or ChAMP are not re-run on every drag.
    diff_filtered_dmps <- reactive({
      dmps <- step_result(dmp_state())$dmps_all
      req(is.data.frame(dmps))
      if (nrow(dmps) == 0) return(dmps)

      fdr_col <- intersect(c("adj.P.Val", "adj.P.value", "adjPVal", "FDR"), names(dmps))
      if (length(fdr_col) > 0) {
        dmps <- dmps[!is.na(dmps[[fdr_col[1]]]) & dmps[[fdr_col[1]]] <= input$diff_met_fdr_cut, ,
                     drop = FALSE]
      }
      if ("logFC" %in% names(dmps)) {
        dmps <- dmps[abs(dmps$logFC) > input$diff_met_lfc_cut, , drop = FALSE]
      }
      dmps
    })

    # Density plot render (produced by the worker; shown as the saved PNG)
    output$diff_met_density_plot <- renderImage({
      res <- step_result(setup_state())
      validate(need(!is.null(res$density_png) && file.exists(res$density_png),
                    "Density plot not available."))
      list(src = res$density_png, contentType = "image/png", width = "100%")
    }, deleteFile = FALSE)

    # The tables render only once their step is done; until then the status card
    # above each one says why it is empty.
    output$diff_met_dmp_table <- DT::renderDataTable({
      req(input$diff_dmps_top_cpgs)
      dmps <- diff_filtered_dmps()
      make_dt(if (nrow(dmps) > 0) head(dmps, input$diff_dmps_top_cpgs) else dmps)
    })

    output$diff_met_dmr_table <- DT::renderDataTable({
      make_dt(step_result(dmr_state())$dmrs)
    })

    output$diff_met_dmg_table <- DT::renderDataTable({
      dmgs <- step_result(dmg_state())$dmgs
      cut  <- if (is.null(input$diff_met_dmg_lfc_cut) || is.na(input$diff_met_dmg_lfc_cut)) {
        0
      } else {
        input$diff_met_dmg_lfc_cut
      }
      if (is.data.frame(dmgs) && nrow(dmgs) > 0 && "logFC" %in% names(dmgs) && cut > 0) {
        dmgs <- dmgs[abs(dmgs$logFC) > cut, , drop = FALSE]
      }
      make_dt(dmgs)
    })

    # One table for both gene-set methods; each has its own task.
    output$diff_met_enrichment_table <- DT::renderDataTable({
      make_dt(step_result(enr_view())$table)
    })

    # Explains the two routes at the point where the choice is made.
    # Side-by-side routes: the one in use is lit, the other is there so the
    # reader can see what the choice above actually changes.
    output$enr_explainer <- renderUI({
      missmethyl <- identical(input$enr_method, "missmethyl")

      flow <- function(steps) {
        items <- list()
        for (i in seq_along(steps)) {
          if (i > 1) items <- c(items, list(span(class = "m4a-arrow", "\u2192")))
          items <- c(items, list(span(class = "m4a-chip", steps[i])))
        }
        div(class = "m4a-flow", items)
      }

      route <- function(title, subtitle, steps, body, active) {
        div(
          class = paste("m4a-route m4a-route-equal",
                        if (active) "m4a-route-on" else "m4a-route-off"),
          div(span(class = "m4a-route-title", title),
              span(class = "m4a-route-sub", " \u00b7 ", subtitle)),
          flow(steps),
          p(class = "m4a-route-body", body)
        )
      }

      div(
        class = "mb-3",
        div(
          class = "row g-2",
          div(class = "col-md-6",
              route("All CpGs", "FGSEA",
                    c("every probe in the region", "gene median", "limma on genes",
                      "genes ranked by logFC", "FGSEA"),
                    paste("Every probe in the region is summarised to one median value per gene and",
                          "limma is fitted on those values, so every gene keeps a score."),
                    !missmethyl)),
          div(class = "col-md-6",
              route("Significant CpGs", "missMethyl",
                    c("CpGs past the thresholds", "genes in the region",
                      "gene-set test", "corrected for probes per gene"),
                    paste("Only the CpGs passing the current FDR and logFC thresholds are mapped to genes,",
                          "and each gene set is tested for over-representation among them. A gene covered",
                          "by 60 probes is likelier to contain a significant CpG than one covered by 3, so",
                          "missMethyl corrects for the number of probes per gene."),
                    missmethyl))
        ),
        p(class = "text-muted m4a-foot mt-2 mb-0",
          "The two answer different questions. Where they disagree, the signal usually sits outside the region tested, or the thresholds are doing the work.")
      )
    })

    # --- missMethyl gene-set testing ---------------------------------------
    # Its own task: it runs on the DMPs as currently filtered by the FDR and
    # logFC sliders, so it must not require re-running the whole fit.
    gst_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_missmethyl_gst", args, app_dir, session_dir = DIRS$analysis)
    })

    # Regions, defined exactly as methylation_buildannot() defines them for the
    # gene-median route: Promoter200 = TSS200 + 1stExon + 5'UTR, and ExonBnd is
    # counted as body. Same words, same CpGs, whichever branch is used.
    M4A_REGIONS <- list(
      promoter     = list(features = c("TSS200", "1stExon", "5\u0027UTR"),
                          group    = "TSS200",
                          label    = "Promoter",
                          note     = "TSS200, 1st exon and 5\u2032UTR \u2014 the region Met4All summarises to a gene median."),
      promoter1500 = list(features = c("TSS1500", "TSS200", "1stExon", "5\u0027UTR"),
                          group    = "TSS1500",
                          label    = "Extended promoter",
                          note     = "The promoter plus TSS1500, so up to 1.5 kb upstream of the transcription start site."),
      body         = list(features = c("Body", "ExonBnd"),
                          group    = "Body",
                          label    = "Gene body",
                          note     = "Everything downstream of the first exon, exon boundaries included."),
      all          = list(features = "ALL",
                          group    = "All",
                          label    = "Whole gene",
                          note     = "Every CpG annotated to the gene, promoter and body together.")
    )

    # A gene drawn left to right, with the parts a region covers lit up. The
    # segments are the Illumina annotation groups, in their order along the gene.
    # Shared by the enrichment tab and the DMGs tab, so both show the same picture.
    m4a_gene_map <- function(region_id) {
      # name, relative width, and whether each region includes it
      segs <- list(
        list(id = "TSS1500",  w = 2.0),
        list(id = "TSS200",   w = 1.0),
        list(id = "5\u2032UTR",   w = 0.9),
        list(id = "1st exon", w = 1.1),
        list(id = "Body",     w = 4.0),
        list(id = "3\u2032UTR",   w = 0.9)
      )
      lit <- switch(region_id,
                    promoter     = c("TSS200", "5\u2032UTR", "1st exon"),
                    promoter1500 = c("TSS1500", "TSS200", "5\u2032UTR", "1st exon"),
                    body         = c("Body"),
                    all          = vapply(segs, function(s) s$id, character(1)),
                    character(0))

      tagList(
        div(
          class = "m4a-ticks",
          lapply(segs, function(s) {
            span(class = paste("m4a-tick", if (identical(s$id, "TSS200")) "m4a-tick-tss" else ""),
                 style = sprintf("flex-grow: %s;", s$w),
                 if (identical(s$id, "TSS200")) "TSS \u25be" else "")
          })
        ),
        div(
          class = "m4a-gene",
          lapply(segs, function(s) {
            div(class = paste("m4a-seg", if (s$id %in% lit) "m4a-seg-on" else ""),
                style = sprintf("flex-grow: %s;", s$w),
                s$id)
          })
        )
      )
    }

    output$enr_region_note <- renderUI({
      reg <- M4A_REGIONS[[input$enr_region]]
      req(!is.null(reg))
      div(
        class = "mb-2",
        m4a_gene_map(input$enr_region),
        p(class = "text-muted mb-0", style = "font-size: 0.85rem;",
          strong(reg$label), ": ", reg$note)
      )
    })

    # How the DMGs are calculated, for the region selected in the DMGs tab.
    output$dmg_source_note <- renderUI({
      reg <- M4A_REGIONS[[input$dmg_region]]
      req(!is.null(reg))

      div(
        class = "m4a-route m4a-route-on mb-3",
        div(span(class = "m4a-route-title", "How these genes are calculated")),
        div(class = "m4a-flow",
            span(class = "m4a-chip", paste0(reg$label, " probes")),
            span(class = "m4a-arrow", "\u2192"),
            span(class = "m4a-chip", "median per gene"),
            span(class = "m4a-arrow", "\u2192"),
            span(class = "m4a-chip", "limma on genes"),
            span(class = "m4a-arrow", "\u2192"),
            span(class = "m4a-chip", "FDR across genes")),
        m4a_gene_map(input$dmg_region),
        p(class = "m4a-route-body text-muted mt-2", strong(reg$label), ": ", reg$note),
        p(class = "m4a-route-body text-muted mb-0",
          "\u0022Probes in region\u0022 is how many probes each gene\u0027s median is based on, and ",
          "\u0022Probe span (bp)\u0022 is the distance from the first to the last of them, a lower ",
          "bound on the region, not its annotated length. The export contains every gene; the ",
          "Min. gene |logFC| filter above changes the view only. Gene medians have smaller ",
          "logFCs than single CpGs, so it is separate from the CpG cut-off in the sidebar.")
      )
    })

    # missMethyl's array.type. EPICv2-only runs keep native EPICv2 IDs; a merged
    # run has already been mapped to EPICv1 or 450K IDs upstream.
    gst_array <- reactive({
      if (is_epicv2()) return("EPIC_V2")
      arrs <- unlist(array_names())
      if (is.null(arrs)) "auto" else if ("EPIC" %in% arrs) "EPIC" else "450K"
    })

    # The gene-median branch: its own task, because the region changes the gene
    # matrix itself, so limma and fgsea have to be redone for it.
    enr_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("run_gene_set_fgsea", args, app_dir, session_dir = DIRS$analysis)
    })

    # Which differential run (and, for missMethyl, which thresholds) each gene-set
    # result was made for. A result is only shown while those still hold.
    enr_params <- reactiveVal(NULL)
    gst_params <- reactiveVal(NULL)

    observeEvent(input$enr_run, {
      if (!identical(input$enr_method, "missmethyl")) {
        diff_step_invoke(enr_task, NULL, list(region     = M4A_REGIONS[[input$enr_region]]$group,
                                              collection = input$enr_collection),
                         "Running gene-set enrichment on all CpGs...")
        return()
      }

      # missMethyl tests the significant CpGs, so it needs current DMPs.
      dmp <- dmp_state()
      if (!identical(dmp$state, "done")) {
        showNotification("Run DMPs first: missMethyl tests the significant CpGs.",
                         type = "warning", duration = 6)
        return()
      }
      dmps <- diff_filtered_dmps()
      if (!is.data.frame(dmps) || nrow(dmps) == 0 || !"CpG" %in% names(dmps)) {
        showNotification("No DMPs pass the current thresholds.", type = "warning", duration = 6)
        return()
      }

      queued <- m4a_queue_message()
      showNotification(
        if (is.null(queued)) "Running missMethyl gene-set testing..." else queued,
        type = "message", duration = 5
      )

      gst_params(list(run = diff_snapshot()$run,
                      fdr = input$diff_met_fdr_cut, lfc = input$diff_met_lfc_cut))
      gst_task$invoke(
        args = list(
          sig_cpg          = dmps$CpG,
          all_cpg_path     = dmp$result$all_cpg_path,
          collection       = input$enr_collection,
          genomic_features = M4A_REGIONS[[input$enr_region]]$features,
          array_type       = gst_array(),
          sig_genes        = isTRUE(input$gst_sig_genes),
          cache_dir        = DIRS$cache,
          pathways_dir     = DIRS$pathways,
          annotation_pkg   = annotation_pkg(),
          gene_set         = cfg$gene_set,
          out_dir          = DIRS$differential
        ),
        app_dir = app_dir
      )
    })

    observe({
      busy <- identical(gst_task$status(), "running") ||
        identical(enr_task$status(), "running")
      if (busy) shinyjs::disable("enr_run") else shinyjs::enable("enr_run")
    })

    # The selected method's result, or why there is none. A result also goes
    # stale when the selectors (or, for missMethyl, the thresholds) no longer
    # match what was run.
    enr_view <- reactive({
      mm <- identical(input$enr_method, "missmethyl")
      st <- if (mm) {
        step_state(gst_task, gst_params()$run, "missMethyl gene-set tests", "Run gene-set analysis")
      } else {
        step_state(enr_task, enr_params()$run, "gene-set enrichment", "Run gene-set analysis")
      }
      if (identical(st$state, "done")) {
        res  <- st$result
        coll <- if (is.null(input$enr_collection)) "gobp" else input$enr_collection
        reg  <- M4A_REGIONS[[input$enr_region]]
        same <- identical(res$collection, coll) && if (mm) {
          identical(res$genomic_features, reg$features) &&
            identical(gst_params()$fdr, input$diff_met_fdr_cut) &&
            identical(gst_params()$lfc, input$diff_met_lfc_cut)
        } else {
          identical(res$region, reg$group)
        }
        if (!same) {
          st <- list(state = "stale", msg = paste0(
            "The selection", if (mm) " or the FDR/logFC thresholds",
            " changed since the last run. Press Run gene-set analysis."))
        }
      }
      st
    })

    output$enr_status <- renderUI(step_status_ui(enr_view()))

    output$gst_summary <- renderUI({
      res <- step_result(enr_view())
      txt <- if (identical(input$enr_method, "missmethyl")) {
        sprintf("%s gene sets | CpGs from: %s | %s significant of %s tested CpGs | array: %s",
                toupper(res$collection), paste(res$genomic_features, collapse = ", "),
                format(res$n_sig, big.mark = ","), format(res$n_all, big.mark = ","),
                res$array_type)
      } else {
        sprintf("%s gene sets | %s region | %s genes ranked by logFC",
                toupper(res$collection), M4A_REGIONS[[input$enr_region]]$label,
                format(res$n_genes, big.mark = ","))
      }
      p(class = "text-muted", style = "font-size: 0.85rem;", txt)
    })

    gst_file <- function(ext) {
      res <- step_result(step_state(gst_task, gst_params()$run, "missMethyl gene-set tests",
                                    "Run gene-set analysis"))
      src <- file.path(DIRS$differential, paste0(res$file_stem, ".", ext))
      validate(need(file.exists(src), "The file is not ready. Please run missMethyl first."))
      src
    }
    output$diff_met_download_gst_csv <- downloadHandler(
      filename = function() paste0("missmethyl_", Sys.Date(), ".csv"),
      content  = function(file) file.copy(gst_file("csv"), file)
    )
    output$diff_met_download_gst_xlsx <- downloadHandler(
      filename = function() paste0("missmethyl_", Sys.Date(), ".xlsx"),
      content  = function(file) file.copy(gst_file("xlsx"), file)
    )
    
    
    # Populate group checkboxes when comparison column changes for Differential
    observeEvent(input$diff_met_comparison_col, {
      req(input$diff_met_comparison_col)
      
      raw_vals <- na.omit(as.character(targets_merged()[[input$diff_met_comparison_col]]))
      
      validate(
        need(length(raw_vals) > 0,
             paste0("Column '", input$diff_met_comparison_col, "' has no non-missing values. Please choose a different column."))
      )
      
      counts <- table(raw_vals)
      levels <- sort(names(counts))
      labels <- paste0(levels, " (", as.numeric(counts[levels]), " ",
                       ifelse(as.numeric(counts[levels]) == 1, "sample", "samples"), ")")
      
      updateCheckboxGroupInput(session, "diff_met_baseline", choices = setNames(levels, labels), selected = NULL)
      updateCheckboxGroupInput(session, "diff_met_comparison", choices = setNames(levels, labels), selected = NULL)
    })    
    
    # --- CNV LOGIC ---
    # Bumped by the Run button once its inputs validate; the worker task below
    # keys off it. This was previously used without ever being declared, which
    # stayed hidden only because the old eventReactive consuming it was lazy.
    run_analysis_trigger <- reactiveVal(0)

    # BED path and the two rendered plot paths
    cnv_bed_path      <- reactiveVal(NULL)
    cached_pileup_png <- reactiveVal(NULL)
    cached_sample_png <- reactiveVal(NULL)

    # Dynamic Export Buttons based on active tab
    observeEvent(input$cnv_bed_file, {
      req(input$cnv_bed_file)
      
      tryCatch({
        file_info <- input$cnv_bed_file
        in_path <- file_info$datapath
        file_name <- file_info$name
        
        out_path <- file.path(DIRS$cnv, file_name)
        
        file.copy(from = in_path, to = out_path, overwrite = TRUE)
        
        # Store path in reactive
        cnv_bed_path(normalizePath(out_path))
        
        showNotification("BED file processed successfully", type = "message")
      }, error = function(e) {
        error_msg <- e$message
        showNotification(paste("Error processing BED file:", error_msg), type = "error", duration = 5)
      })
    })
    
    # Run analysis when button is clicked
    observeEvent(input$cnv_run_analysis, {
      # Validate required inputs with proper error notifications
      if (is.null(cnv_bed_path()) || !file.exists(cnv_bed_path())) {
        showNotification("Please upload a valid BED file first", type = "error", 
                         duration = 5) 
        return()
      }
      
      if (is.null(input$cnv_baseline) || length(input$cnv_baseline) == 0) {
        showNotification("Please select at least one baseline group",  type = "error", 
                         duration = 5)
        return()
      }
      
      if (is.null(input$cnv_comparison) || length(input$cnv_comparison) == 0) {
        showNotification("Please select at least one comparison group", type = "error", 
                         duration = 5) 
        return()
      }
      
      # Also validate that baseline and comparison don't overlap
      if (any(input$cnv_baseline %in% input$cnv_comparison)) {
        overlapping <- intersect(input$cnv_baseline, input$cnv_comparison)
        showNotification( paste("Groups cannot overlap. Overlapping levels:", 
                                paste(overlapping, collapse = ", ")), type = "error", 
                          duration = 5) 
        return()
      }
      
      # All validations passed
      run_analysis_trigger(run_analysis_trigger() + 1)
    })
    
    # Prepare data in a worker process
    cnv_task <- ExtendedTask$new(function(args, app_dir) {
      m4a_submit("prepare_cnv_data", args, app_dir, session_dir = DIRS$analysis)
    })

    output$download_log <- m4a_log_download_handler(DIRS$analysis, DIRS$analysis_id)

    app_dir <- normalizePath(getwd())

    # One progress panel, driven by whichever task is running
    for (tsk in c("mds_task", "pca_task", "umap_task", "predict_task",
                  "heatmap_task", "global_task", "diff_task", "dmp_task",
                  "dmr_task", "dmg_task", "gst_task", "enr_task", "cnv_task")) {
      local({
        nm <- tsk
        observe({
          t <- get0(nm, inherits = TRUE)
          req(!is.null(t), identical(t$status(), "running"))
          m4a_render_progress(output, session, t, DIRS$analysis)
        })
      })
    }

    observeEvent(run_analysis_trigger(), {
      req(run_analysis_trigger() > 0)
      req(input$cnv_array_select,
          input$cnv_comparison_col, cnv_bed_path(),
          input$cnv_baseline, input$cnv_comparison)

      if (is.null(array_names())) {
        showNotification("CNVs cannot be calculated from a beta matrix only; IDATs are required.",
                         type = "error", duration = 5)
        return()
      }

      queued <- m4a_queue_message()
      showNotification(
        if (is.null(queued)) "Running CNV analysis..." else queued,
        type = "message", duration = 5
      )

      cnv_task$invoke(
        args = list(
          mset_list      = mSetSq_list(),
          array_type     = input$cnv_array_select,
          bed_path       = cnv_bed_path(),
          chrXY          = input$cnv_include_xy,
          comparison_col = input$cnv_comparison_col,
          baseline       = input$cnv_baseline,
          comparison     = input$cnv_comparison,
          sample_groups  = setNames(as.character(cnv_targets()[[input$cnv_comparison_col]]),
                                    cnv_sample_ids(cnv_targets())),
          cache_dir      = DIRS$cache
        ),
        app_dir = app_dir
      )
    })

    # Keep the Run button honest about what the server is doing.
    observe({
      if (identical(cnv_task$status(), "running")) {
        shinyjs::disable("cnv_run_analysis")
      } else {
        shinyjs::enable("cnv_run_analysis")
      }
    })

    cnv_data <- reactive({
      status <- cnv_task$status()
      validate(need(status != "initial", "Configure the parameters and press Run Analysis."))
      validate(need(status != "running", "CNV analysis running..."))
      tryCatch(
        cnv_task$result(),
        error = function(e) {
          validate(need(FALSE, paste0("Error preparing CNV data: ", m4a_error_text(e))))
          NULL
        }
      )
    })
    
    # Generate pile-up plot when data is ready
    observeEvent(cnv_data(), {
      req(cnv_data(), input$cnv_baseline, input$cnv_comparison)
      
      tryCatch({
        png_path <- plot_pile_up(
          cnv_data = cnv_data(),
          baseline = input$cnv_baseline,
          comparison = input$cnv_comparison,
          out_dir = DIRS$cnv
        )
        cached_pileup_png(png_path)
      }, error = function(e) {
        error_msg <- e$message
        showNotification(paste("Error generating pile-up plot:", error_msg), 
                         type = "error", duration = 5)
        cached_pileup_png(NULL)
      })
    })
    
    # Render pile-up plot
    output$cnv_pile_up_plot <- renderImage({
      req(cached_pileup_png())
      
      list(
        src = cached_pileup_png(),
        contentType = "image/png",
        width = "100%"
      )
    }, deleteFile = FALSE)
    
    # CNV groups come from the live samplesheet, so clusters added by UMAP or the
    # heatmap, and columns from a re-uploaded samplesheet, can be compared too.
    # Rows are those of the selected array's samples, matched on the sample ID
    # the MethylSet was built with.
    cnv_sample_ids <- function(tg) {
      if ("ID" %in% names(tg)) tg$ID else paste(tg$Sample_Group, tg$Sample_Name, sep = "_")
    }
    cnv_targets <- reactive({
      req(input$cnv_array_select, targets_merged())
      mset_path <- mSetSq_list()[[input$cnv_array_select]]
      req(!is.null(mset_path))
      tg <- targets_merged()
      tg[cnv_sample_ids(tg) %in% rownames(read_mset_pdata(mset_path)), , drop = FALSE]
    })

    # Update CNV comparison column based on selected array
    observe({
      cols <- colnames(cnv_targets())
      sel  <- isolate(input$cnv_comparison_col)
      updateSelectInput(session, "cnv_comparison_col", choices = cols,
                        selected = if (!is.null(sel) && sel %in% cols) sel else cols[1])
    })

    # Populate group checkboxes when comparison column changes for CNV
    observe({
      req(input$cnv_comparison_col)
      tg <- cnv_targets()
      req(input$cnv_comparison_col %in% colnames(tg))
      raw_vals <- na.omit(as.character(tg[[input$cnv_comparison_col]]))

      validate(
        need(length(raw_vals) > 0,
             paste0("Column '", input$cnv_comparison_col, "' has no non-missing values. Please choose a different column."))
      )

      counts <- table(raw_vals)
      levels <- sort(names(counts))
      labels <- paste0(levels, " (", as.numeric(counts[levels]), " ",
                       ifelse(as.numeric(counts[levels]) == 1, "sample", "samples"), ")")

      # Keep the user's picks when the samplesheet gains columns.
      keep <- function(id) intersect(isolate(input[[id]]), levels)
      updateCheckboxGroupInput(session, "cnv_baseline", choices = setNames(levels, labels),
                               selected = keep("cnv_baseline"))
      updateCheckboxGroupInput(session, "cnv_comparison", choices = setNames(levels, labels),
                               selected = keep("cnv_comparison"))
    })
    
    # Populate sample radio buttons when CNV data is ready
    observe({
      req(cnv_data())
      
      tryCatch({
        cnv <- cnv_data()
        sample_choices <- names(cnv@seg$summary)
        
        if (length(sample_choices) > 0) {
          updateRadioButtons(
            session, 
            "cnv_selected_sample", 
            choices = sample_choices,
            selected = sample_choices[1]
          )
        } else {
          updateRadioButtons(
            session, 
            "cnv_selected_sample", 
            choices = c("No samples available" = ""),
            selected = ""
          )
        }
      }, error = function(e) {
        showNotification(paste("Error updating sample choices:", e$message), 
                         type = "error", duration = 5)
      })
    })
    
    # Generate per-sample plot when sample selection changes
    observeEvent(input$cnv_selected_sample, {
      req(cnv_data(), input$cnv_selected_sample, input$cnv_selected_sample != "")
      
      tryCatch({
        png_path <- plot_cnv_per_sample(
          cnv_data = cnv_data(),
          sample_name = input$cnv_selected_sample,
          baseline = input$cnv_baseline,
          comparison = input$cnv_comparison,
          out_dir = DIRS$cnv
        )
        cached_sample_png(png_path)
      }, error = function(e) {
        error_msg <- e$message
        showNotification(paste("Error generating sample plot:", error_msg), 
                         type = "error", duration = 5)
        cached_sample_png(NULL)
      })
    })
    
    # Render per-sample plot
    output$cnv_per_sample_plot <- renderImage({
      req(cached_sample_png())
      
      list(
        src = cached_sample_png(),
        contentType = "image/png",
        width = "100%"
      )
    }, deleteFile = FALSE)
    
    # Dynamic Export Buttons based on active tab
    output$cnv_export_buttons <- renderUI({
      req(input$cnv_tabset)
      
      active_tab <- input$cnv_tabset
      
      switch(active_tab,
             "pileup" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export Pile-up Plot"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("cnv_download_pileup_png"), " PNG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("cnv_download_pileup_pdf"), " PDF",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("cnv_download_pileup_svg"), " SVG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "persample" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export Sample Plot"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("cnv_download_sample_png"), " PNG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("cnv_download_sample_pdf"), " PDF",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("cnv_download_sample_svg"), " SVG",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             # Default fallback
             div(
               p(class = "text-muted mb-2", style = "font-size: 0.75rem;",
                 "Select a tab to see export options")
             )
      )
    })
    
    # Pile-up plot download handlers
    output$cnv_download_pileup_png <- downloadHandler(
      filename = function() {
        paste0("cnv_pileup_", Sys.Date(), ".png")
      },
      content = function(file) {
        src <- file.path(DIRS$cnv, paste0("cnv_pileup_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$cnv_download_pileup_pdf <- downloadHandler(
      filename = function() {
        paste0("cnv_pileup_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        src <- file.path(DIRS$cnv, paste0("cnv_pileup_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )

    output$cnv_download_pileup_svg <- downloadHandler(
      filename = function() {
        paste0("cnv_pileup_", Sys.Date(), ".svg")
      },
      content = function(file) {
        src <- file.path(DIRS$cnv, paste0("cnv_pileup_", Sys.Date(), ".svg"))
        validate(need(file.exists(src), "SVG file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    # Per-sample plot download handlers
    output$cnv_download_sample_png <- downloadHandler(
      filename = function() {
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(input$cnv_selected_sample, input$cnv_selected_sample != "")
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        src <- file.path(DIRS$cnv, paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready. Please select a sample."))
        file.copy(src, file)
      }
    )
    
    output$cnv_download_sample_pdf <- downloadHandler(
      filename = function() {
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(input$cnv_selected_sample, input$cnv_selected_sample != "")
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        src <- file.path(DIRS$cnv, paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready. Please select a sample."))
        file.copy(src, file)
      }
    )

    output$cnv_download_sample_svg <- downloadHandler(
      filename = function() {
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".svg")
      },
      content = function(file) {
        req(input$cnv_selected_sample, input$cnv_selected_sample != "")
        sample_safe <- gsub("[^A-Za-z0-9]", "_", input$cnv_selected_sample)
        src <- file.path(DIRS$cnv, paste0("cnv_sample_", sample_safe, "_", Sys.Date(), ".svg"))
        validate(need(file.exists(src), "SVG file not ready. Please select a sample."))
        file.copy(src, file)
      }
    )
    
    # --- SAMPLESHEET LOGIC ---
    targets_original <- reactiveVal(NULL)
    
    # Store original on first load
    observe({
      req(targets_merged())
      if (is.null(targets_original())) {
        targets_original(targets_merged())
      }
    })
    
    output$samplesheet_table <- DT::renderDataTable({
      req(targets_merged())
      df <- cbind(SampleID = rownames(targets_merged()), targets_merged())
      make_dt(df, editable = TRUE)
    })
    
    observeEvent(input$samplesheet_table_cell_edit, {
      info <- input$samplesheet_table_cell_edit
      updated <- targets_merged()
      # col index from DT is 0-based; col 0 = SampleID (read-only), col 1 = first real col
      real_col <- info$col  # because rownames=FALSE and SampleID is col 0, real cols start at 1
      updated[info$row, real_col] <- DT::coerceValue(info$value, updated[info$row, real_col])
      targets_merged(updated)
    })
    
    # --- ADD METADATA FROM A NEW SAMPLESHEET ---
    # Uploaded sheet is held here until the user applies it
    new_samplesheet <- reactiveVal(NULL)
    
    observeEvent(input$samplesheet_upload, {
      req(input$samplesheet_upload, targets_merged())
      
      tryCatch({
        df <- read_samplesheet_file(input$samplesheet_upload$datapath,
                                    input$samplesheet_upload$name)
        keys <- samplesheet_key_candidates(targets_merged(), df)
        
        if (length(keys) == 0) {
          new_samplesheet(NULL)
          showNotification(
            paste0("No usable match column found. The uploaded samplesheet needs a column with the ",
                   "same name and matching values as one in the current samplesheet ",
                   "(e.g. Sample_Name or ID)."),
            type = "error", duration = 10
          )
          return()
        }
        
        new_samplesheet(df)
        updateSelectInput(session, "samplesheet_key_col", choices = keys, selected = keys[1])
      }, error = function(e) {
        new_samplesheet(NULL)
        showNotification(paste("Could not read samplesheet:", e$message), type = "error", duration = 8)
      })
    })
    
    # Preview what applying the upload would do
    output$samplesheet_upload_status <- renderUI({
      req(new_samplesheet(), targets_merged(), input$samplesheet_key_col)
      df <- new_samplesheet()
      req(input$samplesheet_key_col %in% colnames(df))
      
      n_matched <- sum(!is.na(match_samplesheet_rows(targets_merged(), df, input$samplesheet_key_col)))
      new_cols  <- setdiff(colnames(df), colnames(targets_merged()))
      
      div(
        class = "small",
        span(class = if (n_matched == nrow(targets_merged())) "text-success" else "text-warning",
             sprintf("%d of %d samples matched. ", n_matched, nrow(targets_merged()))),
        span(class = "text-muted",
             if (length(new_cols) > 0) paste("New columns:", paste(new_cols, collapse = ", "))
             else "No new columns — tick 'Overwrite existing columns' to update the ones already present.")
      )
    })
    
    observeEvent(input$samplesheet_apply, {
      req(new_samplesheet(), targets_merged(), input$samplesheet_key_col)
      
      tryCatch({
        res <- merge_samplesheet_columns(
          targets   = targets_merged(),
          new_ss    = new_samplesheet(),
          key_col   = input$samplesheet_key_col,
          overwrite = isTRUE(input$samplesheet_overwrite)
        )
        
        if (length(res$added) == 0 && length(res$updated) == 0) {
          showNotification(
            "Nothing to add — all columns already exist. Tick 'Overwrite existing columns' to update them.",
            type = "warning", duration = 8
          )
          return()
        }
        
        targets_merged(res$targets)
        
        msg <- sprintf("%d/%d samples matched.", res$n_matched, res$n_total)
        if (length(res$added) > 0)   msg <- paste0(msg, " Added: ", paste(res$added, collapse = ", "), ".")
        if (length(res$updated) > 0) msg <- paste0(msg, " Updated: ", paste(res$updated, collapse = ", "), ".")
        if (length(res$skipped) > 0) msg <- paste0(msg, " Kept existing: ", paste(res$skipped, collapse = ", "), ".")
        showNotification(msg, type = "message", duration = 10)
      }, error = function(e) {
        showNotification(paste("Could not add metadata:", e$message), type = "error", duration = 8)
      })
    })
    
    
    # --- HELPER FUNCTIONS ---
    update_active_button <- function(active_id) {
      shinyjs::removeClass(selector = ".content-section", class = "active")
      all_buttons <- c("nav_beta_matrix", "nav_qc", "nav_mds", "nav_pca",
                       "nav_umap", "nav_heatmap", "nav_global", "nav_differential",
                       "nav_cnv", "nav_samplesheet")
      
      for (btn in all_buttons) {
        if (btn == active_id) {
          shinyjs::addClass(id = btn, class = "btn-primary")
          shinyjs::removeClass(id = btn, class = "btn-outline-primary")
        } else {
          shinyjs::removeClass(id = btn, class = "btn-primary")
          shinyjs::addClass(id = btn, class = "btn-outline-primary")
        }
      }
    }
    
    show_view <- function(view_id, title_text) {
      all_views <- c("view_beta_matrix", "view_qc", "view_mds", "view_pca",
                     "view_umap", "view_heatmap", "view_global_met",
                     "view_differential", "view_cnv", "view_samplesheet")
      
      for (view in all_views) { shinyjs::hide(view) }
      shinyjs::show(view_id)
      output$view_title <- renderText(title_text)
    }
    
  })
}
