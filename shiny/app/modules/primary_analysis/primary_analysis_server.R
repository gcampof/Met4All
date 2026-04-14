source("modules/primary_analysis/utils.R")
source("modules/primary_analysis/annotations.R")
source("modules/primary_analysis/mds/mds_server.R")
source("modules/primary_analysis/pca/pca_server.R")
source("modules/primary_analysis/umap/umap_utils.R")
source("modules/primary_analysis/heatmap/heatmap_utils.R")
source("modules/primary_analysis/global_met/global_utils.R")
source("modules/primary_analysis/differential/differential_utils.R")
source("modules/primary_analysis/cnv/cnv_utils.R")

primary_analysis_server <- function(id, load_data_return, DIRS, APP_CACHE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set up variables
    view_initialized <- reactiveVal(FALSE)
    array_names <<- load_data_return$array_names_ld
    mSetSq_list <<- load_data_return$mSetSq_list_ld
    beta_merged <<- load_data_return$beta_merged_ld
    targets_merged <<- load_data_return$targets_merged_ld
    type_selected <<- load_data_return$type_selected()
    umap_data <<- reactiveVal(NULL)
    current_view <<- reactiveVal(NULL)
    PALETTES <<- reactive({
      do.call(reactiveValues,
              prepare_color_palettes(DIRS$custom_color_palette))
    })
    message("[PRIMARY_ANALYSIS] Setup complete!")
    
    # Set qc path
    observe({
      addResourcePath(prefix = "qc_reports",
                      directoryPath = normalizePath(DIRS$qc))
    })
    
    # Disable/enable buttons based on data type (beta or idats)
    observe({
      req(length(names(input)) > 0)
      req(beta_merged() | !is.null(targets_merged()))
      
      if (!view_initialized()) {
        if (type_selected == "IDATS") {
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
    
    # Initialize metadata choices
    observe({
      req(length(names(input)) > 0)
      req(targets_merged())
      meta_cols <- colnames(targets_merged())
      
      updateSelectInput(session, "pca_color_by", choices = meta_cols, selected = meta_cols[1])
      updateSelectInput(session, "mds_color_by", choices = meta_cols, selected = meta_cols[1])
      updateSelectInput(session, "heatmap_annotation_cols", choices = meta_cols, selected = meta_cols[1])
      
      # Initializa sample ID choices dinamically (default "ID")
      default_id <- if ("ID" %in% meta_cols) "ID" else meta_cols[1]
      updateSelectInput(session, "mds_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "pca_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "umap_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "heatmap_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "global_met_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "diff_met_id_col", choices = meta_cols, selected = default_id)
      updateSelectInput(session, "cnv_id_col", choices = meta_cols, selected = default_id)
      
      # Initialize comparison ID choices dinamically (default Sample_Group)
      default_group <- if ("Sample_Group" %in% meta_cols) "Sample_Group" else meta_cols[1]
      updateSelectInput(session, "global_met_comparison_col", choices = meta_cols, selected = default_group)
      updateSelectInput(session, "diff_met_comparison_col", choices = meta_cols, selected = default_group)
      
      # Initialize array type choices dinamically for CNV
      if(!is.null(array_names())){
        updateSelectInput(session, "cnv_array_select", choices = array_names(), selected = array_names()[1])
      }
    })
    
    # --- CALL NESTED SERVER MODULES ---
    observe({
      req(length(names(input)) > 0)
      req(beta_merged(), targets_merged(), PALETTES())
      mds_server(beta_merged(), targets_merged(), PALETTES(), DIRS$mds, input, output, session)
    })
    
    observe({
      req(length(names(input)) > 0)
      req(beta_merged(), targets_merged(), PALETTES())
      pca_server(beta_merged(), targets_merged(), PALETTES(), DIRS$pca, input, output, session)
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
    
    # --- DOWNLOAD BUTTONS BETA/TARGETS ---
    output$download_beta <- downloadHandler(
      filename = function() {
        "beta_merged.csv"
      },
      content = function(file) {
        src <- file.path(DIRS$beta, "merged", "beta_merged.csv")
        file.copy(src, file)
      }
    )
    
    # --- DOWNLOAD TARGETS ---
    output$download_targets <- downloadHandler(
      filename = function() {
        "targets_merged.csv"
      },
      content = function(file) {
        src <- file.path(DIRS$beta, "merged", "targets_merged.csv")
        file.copy(src, file)
      }
    )
    
    # --- BETA MATRIX BOXPLOT UI ---
    output$beta_matrix_tabs <- renderUI({
      req(array_names())
      
      tabs <- lapply(array_names(), function(arr) {
        tabPanel(
          title = arr,
          br(),
          imageOutput(outputId = ns(paste0("pa_beta_boxplot_", arr)), height = "600px"),
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
            width = 1000,
            height = 650
          )
        }, deleteFile = FALSE)
      }
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
        
        # Build URL using the registered alias, not the filesystem path
        src <- paste0("qc_reports/", arr, "/2.0-QC_Report_", arr, ".pdf")
        
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
    
    # --- UMAP PLOT LOGIC ---
    umap_data <- reactive({
      tryCatch({
        result <- prepare_umap_data(beta_merged(),
                                    targets_merged(),
                                    input$umap_top_cpgs,
                                    input$umap_min_dist,
                                    input$umap_n_neighbors,
                                    input$umap_metric,
                                    input$umap_knn,
                                    input$umap_consensus_k_max,
                                    input$umap_id_col,
                                    input$umap_seed)
        
        # Update targets df
        targets_merged(result$targets_updated)
        
        result$umap_df
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error preparing UMAP data: ", error_msg))
        )
        NULL
      })
    })
    
    # Update color_by choices whenever umap_data recomputes
    observe({
      req(umap_data())
      
      # Exclude coordinate and sample ID columns
      exclude_cols <- c("Sample", "UMAP1", "UMAP2")
      color_cols <- setdiff(colnames(umap_data()), exclude_cols)
      current <- isolate(input$umap_color_by)
      updateSelectInput(session, "umap_color_by", choices  = color_cols,
                        selected = if (current %in% color_cols) current else color_cols[1]
      )
    })
    
    # Plot umap
    output$umap_plot <- renderPlotly({
      tryCatch({
        p <- plot_umap(
          umap_df     = umap_data(),
          color_by    = input$umap_color_by,
          legend_position = input$umap_legend_position,
          color_palette = PALETTES()$all_palettes[[input$umap_color_palette]],
          show_summary = input$umap_show_summary,
          top_cpgs    = input$umap_top_cpgs,
          min_dist    = input$umap_min_dist,
          n_neighbors = input$umap_n_neighbors,
          knn = input$umap_knn,
          consensus_k_max = input$umap_consensus_k_max,
          metric      = input$umap_metric,
          out_dir     = DIRS$umap
        )
        
        plotly::ggplotly(p, tooltip = "text")
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error rendering UMAP plot: ", error_msg))
        )
      })
    })
    
    output$umap_download_png <- downloadHandler(
      filename = function() paste0("umap_plot_", Sys.Date(), ".png"),
      content = function(file) {
        src <- file.path(DIRS$umap, paste0("umap_plot_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready"))
        file.copy(src, file)
      }
    )
    
    output$umap_download_pdf <- downloadHandler(
      filename = function() paste0("umap_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        src <- file.path(DIRS$umap, paste0("umap_plot_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready"))
        file.copy(src, file)
      }
    )
    
    # --- HEATMAP PLOT LOGIC ---
    # Reactive trigger for analysis
    heatmap_analysis_trigger <- reactiveVal(0)
    
    # Store the rendered heatmap plot for downloads
    cached_heatmap_ht <- reactiveVal(NULL)
    
    # Reactive that only runs when trigger changes or parameters change
    heatmap_cc_data <- eventReactive(input$heatmap_run_analysis, {
      req(
        beta_merged(), targets_merged(),
        input$heatmap_top, input$heatmap_id_col,
        input$heatmap_cc_kmax, input$heatmap_cc_reps,
        input$heatmap_cc_pItem, input$heatmap_cc_seed,
        input$heatmap_color_palette
      )
      
      showNotification("Running heatmap analysis...", type = "message", duration = 3)
      
      tryCatch({
        prepare_heatmap_cc(
          beta            = beta_merged(),
          targets         = targets_merged(),
          id_col          = input$heatmap_id_col,
          annotation_cols = input$heatmap_annotation_cols,
          color_palette   = PALETTES()$all_palettes[[input$heatmap_color_palette]],
          top_cpgs        = input$heatmap_top,
          cc_kmax         = input$heatmap_cc_kmax,
          cc_reps         = input$heatmap_cc_reps,
          cc_pItem        = input$heatmap_cc_pItem,
          cc_seed         = input$heatmap_cc_seed
        )
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error:", error_msg))
        )
        NULL
      })
    }, ignoreNULL = TRUE)
    
    # Separate reactive for plot parameters (doesn't trigger recomputation)
    heatmap_plot_params <- reactive({
      list(
        cc_data = heatmap_cc_data(),
        row_k = input$heatmap_row_k,
        col_k = input$heatmap_col_k,
        show_row_names = input$heatmap_show_row_names,
        show_col_names = input$heatmap_show_col_names,
        legend_pos = input$heatmap_legend_position,
        annot_legend_pos = input$heatmap_annotation_legend_position
      )
    })
    
    # Create plot only when parameters OR data changes
    cached_heatmap_ht <- reactive({
      req(heatmap_plot_params()$cc_data)
      
      tryCatch({
        ht <- plot_heatmap(
          cc_data        = heatmap_plot_params()$cc_data,
          rowK           = heatmap_plot_params()$row_k,
          colK           = heatmap_plot_params()$col_k,
          show_row_names = heatmap_plot_params()$show_row_names,
          show_col_names = heatmap_plot_params()$show_col_names,
          out_dir        = DIRS$heatmap
        )
        
        ht  # Return the heatmap object
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error rendering heatmap: ", error_msg))
        )
        NULL
      })
    }) %>% bindCache(heatmap_plot_params())  # Cache based on parameters
    
    # Plot output - just renders the cached plot
    output$heatmap_plot <- renderPlot({
      req(cached_heatmap_ht())
      
      ComplexHeatmap::draw(
        cached_heatmap_ht(),
        merge_legends          = TRUE,
        heatmap_legend_side    = isolate(input$heatmap_legend_position),
        annotation_legend_side = isolate(input$heatmap_annotation_legend_position),
        padding                = grid::unit(c(10, 10, 10, 10), "mm")
      )
    }, height = 750, width = 1000)
    
    
    
    # Download handlers using cached plot
    output$heatmap_download_png <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".png"),
      content = function(file) {
        src <- file.path(DIRS$heatmap, paste0("heatmap_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready"))
        file.copy(src, file)
      }
    )
    
    output$heatmap_download_pdf <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".pdf"),
      content = function(file) {
        src <- file.path(DIRS$heatmap, paste0("heatmap_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready"))
        file.copy(src, file)
      }
    )
    
    # Download handler for consensus class TSV
    output$heatmap_download_consensus <- downloadHandler(
      filename = function() {
        paste0("ConsensusClass_k", colK, ".tsv")
      },
      content = function(file) {
        # Construct the expected filename based on col_k parameter
        consensus_file <- file.path(
          DIRS$heatmap, 
          paste0("ConsensusClass_k", input$heatmap_col_k, ".tsv")
        )
        
        # Validate that the file exists
        validate(
          need(file.exists(consensus_file), 
               paste0("Consensus class file not found: consensus_class_k", 
                      input$heatmap_col_k, ".tsv. Please run analysis first."))
        )
        
        # Copy the file to the download location
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
    global_met_data <- eventReactive(input$global_met_run_analysis, {
      req(
        beta_merged(), targets_merged(),
        input$global_met_id_col,
        input$global_met_comparison_col,
        input$global_met_color_palette,
        input$global_met_comparison_type
      )
      
      # For custom comparison, require groups to be selected
      if (input$global_met_comparison_type == "custom") {
        req(input$global_met_group1, input$global_met_group2)
        validate(
          need(length(input$global_met_group1) > 0, "Please select at least one level for Group 1"),
          need(length(input$global_met_group2) > 0, "Please select at least one level for Group 2")
        )
      }
      
      showNotification("Running global methylation analysis...", type = "message", duration = 3)
      
      tryCatch({
        # Return a list with parameters and data for plotting
        list(
          beta = beta_merged(),
          targets = targets_merged(),
          id_col = input$global_met_id_col,
          comparison_col = input$global_met_comparison_col,
          comparison_type = input$global_met_comparison_type,
          group1 = input$global_met_group1,
          group2 = input$global_met_group2,
          annot = APP_CACHE$raw_annot,
          color_palette = PALETTES()$all_palettes[[input$global_met_color_palette]],
          out_dir = DIRS$global_met
        )
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error preparing data: ", error_msg))
        )
        NULL
      })
    }, ignoreNULL = TRUE)
    
    # Create plot when data is ready
    observeEvent(global_met_data(), {
      req(global_met_data())
      
      tryCatch({
        p <- plot_global_methylation(
          beta = global_met_data()$beta,
          targets = global_met_data()$targets,
          id_col = global_met_data()$id_col,
          comparison_col = global_met_data()$comparison_col,
          comparison_type = global_met_data()$comparison_type,
          group1 = global_met_data()$group1,
          group2 = global_met_data()$group2,
          annot = global_met_data()$annot,
          color_palette = global_met_data()$color_palette,
          out_dir = global_met_data()$out_dir
        )
        
        # Store the plot
        cached_global_met_plot(p)
        
      }, error = function(e) {
        error_msg <- e$message
        cached_global_met_plot(NULL)
        shiny::validate(
          shiny::need(FALSE, paste0("Error rendering global methylation plot: ", error_msg))
        )
      })
    })
    
    # Plot output - just renders the cached plot
    output$global_met_plot <- renderPlot({
      req(cached_global_met_plot())
    }, height = 750, width = 1000)
    
    # Download handlers using cached plot
    output$global_met_download_png <- downloadHandler(
      filename = function() paste0("global_methylation_plot_", Sys.Date(), ".png"),
      content = function(file) {
        src <- file.path(DIRS$global_met, paste0("global_methylation_plot_", Sys.Date(), ".png"))
        validate(need(file.exists(src), "PNG file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$global_met_download_pdf <- downloadHandler(
      filename = function() paste0("global_methylation_plot_", Sys.Date(), ".pdf"),
      content = function(file) {
        src <- file.path(DIRS$global_met, paste0("global_methylation_plot_", Sys.Date(), ".pdf"))
        validate(need(file.exists(src), "PDF file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    
    # --- DIFFERENTIAL METHYLATION LOGIC ---
    # Use eventReactive directly
    diff_met_data <- eventReactive(input$diff_met_run_analysis, {
      req(beta_merged(), targets_merged(), input$diff_met_id_col)
      
      showNotification("Running differential methylation analysis...", type = "message", duration = 3)
      
      tryCatch({
        result <- prepare_differential_methylation_data(
          beta_merged(), 
          targets_merged(), 
          APP_CACHE$built_annot,
          input$diff_met_id_col,
          input$diff_met_comparison_col,
          input$diff_met_baseline,
          input$diff_met_comparison
        )
        result
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, HTML(paste0("Error preparing differential methylation data: ", error_msg)))
        )
        NULL
      })
    }, ignoreNULL = TRUE)
    
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
             
             "fgsea_gobp" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export FGSEA Results"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_gobp_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_gobp_xlsx"), " Excel",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "fgsea_kegg" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export FGSEA Results"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_kegg_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_kegg_xlsx"), " Excel",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1")
               )
             ),
             
             "fgsea_hallmark" = div(
               p(class = "text-uppercase fw-bold mb-2", style = "font-size: 0.7rem; letter-spacing: 0.08em; color: #fd7e14;",
                 icon("download", style = "font-size: 0.75rem;"), " Export FGSEA Results"),
               div(
                 class = "d-flex gap-2",
                 downloadButton(ns("diff_met_download_hallmark_csv"), " CSV",
                                class = "btn btn-sm btn-outline-secondary flex-grow-1"),
                 downloadButton(ns("diff_met_download_hallmark_xlsx"), " Excel",
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
    
    # DMP Exports - Fetch pre-saved files
    output$diff_met_download_dmps_csv <- downloadHandler(
      filename = function() {
        paste0("dmps_", ifelse(input$diff_met_run_champ, "champ", "limma"), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmps_", ifelse(input$diff_met_run_champ, "champ", "limma"), "_", Sys.Date(), ".csv"))
        validate(need(file.exists(src), "CSV file not ready. Please run analysis first."))
        file.copy(src, file)
      }
    )
    
    output$diff_met_download_dmps_xlsx <- downloadHandler(
      filename = function() {
        paste0("dmps_", ifelse(input$diff_met_run_champ, "champ", "limma"), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        src <- file.path(DIRS$differential, paste0("dmps_", ifelse(input$diff_met_run_champ, "champ", "limma"), "_", Sys.Date(), ".xlsx"))
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
    
    # Density plot render
    output$diff_met_density_plot <- renderPlot({
      req(diff_met_data())
      tryCatch({
        plot_diff_methylation_density(
          diff_met_data = diff_met_data(),
          color_palette = PALETTES()$all_palettes[[input$diff_met_color_palette]],
          DIRS$differential
        )
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error rendering density plot: ", error_msg))
        )
      })
    }, height = 650, width = 1000)
    
    # DMP table
    output$diff_met_dmp_table <- DT::renderDataTable({
      req(diff_met_data(), input$diff_dmps_top_cpgs, input$diff_met_fdr_cut, input$diff_met_lfc_cut)
      tryCatch({
        dmps <- get_dmps(diff_met_data(), 
                         input$diff_met_fdr_cut,
                         input$diff_met_lfc_cut,
                         input$diff_met_run_champ,
                         DIRS$differential)
        if(nrow(dmps) > 0){
          make_dt(head(dmps, input$diff_dmps_top_cpgs))
        } else {
          make_dt(dmps)
        }
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading DMP table: ", error_msg))
        )
      })
    })
    
    # DMR table
    output$diff_met_dmr_table <- DT::renderDataTable({
      req(diff_met_data())
      tryCatch({
        dmrs <- get_dmrs(diff_met_data(), input$diff_met_run_champ, DIRS$differential)
        make_dt(dmrs)
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading DMR table: ", error_msg))
        )
      })
    })
    
    # DMG table
    output$diff_met_dmg_table <- DT::renderDataTable({
      req(diff_met_data())
      tryCatch({
        dmgs <- get_dmgs(diff_met_data(), input$diff_met_lfc_cut, DIRS$differential)
        make_dt(dmgs)
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading DMG table: ", error_msg))
        )
      })
    })
    
    
    # FGSEA tables
    output$diff_met_fgsea_gobp_table <- DT::renderDataTable({
      req(diff_met_data())
      tryCatch({
        fgsea_gobp <- get_fgsea(diff_met_data(), APP_CACHE$pathways, "gobp", DIRS$differential)
        make_dt(fgsea_gobp)
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading FGSEA GOBP results: ", error_msg))
        )
      })
    })
    
    output$diff_met_fgsea_kegg_table <- DT::renderDataTable({
      req(diff_met_data())
      tryCatch({
        fgsea_kegg <- get_fgsea(diff_met_data(), APP_CACHE$pathways, "kegg", DIRS$differential)
        make_dt(fgsea_kegg)
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading FGSEA KEGG results: ", error_msg))
        )
      })
    })
    
    output$diff_met_fgsea_hallmark_table <- DT::renderDataTable({
      req(diff_met_data())
      tryCatch({
        fgsea_hallmark <- get_fgsea(diff_met_data(), APP_CACHE$pathways, "hallmark", DIRS$differential)
        make_dt(fgsea_hallmark)
      }, error = function(e) {
        error_msg <- e$message
        shiny::validate(
          shiny::need(FALSE, paste0("Error loading FGSEA HALLMARK results: ", error_msg))
        )
      })
    })
    
    
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
    
    # Reactive to store final BED file path
    cnv_bed_path <- reactiveVal(NULL)
    
    # Reactive to trigger analysis
    run_analysis_trigger <- reactiveVal(0)
    
    # Store paths to saved plots
    cached_pileup_png <- reactiveVal(NULL)
    cached_sample_png <- reactiveVal(NULL)
    
    # Observer for BED file upload
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
    
    # Prepare data (only runs when trigger changes)
    cnv_data <- eventReactive(run_analysis_trigger(), {
      req(input$cnv_array_select,
          input$cnv_comparison_col, cnv_bed_path(),
          input$cnv_baseline, input$cnv_comparison)
      
      if(is.null(array_names())){
        shiny::need(FALSE, "CNVs can not be calculated from BetaMatrix only, 
                    require IDATs")
      } else {
        tryCatch({
          showNotification("Running CNV analysis...", type = "message", duration = 3)
          result <- prepare_cnv_data(mSetSq_list(),
                                     input$cnv_array_select, 
                                     cnv_bed_path(),
                                     input$cnv_include_xy,
                                     input$cnv_comparison_col,
                                     input$cnv_baseline,
                                     input$cnv_comparison)
          result
        }, error = function(e) {
          error_msg <- e$message
          shiny::validate(
            shiny::need(FALSE, paste0("Error preparing CNV data: ", error_msg))
          )
          NULL
        })
      }
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
        width = 1000,
        height = 650
      )
    }, deleteFile = FALSE)
    
    # Update CNV comparison column based on selected array
    observeEvent(input$cnv_array_select, {
      req(input$cnv_array_select, mSetSq_list)
      
      selected_array <- input$cnv_array_select
      mset_path <- mSetSq_list()[[selected_array]]
      mset_obj <- readRDS(mset_path)
      
      if (!is.null(mset_obj)) {
        meta_cols <- colnames(pData(mset_obj))
        
        updateSelectInput(session, "cnv_comparison_col", choices = meta_cols,
                          selected = meta_cols[1])
      }
    })
    
    # Populate group checkboxes when comparison column changes for CNV
    observeEvent(input$cnv_comparison_col, {
      req(input$cnv_comparison_col, input$cnv_array_select, mSetSq_list)
      
      selected_array <- input$cnv_array_select
      mset_path <- mSetSq_list()[[selected_array]]
      mset_obj <- readRDS(mset_path)
      
      if (!is.null(mset_obj)) {
        pd <- pData(mset_obj)
        raw_vals <- na.omit(as.character(pd[[input$cnv_comparison_col]]))
        
        validate(
          need(length(raw_vals) > 0,
               paste0("Column '", input$cnv_comparison_col, "' has no non-missing values. Please choose a different column."))
        )
        
        counts <- table(raw_vals)
        levels <- sort(names(counts))
        labels <- paste0(levels, " (", as.numeric(counts[levels]), " ",
                         ifelse(as.numeric(counts[levels]) == 1, "sample", "samples"), ")")
        
        updateCheckboxGroupInput(session, "cnv_baseline", choices = setNames(levels, labels), selected = NULL)
        updateCheckboxGroupInput(session, "cnv_comparison", choices = setNames(levels, labels), selected = NULL)
      }
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
        width = 1000,
        height = 650
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
    
    
    # --- HELPER FUNCTIONS ---
    update_active_button <- function(active_id) {
      shinyjs::removeClass(selector = ".content-section", class = "active")
      all_buttons <- c("nav_beta_matrix", "nav_qc", "nav_mds", "nav_pca", 
                       "nav_umap", "nav_heatmap", "nav_global", "nav_differential", "nav_cnv")
      
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
                     "view_differential", "view_cnv")
      
      for (view in all_views) { shinyjs::hide(view) }
      shinyjs::show(view_id)
      output$view_title <- renderText(title_text)
    }
    
  })
}
