# ============================================================================
# Module: Summary Statistics Table
# ============================================================================

# UI --------------------------------------------------------------------------
summary_stats_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(
      "Summary Statistics",
      class = "bg-primary"
    ),
    card_body(
      DTOutput(ns("summary_table"))
    )
  )
}

# Server ----------------------------------------------------------------------
summary_stats_server <- function(id, filtered_data, dataset_filter, show_tests) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate summary statistics
    summary_stats <- reactive({
      req(filtered_data())
      calculate_summary_stats(filtered_data(), dataset_filter())
    })
    
    # Perform statistical tests
    test_results <- reactive({
      req(filtered_data())
      if (dataset_filter() == "all" && show_tests()) {
        perform_comparison_tests(filtered_data())
      } else {
        NULL
      }
    })
    
    # Build summary table
    output$summary_table <- renderDT({
      req(summary_stats())
      
      stats <- summary_stats()
      tests <- test_results()
      
      # Build table structure
      table_data <- tibble(
        Metric = c(
          "Sample Size",
          "Plots",
          "",
          "Biomass (Mg/ha)",
          "Mean ± SD",
          "Median",
          "95th Percentile",
          "99th Percentile",
          "",
          "Covariates",
          "NDVI (Sentinel-2)",
          "NDVI (MODIS)",
          "Temperature (°C)",
          "Precipitation (cm)"
        )
      )
      
      # Add dataset columns
      for (ds in unique(stats$dataset)) {
        ds_stats <- stats %>% filter(dataset == ds)
        
        table_data[[ds]] <- c(
          "",
          scales::comma(ds_stats$n_plots),
          "",
          "",
          format_mean_sd(ds_stats$biomass_mean, ds_stats$biomass_sd, 1),
          format_stat(ds_stats$biomass_median, 1),
          format_stat(ds_stats$biomass_p95, 1),
          format_stat(ds_stats$biomass_p99, 1),
          "",
          "",
          format_mean_sd(ds_stats$ndvi_s2_mean, ds_stats$ndvi_s2_sd, 3),
          format_mean_sd(ds_stats$ndvi_modis_mean, ds_stats$ndvi_modis_sd, 3),
          format_mean_sd(ds_stats$temp_mean, ds_stats$temp_sd, 1),
          format_mean_sd(ds_stats$precip_mean, ds_stats$precip_sd, 1)
        )
      }
      
      # Add test results column if comparing datasets
      if (!is.null(tests)) {
        table_data$`Test Result` <- c(
          "",
          "",
          "",
          "",
          paste0("KS: D=", round(tests$ks_biomass$statistic, 2), ", ", format_pvalue(tests$ks_biomass$p.value)),
          paste0("MW: ", format_pvalue(tests$mw_biomass$p.value)),
          "",
          "",
          "",
          "",
          paste0("KS: D=", round(tests$ks_ndvi_s2$statistic, 2), ", ", format_pvalue(tests$ks_ndvi_s2$p.value)),
          "",
          paste0("KS: D=", round(tests$ks_temp$statistic, 2), ", ", format_pvalue(tests$ks_temp$p.value)),
          paste0("KS: D=", round(tests$ks_precip$statistic, 2), ", ", format_pvalue(tests$ks_precip$p.value))
        )
      }
      
      # Render DataTable
      datatable(
        table_data,
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = '_all')
          )
        ),
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          'Metric',
          target = 'row',
          fontWeight = styleEqual(
            c("Sample Size", "Biomass (Mg/ha)", "Covariates"),
            c("bold", "bold", "bold")
          ),
          backgroundColor = styleEqual(
            c("Sample Size", "Biomass (Mg/ha)", "Covariates"),
            c("#f0f0f0", "#f0f0f0", "#f0f0f0")
          )
        )
    })
    
  })
}
