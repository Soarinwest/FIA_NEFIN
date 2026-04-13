# ============================================================================
# Module: Summary Statistics Table
# ============================================================================

# UI --------------------------------------------------------------------------
summary_stats_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(
      "Summary Statistics",
      class = "bg-dark"
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
      req(nrow(summary_stats()) > 0)

      stats <- summary_stats()
      tests <- test_results()
      data <- filtered_data()

      # Ensure we have valid data
      if (nrow(stats) == 0) {
        return(datatable(
          data.frame(Message = "No data available for selected filters"),
          rownames = FALSE
        ))
      }

      # Helper: does at least one dataset have non-NA values?
      has_data <- function(col) sum(!is.na(data[[col]])) > 0

      # Safe test result formatter
      fmt_test <- function(test, prefix = "KS") {
        if (is.null(test)) return("N/A")
        paste0(prefix, ": D=", round(test$statistic, 2), ", ", format_pvalue(test$p.value))
      }
      fmt_mw <- function(test) {
        if (is.null(test)) return("N/A")
        paste0("MW: ", format_pvalue(test$p.value))
      }

      # Start with biomass rows (always available)
      metric_rows <- c(
        "Sample Size", "Plots", "",
        "Biomass (Mg/ha)", "Mean \u00b1 SD", "Median",
        "95th Percentile", "99th Percentile"
      )

      # Build dataset column values for biomass rows
      ds_values <- list()
      for (ds in unique(stats$dataset)) {
        ds_stats <- stats %>% filter(dataset == ds)
        ds_values[[ds]] <- c(
          "",
          scales::comma(ds_stats$n_plots),
          "",
          "",
          format_mean_sd(ds_stats$biomass_mean, ds_stats$biomass_sd, 1),
          format_stat(ds_stats$biomass_median, 1),
          format_stat(ds_stats$biomass_p95, 1),
          format_stat(ds_stats$biomass_p99, 1)
        )
      }

      # Build test results for biomass rows
      test_vals <- c("", "", "", "",
        fmt_test(tests$ks_biomass),
        fmt_mw(tests$mw_biomass),
        "", ""
      )

      # Covariate definitions: column name, display label, stat mean/sd keys, test key
      covariates <- list(
        list(col = "ndvi_s2",       label = "NDVI (Sentinel-2)", mean_key = "ndvi_s2_mean",    sd_key = "ndvi_s2_sd",    digits = 3, test_key = "ks_ndvi_s2"),
        list(col = "ndvi_modis",    label = "NDVI (MODIS)",      mean_key = "ndvi_modis_mean", sd_key = "ndvi_modis_sd", digits = 3, test_key = NULL),
        list(col = "temp_mean",     label = "Temperature (\u00b0C)",  mean_key = "temp_mean",       sd_key = "temp_sd",       digits = 1, test_key = "ks_temp"),
        list(col = "precip_annual", label = "Precipitation (cm)", mean_key = "precip_mean",     sd_key = "precip_sd",     digits = 1, test_key = "ks_precip")
      )

      # Filter to covariates that have data
      avail_covs <- Filter(function(cov) has_data(cov$col), covariates)

      if (length(avail_covs) > 0) {
        metric_rows <- c(metric_rows, "", "Covariates")
        for (ds in names(ds_values)) {
          ds_values[[ds]] <- c(ds_values[[ds]], "", "")
        }
        test_vals <- c(test_vals, "", "")

        for (cov in avail_covs) {
          metric_rows <- c(metric_rows, cov$label)
          for (ds in names(ds_values)) {
            ds_stats <- stats %>% filter(dataset == ds)
            ds_values[[ds]] <- c(ds_values[[ds]],
              format_mean_sd(ds_stats[[cov$mean_key]], ds_stats[[cov$sd_key]], cov$digits)
            )
          }
          if (!is.null(cov$test_key) && !is.null(tests)) {
            test_vals <- c(test_vals, fmt_test(tests[[cov$test_key]]))
          } else {
            test_vals <- c(test_vals, "")
          }
        }
      }

      # Assemble table
      table_data <- tibble(Metric = metric_rows)
      for (ds in names(ds_values)) {
        table_data[[ds]] <- ds_values[[ds]]
      }
      if (!is.null(tests)) {
        table_data$`Test Result` <- test_vals
      }

      # Bold/highlight row labels
      bold_labels <- c("Sample Size", "Biomass (Mg/ha)", "Covariates")
      present_bold <- intersect(bold_labels, metric_rows)

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
          fontWeight = styleEqual(present_bold, rep("bold", length(present_bold))),
          backgroundColor = styleEqual(present_bold, rep("#f0f0f0", length(present_bold)))
        )
    })
    
  })
}
