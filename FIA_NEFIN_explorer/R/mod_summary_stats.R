# ============================================================================
# Module: Summary Statistics Table
# ============================================================================

# UI --------------------------------------------------------------------------
summary_stats_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(
      "Summary Statistics ",
      tags$span(
        title = paste0(
          "Shapiro-Wilk: tests normality. p<0.05 = not normally distributed. ",
          "Welch's t-test: parametric mean comparison (robust to unequal variance). ",
          "Mann-Whitney U: non-parametric median comparison. ",
          "KS test: tests whether two samples come from the same distribution. ",
          "Cohen's d: effect size (small<0.2, medium<0.5, large>0.8). ",
          "IQR: interquartile range (P75-P25). ",
          "Skewness: positive = right-skewed (long tail of high values)."
        ),
        style = "cursor:help; color:#64748b;",
        bsicons::bs_icon("info-circle")
      ),
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

      # Safe test result formatters
      fmt_ks <- function(test) {
        if (is.null(test)) return("N/A")
        paste0("D=", round(test$statistic, 3), ", ", format_pvalue(test$p.value))
      }
      fmt_mw <- function(test) {
        if (is.null(test)) return("N/A")
        format_pvalue(test$p.value)
      }
      fmt_tt <- function(test) {
        if (is.null(test)) return("N/A")
        paste0("t=", round(test$statistic, 2), ", ", format_pvalue(test$p.value))
      }
      fmt_sw <- function(test) {
        if (is.null(test)) return("N/A")
        paste0("W=", round(test$statistic, 4), ", ", format_pvalue(test$p.value))
      }
      fmt_d <- function(d) {
        if (is.na(d)) return("N/A")
        size <- if (abs(d) < 0.2) "negligible"
                else if (abs(d) < 0.5) "small"
                else if (abs(d) < 0.8) "medium"
                else "large"
        paste0(round(d, 3), " (", size, ")")
      }

      # Build rows as a list of lists for clarity
      rows <- list()
      add_row <- function(metric, vals, test_val = "") {
        rows[[length(rows) + 1]] <<- list(metric = metric, vals = vals, test = test_val)
      }
      ds_names <- unique(stats$dataset)
      get_ds <- function(ds) stats %>% filter(dataset == ds)

      # Section: Sample Size
      add_row("Sample Size", setNames(rep("", length(ds_names)), ds_names))
      add_row("  Plots",
        setNames(sapply(ds_names, function(ds) scales::comma(get_ds(ds)$n_plots)), ds_names))

      # Section: Biomass
      add_row("Biomass (Mg/ha)", setNames(rep("", length(ds_names)), ds_names))
      add_row("  Mean +/- SD",
        setNames(sapply(ds_names, function(ds) {
          s <- get_ds(ds); format_mean_sd(s$biomass_mean, s$biomass_sd, 1)
        }), ds_names),
        if (!is.null(tests)) fmt_ks(tests$ks_biomass) else "")
      add_row("  Median",
        setNames(sapply(ds_names, function(ds) format_stat(get_ds(ds)$biomass_median, 1)), ds_names),
        if (!is.null(tests)) paste0("MW: ", fmt_mw(tests$mw_biomass)) else "")
      add_row("  IQR",
        setNames(sapply(ds_names, function(ds) format_stat(get_ds(ds)$biomass_iqr, 1)), ds_names))
      add_row("  Skewness",
        setNames(sapply(ds_names, function(ds) format_stat(get_ds(ds)$biomass_skew, 2)), ds_names))
      add_row("  P95",
        setNames(sapply(ds_names, function(ds) format_stat(get_ds(ds)$biomass_p95, 1)), ds_names))
      add_row("  P99",
        setNames(sapply(ds_names, function(ds) format_stat(get_ds(ds)$biomass_p99, 1)), ds_names))

      # Normality + parametric tests (only when comparing)
      if (!is.null(tests)) {
        add_row("Normality (Shapiro-Wilk)", setNames(rep("", length(ds_names)), ds_names))
        add_row("  FIA biomass",
          setNames(sapply(ds_names, function(ds) {
            if (ds == "FIA") fmt_sw(tests$sw_fia_biomass) else ""
          }), ds_names))
        add_row("  NEFIN biomass",
          setNames(sapply(ds_names, function(ds) {
            if (ds == "NEFIN") fmt_sw(tests$sw_nefin_biomass) else ""
          }), ds_names))

        add_row("Comparison Tests", setNames(rep("", length(ds_names)), ds_names))
        add_row("  Welch's t-test",
          setNames(rep("", length(ds_names)), ds_names),
          fmt_tt(tests$tt_biomass))
        add_row("  Mann-Whitney U",
          setNames(rep("", length(ds_names)), ds_names),
          paste0("MW: ", fmt_mw(tests$mw_biomass)))
        add_row("  KS test",
          setNames(rep("", length(ds_names)), ds_names),
          paste0("KS: ", fmt_ks(tests$ks_biomass)))
        add_row("  Cohen's d",
          setNames(rep("", length(ds_names)), ds_names),
          fmt_d(tests$d_biomass))
      }

      # Section: Covariates
      covariates <- list(
        list(col = "canopy_height", label = "Canopy Height (m)",   mean_key = "ch_mean",         sd_key = "ch_sd",         digits = 1, test_key = "ks_ch"),
        list(col = "elevation",     label = "Elevation (m)",       mean_key = "elev_mean",       sd_key = "elev_sd",       digits = 0, test_key = "ks_elev"),
        list(col = "ndvi_s2",       label = "NDVI (Sentinel-2)",   mean_key = "ndvi_s2_mean",    sd_key = "ndvi_s2_sd",    digits = 3, test_key = "ks_ndvi_s2"),
        list(col = "ndvi_modis",    label = "NDVI (MODIS)",        mean_key = "ndvi_modis_mean", sd_key = "ndvi_modis_sd", digits = 3, test_key = NULL),
        list(col = "temp_mean",     label = "Temperature (C)",     mean_key = "temp_mean",       sd_key = "temp_sd",       digits = 1, test_key = "ks_temp"),
        list(col = "precip_annual", label = "Precipitation (cm)",  mean_key = "precip_mean",     sd_key = "precip_sd",     digits = 1, test_key = "ks_precip")
      )
      avail_covs <- Filter(function(cov) has_data(cov$col), covariates)

      if (length(avail_covs) > 0) {
        add_row("Covariates", setNames(rep("", length(ds_names)), ds_names))
        for (cov in avail_covs) {
          test_val <- ""
          if (!is.null(cov$test_key) && !is.null(tests) && !is.null(tests[[cov$test_key]])) {
            test_val <- paste0("KS: ", fmt_ks(tests[[cov$test_key]]))
          }
          add_row(paste0("  ", cov$label),
            setNames(sapply(ds_names, function(ds) {
              s <- get_ds(ds)
              format_mean_sd(s[[cov$mean_key]], s[[cov$sd_key]], cov$digits)
            }), ds_names),
            test_val)
        }
      }

      # Assemble into data frame
      metric_rows <- sapply(rows, function(r) r$metric)
      table_data <- tibble(Metric = metric_rows)
      for (ds in ds_names) {
        table_data[[ds]] <- sapply(rows, function(r) r$vals[[ds]])
      }
      if (!is.null(tests)) {
        table_data$`Test (FIA vs NEFIN)` <- sapply(rows, function(r) r$test)
      }

      # Bold section headers
      bold_labels <- c("Sample Size", "Biomass (Mg/ha)", "Covariates",
                       "Normality (Shapiro-Wilk)", "Comparison Tests")
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
