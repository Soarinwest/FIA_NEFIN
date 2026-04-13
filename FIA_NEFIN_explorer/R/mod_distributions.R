# ============================================================================
# Module: Distribution Comparison Plots (ECDFs)
# ============================================================================

# UI --------------------------------------------------------------------------
distributions_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = TRUE,
    card_header(
      textOutput(ns("dist_header")),
      class = "bg-dark"
    ),
    card_body(
      selectInput(ns("dist_var"), "Variable:",
        choices = c(
          "Biomass (Mg/ha)"          = "biomass",
          "NDVI - Sentinel-2"        = "ndvi_s2",
          "NDVI - MODIS"             = "ndvi_modis",
          "Mean Temperature (deg C)" = "temp_mean",
          "Annual Precipitation"     = "precip_annual",
          "Canopy Height (m)"        = "canopy_height",
          "Elevation (m)"            = "elevation"
        ),
        selected = "biomass",
        width = "400px"
      ),
      uiOutput(ns("na_warning")),
      navset_card_tab(
        id = ns("dist_tabs"),
        nav_panel(
          title = tagList("ECDF ",
            tags$span(
              title = "Empirical cumulative distribution function. Shows the proportion of observations below each value. Useful for comparing distributional shape, spread, and tail behavior between datasets.",
              style = "cursor:help; color:#64748b;",
              bsicons::bs_icon("info-circle")
            )
          ),
          plotlyOutput(ns("ecdf_main"), height = "480px")
        ),
        nav_panel(
          title = tagList("Histogram ",
            tags$span(
              title = "Overlapping frequency distributions for FIA and NEFIN. Dashed lines mark dataset means. Reveals where the distributions diverge most.",
              style = "cursor:help; color:#64748b;",
              bsicons::bs_icon("info-circle")
            )
          ),
          plotOutput(ns("biomass_hist"), height = "480px")
        )
      )
    )
  )
}

# Server ----------------------------------------------------------------------
distributions_server <- function(id, filtered_data, dataset_filter, show_ci) {
  moduleServer(id, function(input, output, session) {

    # Variable labels and units
    var_labels <- c(
      biomass       = "Biomass Distribution",
      ndvi_s2       = "NDVI Distribution (Sentinel-2)",
      ndvi_modis    = "NDVI Distribution (MODIS)",
      temp_mean     = "Temperature Distribution",
      precip_annual = "Precipitation Distribution",
      canopy_height = "Canopy Height Distribution",
      elevation     = "Elevation Distribution"
    )

    var_units <- c(
      biomass       = "Mg/ha",
      ndvi_s2       = "(0-1)",
      ndvi_modis    = "(0-1)",
      temp_mean     = "deg C",
      precip_annual = "cm/year",
      canopy_height = "m",
      elevation     = "m"
    )

    # Dynamic card header
    output$dist_header <- renderText({
      req(input$dist_var)
      var_labels[input$dist_var]
    })

    # Helper: prepare plot data based on dataset filter
    prepare_plot_data <- function(data, var = NULL) {
      if (dataset_filter() == "all") {
        plot_data <- bind_rows(
          data %>% filter(dataset == "FIA")   %>% mutate(dataset_label = "FIA"),
          data %>% filter(dataset == "NEFIN") %>% mutate(dataset_label = "NEFIN"),
          data                                %>% mutate(dataset_label = "Pooled")
        )
      } else if (dataset_filter() == "pooled") {
        plot_data <- data %>% mutate(dataset_label = "Pooled")
      } else {
        plot_data <- data %>%
          filter(dataset == toupper(dataset_filter())) %>%
          mutate(dataset_label = toupper(dataset_filter()))
      }

      # Remove groups where all values of the variable are NA
      if (!is.null(var)) {
        plot_data <- plot_data %>%
          group_by(dataset_label) %>%
          filter(any(!is.na(.data[[var]]))) %>%
          ungroup()
      }

      plot_data
    }

    # Helper function to create ECDF plot
    create_ecdf_plot <- function(data, var, var_label, var_unit = "") {
      plot_data <- prepare_plot_data(data, var)

      if (nrow(plot_data) == 0 || all(is.na(plot_data[[var]]))) {
        return(
          plotly_empty() %>%
            layout(title = list(text = "No data available for this variable"))
        )
      }

      # Calculate quantiles for vertical lines
      quantiles <- plot_data %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(dataset_label) %>%
        summarise(
          p50 = quantile(.data[[var]], 0.5, na.rm = TRUE),
          p95 = quantile(.data[[var]], 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      colors_present <- DATASET_COLORS[unique(plot_data$dataset_label)]

      p <- ggplot(plot_data, aes(x = .data[[var]], color = dataset_label)) +
        stat_ecdf(linewidth = 1.2) +
        scale_color_manual(values = colors_present, name = "Dataset") +
        labs(
          x = paste0(var_label, if (var_unit != "") paste0(" (", var_unit, ")")),
          y = "Cumulative Proportion"
        ) +
        theme_fia_nefin()

      if (show_ci()) {
        p <- p +
          geom_vline(
            data = quantiles,
            aes(xintercept = p95, color = dataset_label),
            linetype = "dashed",
            alpha = 0.6
          )
      }

      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15, x = 0)
        )
    }

    # NA warning when NEFIN covariate data not available
    output$na_warning <- renderUI({
      req(input$dist_var)
      if (input$dist_var != "biomass" && dataset_filter() %in% c("all", "nefin")) {
        data <- filtered_data()
        nefin_vals <- data %>% filter(dataset == "NEFIN") %>% pull(!!sym(input$dist_var))
        if (all(is.na(nefin_vals))) {
          tags$div(
            class = "alert alert-info py-1 px-2 mb-2",
            "NEFIN covariate data not available for this variable."
          )
        }
      }
    })

    # Main ECDF plot
    output$ecdf_main <- renderPlotly({
      req(filtered_data(), input$dist_var)
      var <- input$dist_var
      unit <- var_units[var]
      label <- gsub(" Distribution", "", var_labels[var])
      create_ecdf_plot(filtered_data(), var, label, unit)
    })

    # Histogram for selected variable
    output$biomass_hist <- renderPlot({
      req(filtered_data(), input$dist_var)
      data <- filtered_data()
      var <- input$dist_var

      plot_data <- prepare_plot_data(data, var)
      req(nrow(plot_data) > 0)

      colors_present <- DATASET_COLORS[unique(plot_data$dataset_label)]

      # Calculate means
      means <- plot_data %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(dataset_label) %>%
        summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

      var_label <- gsub(" Distribution", "", var_labels[var])
      var_unit  <- var_units[var]

      ggplot(plot_data, aes(x = .data[[var]], fill = dataset_label)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
        geom_vline(
          data = means,
          aes(xintercept = mean_val, color = dataset_label),
          linewidth = 1.5,
          linetype = "dashed"
        ) +
        scale_fill_manual(values = colors_present, name = "Dataset") +
        scale_color_manual(values = colors_present, name = "Dataset") +
        labs(
          x = paste0(var_label, " (", var_unit, ")"),
          y = "Count",
          title = paste0(var_label, " Distribution Overlap")
        ) +
        theme_fia_nefin()
    })

  })
}
