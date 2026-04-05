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
      class = "bg-primary"
    ),
    card_body(
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput(ns("dist_var"), "Variable:",
          choices = c(
            "Biomass (Mg/ha)"          = "biomass",
            "NDVI -- Sentinel-2"       = "ndvi_s2",
            "NDVI -- MODIS"            = "ndvi_modis",
            "Mean Temperature (deg C)" = "temp_mean",
            "Annual Precipitation"     = "precip_annual",
            "Canopy Height (m)"        = "canopy_height",
            "Elevation (m)"            = "elevation"
          ),
          selected = "biomass"
        ),
        checkboxInput(ns("show_overlap"), "Show histogram overlay", value = FALSE),
        checkboxInput(ns("show_qq"), "Show Q-Q plot", value = FALSE)
      ),
      uiOutput(ns("na_warning")),
      plotlyOutput(ns("ecdf_main"), height = "480px"),
      conditionalPanel(
        condition = paste0("input['", ns("show_overlap"), "']"),
        layout_columns(
          col_widths = c(6, 6),
          plotOutput(ns("biomass_hist"), height = "350px"),
          plotOutput(ns("qq_plot"), height = "350px")
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

    # Helper function to create ECDF plot
    create_ecdf_plot <- function(data, var, var_label, var_unit = "") {

      # Prepare data based on dataset filter
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
      plot_data <- plot_data %>%
        group_by(dataset_label) %>%
        filter(any(!is.na(.data[[var]]))) %>%
        ungroup()

      # If no data remains, show message
      if (nrow(plot_data) == 0 || all(is.na(plot_data[[var]]))) {
        return(
          plotly_empty() %>%
            layout(title = list(text = "No data available for this variable"))
        )
      }

      # Calculate quantiles for vertical lines (only for groups with data)
      quantiles <- plot_data %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(dataset_label) %>%
        summarise(
          p50 = quantile(.data[[var]], 0.5, na.rm = TRUE),
          p95 = quantile(.data[[var]], 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      # Use only colors present in the data
      colors_present <- DATASET_COLORS[unique(plot_data$dataset_label)]

      # Create plot (no title — title is in card_header)
      p <- ggplot(plot_data, aes(x = .data[[var]], color = dataset_label)) +
        stat_ecdf(linewidth = 1.2) +
        scale_color_manual(
          values = colors_present,
          name = "Dataset"
        ) +
        labs(
          x = paste0(var_label, if (var_unit != "") paste0(" (", var_unit, ")")),
          y = "Cumulative Proportion"
        ) +
        theme_fia_nefin()

      # Add quantile lines if requested
      if (show_ci()) {
        p <- p +
          geom_vline(
            data = quantiles,
            aes(xintercept = p95, color = dataset_label),
            linetype = "dashed",
            alpha = 0.6
          )
      }

      # Convert to plotly — legend below plot to avoid title overlap
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            y = -0.15,
            x = 0
          )
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

    # Main ECDF plot (reactive on dist_var)
    output$ecdf_main <- renderPlotly({
      req(filtered_data())
      req(input$dist_var)
      var <- input$dist_var
      unit <- var_units[var]
      label <- gsub(" Distribution", "", var_labels[var])
      create_ecdf_plot(filtered_data(), var, label, unit)
    })

    # Overlapping histogram (biomass only)
    output$biomass_hist <- renderPlot({
      req(filtered_data())
      data <- filtered_data()

      # Prepare data
      if (dataset_filter() == "all") {
        plot_data <- bind_rows(
          data %>% filter(dataset == "FIA") %>% mutate(dataset_label = "FIA"),
          data %>% filter(dataset == "NEFIN") %>% mutate(dataset_label = "NEFIN")
        )
      } else if (dataset_filter() == "pooled") {
        plot_data <- data %>% mutate(dataset_label = "Pooled")
      } else {
        plot_data <- data %>%
          filter(dataset == toupper(dataset_filter())) %>%
          mutate(dataset_label = toupper(dataset_filter()))
      }

      colors_present <- DATASET_COLORS[unique(plot_data$dataset_label)]

      # Calculate means
      means <- plot_data %>%
        group_by(dataset_label) %>%
        summarise(mean_biomass = mean(biomass, na.rm = TRUE), .groups = "drop")

      # Plot
      ggplot(plot_data, aes(x = biomass, fill = dataset_label)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
        geom_vline(
          data = means,
          aes(xintercept = mean_biomass, color = dataset_label),
          linewidth = 1.5,
          linetype = "dashed"
        ) +
        scale_fill_manual(values = colors_present, name = "Dataset") +
        scale_color_manual(values = colors_present, name = "Dataset") +
        labs(
          x = "Biomass (Mg/ha)",
          y = "Count",
          title = "Biomass Distribution Overlap"
        ) +
        theme_fia_nefin()
    })

    # Q-Q plot (only when comparing FIA vs NEFIN)
    output$qq_plot <- renderPlot({
      req(filtered_data())

      if (dataset_filter() != "all") {
        # Show message
        ggplot() +
          annotate(
            "text",
            x = 0.5, y = 0.5,
            label = "Q-Q plot only available when\ncomparing FIA and NEFIN",
            size = 6,
            color = "gray50"
          ) +
          theme_void()
      } else {
        data <- filtered_data()
        fia_biomass <- data %>% filter(dataset == "FIA") %>% pull(biomass)
        nefin_biomass <- data %>% filter(dataset == "NEFIN") %>% pull(biomass)

        # Create Q-Q plot data
        qq_data <- tibble(
          fia = sort(fia_biomass),
          nefin = sort(nefin_biomass)
        ) %>%
          slice(1:min(length(fia_biomass), length(nefin_biomass)))

        # Plot
        ggplot(qq_data, aes(x = fia, y = nefin)) +
          geom_point(alpha = 0.3, size = 1) +
          geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1, linetype = "dashed") +
          labs(
            x = "FIA Biomass Quantiles (Mg/ha)",
            y = "NEFIN Biomass Quantiles (Mg/ha)",
            title = "Q-Q Plot: FIA vs NEFIN Biomass"
          ) +
          theme_fia_nefin()
      }
    })

  })
}
