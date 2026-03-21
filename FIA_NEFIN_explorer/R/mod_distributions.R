# ============================================================================
# Module: Distribution Comparison Plots (ECDFs)
# ============================================================================

# UI --------------------------------------------------------------------------
distributions_ui <- function(id) {
  ns <- NS(id)
  
  card(
    full_screen = TRUE,
    card_header(
      "Distribution Comparisons",
      class = "bg-primary"
    ),
    card_body(
      navset_card_tab(
        id = ns("dist_tabs"),
        nav_panel(
          "Biomass",
          plotlyOutput(ns("ecdf_biomass"), height = "500px")
        ),
        nav_panel(
          "NDVI (Sentinel-2)",
          plotlyOutput(ns("ecdf_ndvi_s2"), height = "500px")
        ),
        nav_panel(
          "NDVI (MODIS)",
          plotlyOutput(ns("ecdf_ndvi_modis"), height = "500px")
        ),
        nav_panel(
          "Temperature",
          plotlyOutput(ns("ecdf_temp"), height = "500px")
        ),
        nav_panel(
          "Precipitation",
          plotlyOutput(ns("ecdf_precip"), height = "500px")
        ),
        nav_panel(
          "Overlapping Distributions",
          layout_columns(
            col_widths = c(6, 6),
            plotOutput(ns("biomass_hist"), height = "450px"),
            plotOutput(ns("qq_plot"), height = "450px")
          )
        )
      )
    )
  )
}

# Server ----------------------------------------------------------------------
distributions_server <- function(id, filtered_data, dataset_filter, show_ci) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to create ECDF plot
    create_ecdf_plot <- function(data, var, var_label, var_unit = "") {
      
      # Prepare data based on dataset filter
      if (dataset_filter() == "all") {
        plot_data <- bind_rows(
          data %>% filter(dataset == "FIA") %>% mutate(dataset_label = "FIA"),
          data %>% filter(dataset == "NEFIN") %>% mutate(dataset_label = "NEFIN"),
          data %>% mutate(dataset_label = "Pooled")
        )
      } else if (dataset_filter() == "pooled") {
        plot_data <- data %>% mutate(dataset_label = "Pooled")
      } else {
        plot_data <- data %>% 
          filter(dataset == toupper(dataset_filter())) %>%
          mutate(dataset_label = toupper(dataset_filter()))
      }
      
      # Calculate quantiles for vertical lines
      quantiles <- plot_data %>%
        group_by(dataset_label) %>%
        summarise(
          p50 = quantile(.data[[var]], 0.5, na.rm = TRUE),
          p95 = quantile(.data[[var]], 0.95, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Create plot
      p <- ggplot(plot_data, aes(x = .data[[var]], color = dataset_label)) +
        stat_ecdf(linewidth = 1.2) +
        scale_color_manual(
          values = DATASET_COLORS,
          name = "Dataset"
        ) +
        labs(
          x = paste0(var_label, if (var_unit != "") paste0(" (", var_unit, ")")),
          y = "Cumulative Proportion",
          title = paste0(var_label, " Distribution Comparison")
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
      
      # Convert to plotly
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            y = 1.1
          )
        )
    }
    
    # ECDF plots
    output$ecdf_biomass <- renderPlotly({
      req(filtered_data())
      create_ecdf_plot(
        filtered_data(),
        "biomass",
        "Biomass",
        "Mg/ha"
      )
    })
    
    output$ecdf_ndvi_s2 <- renderPlotly({
      req(filtered_data())
      create_ecdf_plot(
        filtered_data(),
        "ndvi_s2",
        "NDVI (Sentinel-2)",
        ""
      )
    })
    
    output$ecdf_ndvi_modis <- renderPlotly({
      req(filtered_data())
      create_ecdf_plot(
        filtered_data(),
        "ndvi_modis",
        "NDVI (MODIS)",
        ""
      )
    })
    
    output$ecdf_temp <- renderPlotly({
      req(filtered_data())
      create_ecdf_plot(
        filtered_data(),
        "temp_mean",
        "Mean Temperature",
        "°C"
      )
    })
    
    output$ecdf_precip <- renderPlotly({
      req(filtered_data())
      create_ecdf_plot(
        filtered_data(),
        "precip_annual",
        "Annual Precipitation",
        "cm"
      )
    })
    
    # Overlapping histogram
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
        # Can't show overlapping for pooled
        plot_data <- data %>% mutate(dataset_label = "Pooled")
      } else {
        plot_data <- data %>% 
          filter(dataset == toupper(dataset_filter())) %>%
          mutate(dataset_label = toupper(dataset_filter()))
      }
      
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
        scale_fill_manual(values = DATASET_COLORS, name = "Dataset") +
        scale_color_manual(values = DATASET_COLORS, name = "Dataset") +
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
