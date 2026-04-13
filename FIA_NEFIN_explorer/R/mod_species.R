# ============================================================================
# Module: Species Analysis
# ============================================================================

# UI --------------------------------------------------------------------------
species_ui <- function(id) {
  ns <- NS(id)

  card(
    full_screen = TRUE,
    card_header(
      "Species-Level Analysis",
      class = "bg-dark"
    ),
    layout_sidebar(
      sidebar = sidebar(
        id = ns("species_sidebar"),
        width = 300,

        selectInput(
          ns("species_select"),
          "Species:",
          choices = SPECIES_CHOICES,
          selected = "ALL"
        ),

        radioButtons(
          ns("species_metric"),
          "Metric:",
          choices = c("P99 DBH Difference" = "p99"),
          selected = "p99"
        ),
        helpText("P95 and Max metrics not available in current dataset."),

        checkboxInput(
          ns("show_significant_only"),
          "Show significant species only",
          value = FALSE
        ),

        hr(),

        # Summary text
        uiOutput(ns("species_summary"))
      ),

      navset_card_tab(
        id = ns("species_tabs"),
        nav_panel(
          "Forest Plot (All Species)",
          plotlyOutput(ns("forest_plot"), height = "700px")
        ),
        nav_panel(
          "Selected Species Detail",
          plotlyOutput(ns("species_dbh_hist"), height = "400px"),
          plotlyOutput(ns("species_ecdf"), height = "400px")
        ),
        nav_panel(
          "Diameter Classes",
          plotOutput(ns("diameter_classes"), height = "600px")
        )
      )
    )
  )
}

# Server ----------------------------------------------------------------------
species_server <- function(id, filtered_data, dataset_filter) {
  moduleServer(id, function(input, output, session) {

    # --- DEBUG: log SPECIES_CHOICES structure on init ---
    message("[species] === SPECIES_CHOICES DEBUG ===")
    message("[species] Length: ", length(SPECIES_CHOICES))
    message("[species] First 3 names:  ", paste(head(names(SPECIES_CHOICES), 3), collapse = " | "))
    message("[species] First 3 values: ", paste(head(as.character(SPECIES_CHOICES), 3), collapse = " | "))
    message("[species] tree_data cols: ", paste(names(tree_data), collapse = ", "))
    message("[species] tree_data$species_name unique (first 5): ",
            paste(head(unique(tree_data$species_name), 5), collapse = " | "))
    if ("species_code" %in% names(tree_data)) {
      message("[species] tree_data$species_code unique (first 5): ",
              paste(head(unique(tree_data$species_code), 5), collapse = " | "))
    }
    message("[species] species_summary$species_code (first 5): ",
            paste(head(species_summary$species_code, 5), collapse = " | "))

    # --- DEBUG: log input$species_select changes ---
    observeEvent(input$species_select, {
      message("[species] input$species_select changed to: '", input$species_select, "'")
      message("[species] nchar: ", nchar(input$species_select))
    })

    # Filter species data based on selections (only species with tree data)
    filtered_species <- reactive({
      available <- unique(tree_data$species_name)
      data <- species_summary %>%
        filter(species_code %in% available)

      if (isTRUE(input$show_significant_only)) {
        data <- data %>% filter(!is.na(p99_pvalue), p99_pvalue < 0.05)
      }

      message("[species] filtered_species: ", nrow(data), " species",
              if (isTRUE(input$show_significant_only)) " (significant only)" else "")
      data
    })

    # Get data for selected species from tree data
    selected_species_trees <- reactive({
      req(input$species_select, input$species_select != "ALL")

      message("[species] Filtering tree_data where species_name == '", input$species_select, "'")
      message("[species] tree_data has ", nrow(tree_data), " rows")
      message("[species] Unique species_name values (first 10): ",
              paste(head(unique(tree_data$species_name), 10), collapse = " | "))

      result <- tree_data %>%
        filter(species_name == input$species_select)

      message("[species] Filter result: ", nrow(result), " rows")

      # Also try matching species_code as fallback diagnostic
      if (nrow(result) == 0 && "species_code" %in% names(tree_data)) {
        alt <- tree_data %>% filter(species_code == input$species_select)
        message("[species] Alt filter by species_code: ", nrow(alt), " rows")
      }

      validate(need(
        nrow(result) > 0,
        paste0("No tree records found for species: ", input$species_select)
      ))

      result
    })

    # Species summary text
    output$species_summary <- renderUI({
      data <- filtered_species()

      n_total <- nrow(species_summary)
      n_shown <- nrow(data)
      n_significant <- sum(!is.na(data$p99_pvalue) & data$p99_pvalue < 0.05)
      mean_advantage <- mean(data$p99_diff, na.rm = TRUE)

      HTML(paste0(
        "<div style='font-size: 0.9em; color: #555;'>",
        "<strong>Species Analysis Summary</strong><br/>",
        "\u2022 ", n_shown, " of ", n_total, " species shown<br/>",
        "\u2022 ", n_significant, " with significant NEFIN advantage (",
        round(100 * n_significant / max(n_shown, 1), 1), "%)<br/>",
        "\u2022 Mean P99 advantage: +", round(mean_advantage, 1), " cm<br/>",
        "</div>"
      ))
    })

    # Forest plot (all species)
    output$forest_plot <- renderPlotly({
      req(filtered_species())

      data <- filtered_species() %>%
        arrange(desc(p99_diff))

      # Show all when significant-only, top 10 otherwise
      if (!isTRUE(input$show_significant_only) && nrow(data) > 10) {
        data <- data %>% dplyr::slice_head(n = 10)
      }

      # Always use p99 (only available metric)
      metric_col <- "p99_diff"

      # Handle all-NA metric column
      if (all(is.na(data[[metric_col]]))) {
        return(
          plotly_empty() %>%
            layout(title = list(text = "No data available for selected metric"))
        )
      }

      # Calculate confidence intervals
      data <- data %>%
        mutate(
          ci_lower = .data[[metric_col]] - 1.96 * p99_diff_se,
          ci_upper = .data[[metric_col]] + 1.96 * p99_diff_se,
          significant = !is.na(p99_pvalue) & p99_pvalue < 0.05
        )

      # Create plot
      p <- ggplot(data, aes(
        x = .data[[metric_col]],
        y = reorder(common_name, .data[[metric_col]])
      )) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        geom_point(aes(color = significant), size = 3) +
        geom_errorbarh(
          aes(xmin = ci_lower, xmax = ci_upper, color = significant),
          height = 0
        ) +
        scale_color_significance() +
        labs(
          x = "P99 DBH Difference: NEFIN - FIA (cm)",
          y = NULL,
          title = "Large Tree Advantage by Species"
        ) +
        theme_fia_nefin() +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          hovermode = "y unified",
          margin = list(l = 150)
        )
    })

    # Selected species histogram
    output$species_dbh_hist <- renderPlotly({
      req(input$species_select, input$species_select != "ALL")
      data <- selected_species_trees()
      req(nrow(data) > 0)

      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)

      # Colors for datasets present in data
      colors_present <- DATASET_COLORS[unique(na.omit(data$dataset))]

      # Calculate quantiles
      quantiles <- data %>%
        group_by(dataset) %>%
        summarise(
          p95 = quantile(dbh, 0.95, na.rm = TRUE),
          p99 = quantile(dbh, 0.99, na.rm = TRUE),
          .groups = "drop"
        )

      # Plot
      p <- ggplot(data, aes(x = dbh, fill = dataset)) +
        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
        geom_vline(
          data = quantiles,
          aes(xintercept = p99, color = dataset),
          linetype = "dashed",
          linewidth = 1
        ) +
        scale_fill_manual(values = colors_present, name = "Dataset") +
        scale_color_manual(values = colors_present, name = "Dataset") +
        labs(
          x = "DBH (cm)",
          y = "Count",
          title = paste0(species_name, " - DBH Distribution"),
          subtitle = "(FIA plots only)"
        ) +
        theme_fia_nefin()

      ggplotly(p) %>%
        layout(legend = list(orientation = "h", y = 1.1))
    })

    # Selected species ECDF
    output$species_ecdf <- renderPlotly({
      req(input$species_select, input$species_select != "ALL")
      data <- selected_species_trees()
      req(nrow(data) > 0)

      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)

      # Colors for datasets present in data
      colors_present <- DATASET_COLORS[unique(na.omit(data$dataset))]

      p <- ggplot(data, aes(x = dbh, color = dataset)) +
        stat_ecdf(linewidth = 1.2) +
        scale_color_manual(values = colors_present, name = "Dataset") +
        labs(
          x = "DBH (cm)",
          y = "Cumulative Proportion",
          title = paste0(species_name, " - DBH ECDF"),
          subtitle = "(FIA plots only)"
        ) +
        theme_fia_nefin()

      ggplotly(p) %>%
        layout(
          hovermode = "x unified",
          legend = list(orientation = "h", y = 1.1)
        )
    })

    # Diameter class comparison
    output$diameter_classes <- renderPlot({
      req(input$species_select, input$species_select != "ALL")
      data <- selected_species_trees()
      req(nrow(data) > 0)

      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)

      # Colors for datasets present in data
      colors_present <- DATASET_COLORS[unique(na.omit(data$dataset))]

      # Create diameter classes
      diameter_summary <- data %>%
        mutate(
          dbh_class = cut(
            dbh,
            breaks = c(0, 15, 25, 35, 45, 55, Inf),
            labels = c("5-15", "15-25", "25-35", "35-45", "45-55", ">55"),
            right = TRUE
          ) %>% droplevels()
        ) %>%
        group_by(dataset, dbh_class) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(dataset) %>%
        mutate(pct = 100 * n / sum(n))

      # Plot
      ggplot(diameter_summary, aes(x = dbh_class, y = pct, fill = dataset)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = colors_present, name = "Dataset") +
        labs(
          x = "DBH Class (cm)",
          y = "% of Trees",
          title = paste0(species_name, " - Diameter Distribution"),
          subtitle = "(FIA plots only)"
        ) +
        theme_fia_nefin() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(angle = 0)
        )
    })

  })
}
