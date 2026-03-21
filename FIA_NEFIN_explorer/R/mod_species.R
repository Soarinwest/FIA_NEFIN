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
      class = "bg-primary"
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
          choices = c(
            "P99 DBH Difference" = "p99",
            "P95 DBH Difference" = "p95",
            "Max DBH Difference" = "max"
          ),
          selected = "p99"
        ),
        
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
    
    # Filter species data based on selections
    filtered_species <- reactive({
      data <- species_summary
      
      if (input$show_significant_only) {
        data <- data %>% filter(p99_pvalue < 0.05)
      }
      
      data
    })
    
    # Get data for selected species from tree data
    selected_species_trees <- reactive({
      req(input$species_select != "ALL")
      
      tree_data %>%
        filter(species_code == input$species_select)
    })
    
    # Species summary text
    output$species_summary <- renderUI({
      data <- filtered_species()
      
      n_total <- nrow(species_summary)
      n_shown <- nrow(data)
      n_significant <- sum(data$p99_pvalue < 0.05, na.rm = TRUE)
      mean_advantage <- mean(data$p99_diff, na.rm = TRUE)
      
      HTML(paste0(
        "<div style='font-size: 0.9em; color: #555;'>",
        "<strong>Species Analysis Summary</strong><br/>",
        "• ", n_shown, " of ", n_total, " species shown<br/>",
        "• ", n_significant, " with significant NEFIN advantage (", 
        round(100 * n_significant / n_shown, 1), "%)<br/>",
        "• Mean P99 advantage: +", round(mean_advantage, 1), " cm<br/>",
        "</div>"
      ))
    })
    
    # Forest plot (all species)
    output$forest_plot <- renderPlotly({
      req(filtered_species())
      
      data <- filtered_species() %>%
        arrange(desc(p99_diff))
      
      # Limit to top 10 for initial display unless "show all" is checked
      if (!input$show_significant_only && nrow(data) > 10) {
        data <- data %>% slice(1:10)
      }
      
      # Determine which metric to plot
      metric_col <- switch(
        input$species_metric,
        "p99" = "p99_diff",
        "p95" = "p95_diff",
        "max" = "max_diff"
      )
      
      # Calculate confidence intervals (approximation)
      data <- data %>%
        mutate(
          ci_lower = .data[[metric_col]] - 1.96 * p99_diff_se,
          ci_upper = .data[[metric_col]] + 1.96 * p99_diff_se,
          significant = p99_pvalue < 0.05
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
          x = paste0(
            switch(input$species_metric,
                   "p99" = "P99",
                   "p95" = "P95",
                   "max" = "Maximum"
            ),
            " DBH Difference: NEFIN - FIA (cm)"
          ),
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
      req(input$species_select != "ALL")
      req(selected_species_trees())
      
      data <- selected_species_trees()
      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)
      
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
        scale_fill_dataset() +
        scale_color_dataset() +
        labs(
          x = "DBH (cm)",
          y = "Count",
          title = paste0(species_name, " - DBH Distribution")
        ) +
        theme_fia_nefin()
      
      ggplotly(p) %>%
        layout(legend = list(orientation = "h", y = 1.1))
    })
    
    # Selected species ECDF
    output$species_ecdf <- renderPlotly({
      req(input$species_select != "ALL")
      req(selected_species_trees())
      
      data <- selected_species_trees()
      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)
      
      p <- ggplot(data, aes(x = dbh, color = dataset)) +
        stat_ecdf(linewidth = 1.2) +
        scale_color_dataset() +
        labs(
          x = "DBH (cm)",
          y = "Cumulative Proportion",
          title = paste0(species_name, " - DBH ECDF")
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
      req(input$species_select != "ALL")
      req(selected_species_trees())
      
      data <- selected_species_trees()
      species_name <- species_summary %>%
        filter(species_code == input$species_select) %>%
        pull(common_name)
      
      # Create diameter classes
      diameter_summary <- data %>%
        mutate(
          dbh_class = cut(
            dbh,
            breaks = c(0, 15, 25, 35, 45, 55, Inf),
            labels = c("5-15", "15-25", "25-35", "35-45", "45-55", ">55"),
            right = TRUE
          )
        ) %>%
        group_by(dataset, dbh_class) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(dataset) %>%
        mutate(pct = 100 * n / sum(n))
      
      # Plot
      ggplot(diameter_summary, aes(x = dbh_class, y = pct, fill = dataset)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_dataset() +
        labs(
          x = "DBH Class (cm)",
          y = "% of Trees",
          title = paste0(species_name, " - Diameter Distribution")
        ) +
        theme_fia_nefin() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(angle = 0)
        )
    })
    
  })
}
