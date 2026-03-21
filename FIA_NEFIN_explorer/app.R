# ============================================================================
# FIA-NEFIN Comprehensive Explorer
# ============================================================================
# Interactive Shiny application for exploring compositional differences
# between FIA and NEFIN forest inventory datasets
# 
# Author: Soren Walljasper
# Organization: Forest Ecosystem Monitoring Cooperative (FEMC) / UVM
# Date: February 2025
# ============================================================================

# Load global environment (packages, data, functions)
source("global.R")

# UI ==========================================================================
ui <- page_navbar(
  title = "FIA-NEFIN Explorer",
  id = "main_navbar",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#E69F00",
    base_font = font_google("Open Sans")
  ),
  
  # Global sidebar (appears on all tabs) --------------------------------------
  sidebar = sidebar(
    id = "global_sidebar",
    width = 280,
    
    # Header
    h4("Data Filters", class = "text-primary"),
    
    # Dataset selection
    radioButtons(
      "dataset_filter",
      "Dataset:",
      choices = c(
        "Compare All Three" = "all",
        "FIA Only" = "fia",
        "NEFIN Only" = "nefin",
        "Pooled (FIA + NEFIN)" = "pooled"
      ),
      selected = "all"
    ),
    
    hr(),
    
    # Geographic filters
    h5("Geographic Filters"),
    
    selectInput(
      "state_filter",
      "State:",
      choices = c("All", STATE_LIST),
      selected = "All",
      multiple = TRUE
    ),
    
    sliderInput(
      "biomass_range",
      "Biomass Range:",
      min = 0,
      max = 800,
      value = c(0, 800),
      step = 10,
      post = " Mg/ha"
    ),
    
    hr(),
    
    # Display options
    h5("Display Options"),
    
    checkboxInput(
      "show_ci",
      "Show confidence intervals",
      value = TRUE
    ),
    
    checkboxInput(
      "show_tests",
      "Show statistical tests",
      value = TRUE
    ),
    
    hr(),
    
    # Data summary
    uiOutput("data_summary_text"),
    
    hr(),
    
    # Download button
    downloadButton(
      "download_data",
      "Export Filtered Data",
      class = "btn-primary btn-sm"
    )
  ),
  
  # Tab 1: Dataset Comparison -------------------------------------------------
  nav_panel(
    title = "Dataset Comparison",
    icon = icon("chart-bar"),
    value = "tab_comparison",
    
    # Introduction text
    card(
      card_body(
        class = "bg-light",
        h4("Compositional Differences Between FIA and NEFIN"),
        p(
          "This tab explores fundamental differences between the Forest Inventory and Analysis (FIA)",
          "and Northeast Forest Inventory Network (NEFIN) datasets. Key questions:",
          tags$ul(
            tags$li("Are NEFIN plots systematically different from FIA plots in terms of biomass, species composition, and environmental conditions?"),
            tags$li("Does NEFIN provide access to larger trees that are underrepresented in FIA?"),
            tags$li("How do the datasets compare statistically?")
          )
        )
      )
    ),
    
    # Main content
    layout_columns(
      col_widths = c(12, 12, 12),
      
      # Summary statistics
      summary_stats_ui("summary"),
      
      # Distribution comparisons
      distributions_ui("distributions"),
      
      # Species analysis
      species_ui("species")
    )
  ),
  
  # Footer --------------------------------------------------------------------
  nav_spacer(),
  
  nav_item(
    tags$a(
      icon("github"),
      "GitHub",
      href = "https://github.com/your-repo/fia-nefin-explorer",
      target = "_blank"
    )
  ),
  
  nav_item(
    tags$a(
      icon("info-circle"),
      "About",
      href = "#",
      onclick = "alert('FIA-NEFIN Explorer v1.0\\nAuthor: Soren Walljasper\\nFEMC/UVM\\nFebruary 2025')"
    )
  )
)

# SERVER ======================================================================
server <- function(input, output, session) {
  
  # Reactive: Filtered plot data ----------------------------------------------
  filtered_data <- reactive({
    filter_plot_data(
      plot_data,
      input$dataset_filter,
      input$state_filter,
      input$biomass_range
    )
  })
  
  # Data summary text ----------------------------------------------------------
  output$data_summary_text <- renderUI({
    req(filtered_data())
    
    data <- filtered_data()
    n_total <- nrow(data)
    n_fia <- sum(data$dataset == "FIA")
    n_nefin <- sum(data$dataset == "NEFIN")
    
    HTML(paste0(
      "<div style='font-size: 0.85em; color: #555;'>",
      "<strong>Current Selection:</strong><br/>",
      "Total: ", scales::comma(n_total), " plots<br/>",
      "FIA: ", scales::comma(n_fia), "<br/>",
      "NEFIN: ", scales::comma(n_nefin), "<br/>",
      "</div>"
    ))
  })
  
  # Download handler -----------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fia_nefin_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Module servers -------------------------------------------------------------
  
  # Summary statistics
  summary_stats_server(
    "summary",
    filtered_data = filtered_data,
    dataset_filter = reactive(input$dataset_filter),
    show_tests = reactive(input$show_tests)
  )
  
  # Distribution plots
  distributions_server(
    "distributions",
    filtered_data = filtered_data,
    dataset_filter = reactive(input$dataset_filter),
    show_ci = reactive(input$show_ci)
  )
  
  # Species analysis
  species_server(
    "species",
    filtered_data = filtered_data,
    dataset_filter = reactive(input$dataset_filter)
  )
  
}

# Run app =====================================================================
shinyApp(ui = ui, server = server)
