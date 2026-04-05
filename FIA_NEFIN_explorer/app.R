# ============================================================================
# FIA-NEFIN Comprehensive Explorer
# ============================================================================
# Interactive Shiny application for exploring compositional differences
# between FIA and NEFIN forest inventory datasets
#
# Author: Soren Donisvitch
# Date: February 2025 (updated 2026-04)
# ============================================================================

# Load global environment (packages, data, functions)
source("global.R")

# UI ==========================================================================
ui <- page_navbar(
  title = "FIA-NEFIN Explorer",
  id    = "main_navbar",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#E69F00",
    base_font  = font_google("Open Sans")
  ),

  # Loading screen overlay — fades out when Shiny connects
  header = tagList(
    tags$div(
      id = "loading-screen",
      style = paste0(
        "position:fixed; top:0; left:0; width:100%; height:100%;",
        "background:white; z-index:9999; display:flex;",
        "align-items:center; justify-content:center; flex-direction:column;"
      ),
      tags$div(
        class = "spinner-border text-primary", role = "status",
        style = "width:3rem; height:3rem;",
        tags$span(class = "visually-hidden", "Loading...")
      ),
      tags$h4("Loading FIA-NEFIN Explorer...", class = "mt-3 text-muted"),
      tags$p("Preparing data and map tiles", class = "text-muted small")
    ),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          $('#loading-screen').fadeOut(600, function() { $(this).remove(); });
        }, 800);
      });
    "))
  ),

  # Global sidebar (appears on Dataset Comparison tab) -----------------------
  sidebar = sidebar(
    id    = "global_sidebar",
    width = 280,

    # Header
    h4("Data Filters", class = "text-primary"),

    # Dataset selection
    radioButtons(
      "dataset_filter",
      "Dataset:",
      choices = c(
        "Compare All Three"     = "all",
        "FIA Only"              = "fia",
        "NEFIN Only"            = "nefin",
        "Pooled (FIA + NEFIN)"  = "pooled"
      ),
      selected = "all"
    ),

    hr(),

    # Geographic filters
    h5("Geographic Filters"),

    selectInput(
      "state_filter",
      "State:",
      choices  = c("All", STATE_LIST),
      selected = "All",
      multiple = TRUE
    ),

    sliderInput(
      "biomass_range",
      "Biomass Range:",
      min   = 0,
      max   = 800,
      value = c(0, 800),
      step  = 10,
      post  = " Mg/ha"
    ),

    hr(),

    # Display options
    h5("Display Options"),

    checkboxInput("show_ci",    "Show confidence intervals", value = TRUE),
    checkboxInput("show_tests", "Show statistical tests",    value = TRUE),

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

  # Tab 1: Overview -----------------------------------------------------------
  nav_panel(
    title = "Overview",
    icon  = icon("house"),
    value = "tab_overview",
    overview_ui("overview")
  ),

  # Tab 2: Dataset Comparison -------------------------------------------------
  nav_panel(
    title = "Dataset Comparison",
    icon  = icon("chart-bar"),
    value = "tab_comparison",

    card(
      card_body(
        class = "bg-light",
        h4("Compositional Differences Between FIA and NEFIN"),
        p(
          "This tab explores fundamental differences between the Forest Inventory",
          "and Analysis (FIA) and Northeast Forest Inventory Network (NEFIN) datasets.",
          "Key questions:",
          tags$ul(
            tags$li("Are NEFIN plots systematically different from FIA in biomass, species composition, and environmental conditions?"),
            tags$li("Does NEFIN provide access to larger trees that are underrepresented in FIA?"),
            tags$li("How do the datasets compare statistically?")
          )
        )
      )
    ),

    layout_columns(
      col_widths = c(12, 12, 12),
      navset_card_tab(
        nav_panel(
          "Summary Statistics",
          summary_stats_ui("summary")
        ),
        nav_panel(
          "Distributions",
          distributions_ui("distributions")
        ),
        nav_panel(
          "Species Analysis",
          species_ui("species")
        )
      )
    )
  ),

  # Tab 3: Spatial Explorer ---------------------------------------------------
  nav_panel(
    title = "Spatial Explorer",
    icon  = icon("map"),
    value = "tab_spatial",
    spatial_ui("spatial")
  ),

  # Tab 4: Scale Analysis -----------------------------------------------------
  nav_panel(
    title = "Scale Analysis",
    icon  = icon("chart-area"),
    value = "tab_scale",
    scale_ui("scale")
  ),

  # Tab 5: Modeling Results ---------------------------------------------------
  nav_panel(
    title = "Modeling Results",
    icon  = icon("robot"),
    value = "tab_modeling",
    modeling_ui("modeling")
  ),

  # Tab 6: Methods ------------------------------------------------------------
  nav_panel(
    title = "Methods",
    icon  = icon("book"),
    value = "tab_methods",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Sources & Processing"),
        card_body(
          h5("FIA Data"),
          p(
            "Forest Inventory and Analysis (FIA) data downloaded from the USDA Forest",
            "Service FIA DataMart. Plots from 7 northeastern states (ME, NH, VT, MA,",
            "CT, RI, NY), measurement years 2020\u20132024. Above-ground live biomass",
            "calculated from component ratio method (DRYBIO_AG field, converted from",
            "lb/acre to Mg/ha using factor 0.001121)."
          ),
          h5("FIA Coordinate Handling"),
          p(
            "FIA plot coordinates are ", strong("pre-fuzzed"), " by the USDA Forest",
            "Service \u2014 displaced up to 1 mile (1.6 km) from the true location to",
            "protect landowner privacy. All 7,345 plots in this dataset have",
            tags$code("coord_source = 'fuzzed'"), ". Coordinates shown on the Spatial",
            "Explorer map are these fuzzed positions. Uncertainty circles depict the",
            "covariate extraction radius used in the Monte Carlo analysis."
          ),
          h5("NEFIN Data"),
          p(
            "Northeast Forest Inventory Network (NEFIN) plot data provided by FEMC.",
            "457 plots; 93.7% measured in 2024, with historical remeasurements back",
            "to the 1960s. True GPS coordinates (no administrative fuzzing)."
          ),
          h5("Monte Carlo Uncertainty"),
          p(
            "For each FIA plot, 100 random locations were drawn uniformly within the",
            "1-mile fuzz radius. Remote sensing covariates were extracted at each",
            "jittered location. The standard deviation of extracted values across 100",
            "replicates is reported as covariate uncertainty."
          )
        )
      ),
      card(
        card_header("Remote Sensing & Modeling"),
        card_body(
          h5("Covariates"),
          tags$dl(
            tags$dt("ETH Global Canopy Height 2020"),
            tags$dd("Lang et al. (2023), 10 m resolution. Top predictor in all models (100% normalized importance)."),
            tags$dt("Sentinel-2 (10m)"),
            tags$dd("NDVI, EVI, NBR, NDWI, and spectral bands. Median composite 2020\u20132022."),
            tags$dt("MODIS (250m)"),
            tags$dd("NDVI, EVI, NBR, NDWI, and spectral bands. Annual median composites."),
            tags$dt("Climate \u2014 Daymet V4"),
            tags$dd("Daily surface weather interpolated at 1 km. Mean annual temperature (tmean) and total annual precipitation (ppt). NOT PRISM."),
            tags$dt("Topography"),
            tags$dd("Elevation, slope, aspect from 10m and 250m DEMs.")
          ),
          h5("Modeling Framework"),
          p(
            "Random Forest and XGBoost models trained on FIA-only, NEFIN-only, and",
            "Pooled (FIA+NEFIN) training sets at two spatial scales: fine (10m covariates)",
            "and coarse (250m covariates). Spatial leave-one-block-out cross-validation",
            "(5 folds) used to estimate generalization error."
          ),
          h5("Hexagon Aggregation"),
          p(
            "Plots aggregated into DGGRID H3 hexagons at 9 spatial scales from 100 ha",
            "to 100,000 ha. All area calculations use EPSG:5070 (Albers Equal Area Conic,",
            "NAD83). Leaflet maps display in WGS84 (EPSG:4326)."
          ),
          h5("Reproducibility"),
          p("Random seed: 42 for all subsampling and Monte Carlo operations.")
        )
      )
    )
  ),

  # Footer -------------------------------------------------------------------
  nav_spacer(),

  nav_item(
    tags$a(
      icon("github"),
      "GitHub",
      href   = "https://github.com/your-repo/fia-nefin-explorer",
      target = "_blank"
    )
  ),

  nav_item(
    tags$a(
      icon("info-circle"),
      "About",
      href    = "#",
      onclick = "alert('FIA-NEFIN Explorer v2.0\\nAuthor: Soren Donisvitch\\nFEMC/UVM\\n2026')"
    )
  )
)


# SERVER ======================================================================
server <- function(input, output, session) {

  # Pulse spinner on outputs while recalculating
  shiny::useBusyIndicators()

  # Reactive: filtered plot data ---------------------------------------------
  filtered_data <- reactive({
    filter_plot_data(
      plot_data,
      input$dataset_filter,
      input$state_filter,
      input$biomass_range
    )
  })

  # Data summary text --------------------------------------------------------
  output$data_summary_text <- renderUI({
    req(filtered_data())
    data    <- filtered_data()
    n_total <- nrow(data)
    n_fia   <- sum(data$dataset == "FIA",   na.rm = TRUE)
    n_nefin <- sum(data$dataset == "NEFIN", na.rm = TRUE)

    HTML(paste0(
      "<div style='font-size: 0.85em; color: #555;'>",
      "<strong>Current Selection:</strong><br/>",
      "Total: ", scales::comma(n_total), " plots<br/>",
      "FIA: ", scales::comma(n_fia), "<br/>",
      "NEFIN: ", scales::comma(n_nefin), "<br/>",
      "</div>"
    ))
  })

  # Download handler ---------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fia_nefin_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # Module servers -----------------------------------------------------------

  # Tab 1: Overview
  overview_server("overview")

  # Tab 2: Dataset Comparison (existing modules)
  summary_stats_server(
    "summary",
    filtered_data  = filtered_data,
    dataset_filter = reactive(input$dataset_filter),
    show_tests     = reactive(input$show_tests)
  )

  distributions_server(
    "distributions",
    filtered_data  = filtered_data,
    dataset_filter = reactive(input$dataset_filter),
    show_ci        = reactive(input$show_ci)
  )

  species_server(
    "species",
    filtered_data  = filtered_data,
    dataset_filter = reactive(input$dataset_filter)
  )

  # Tab 3: Spatial Explorer
  spatial_server(
    "spatial",
    fia_plots        = fia_plots,
    nefin_plots      = nefin_plots,
    plot_uncertainty = plot_uncertainty,
    hex_1kha         = hex_1kha,
    states_sf        = states_sf,
    cv_results       = cv_results
  )

  # Tab 4: Scale Analysis
  scale_server(
    "scale",
    scale_metrics      = scale_metrics,
    bootstrap_variance = bootstrap_variance
  )

  # Tab 5: Modeling Results
  modeling_server(
    "modeling",
    cv_results       = cv_results,
    fold_results     = fold_results,
    test_predictions = test_predictions,
    var_importance   = var_importance,
    fuzzing_sig      = fuzzing_sig,
    fuzzing_rmse     = fuzzing_rmse
  )

}


# Run app =====================================================================
shinyApp(ui = ui, server = server)
