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
    bootswatch = NULL,  # Use custom theme
    # Dark slate foundation
    bg = "#0f172a",           # Deep slate background
    fg = "#e2e8f0",           # Light text
    primary = "#3b82f6",      # Soft blue for accents/buttons
    secondary = "#64748b",    # Medium slate
    success = "#10b981",      # Green for success
    danger = "#ef4444",       # Red for errors
    warning = "#f59e0b",      # Amber for warnings
    info = "#06b6d4",         # Cyan for info
    light = "#1e293b",        # Slate for light backgrounds
    dark = "#0f172a",         # Navy for dark backgrounds
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  # Custom CSS for dark theme refinement
  tags$head(
    tags$style(HTML("
      :root {
        --bs-body-bg: #0f172a;
        --bs-body-color: #e2e8f0;
        --bs-border-color: #334155;
        --bs-emphasis-color: #94a3b8;
      }

      /* Navbar styling */
      .navbar {
        background-color: #1e293b !important;
        border-bottom: 2px solid #3b82f6;
      }

      .nav-link {
        color: #cbd5e1 !important;
        transition: all 0.2s ease;
      }

      .nav-link:hover,
      .nav-link.active {
        color: #3b82f6 !important;
        border-bottom: 3px solid #3b82f6;
      }

      /* Card styling */
      .card {
        background-color: #1e293b;
        border-color: #334155;
        border: 1px solid #334155;
      }

      .card-header {
        background-color: #0f172a;
        border-bottom-color: #334155;
        color: #e2e8f0;
      }

      .card-body {
        color: #e2e8f0;
      }

      /* Sidebar styling */
      .sidebar {
        background-color: #1e293b;
        border-right: 1px solid #334155;
      }

      /* Form controls */
      .form-control, .form-select {
        background-color: #0f172a;
        border-color: #334155;
        color: #e2e8f0;
      }

      .form-control:focus, .form-select:focus {
        background-color: #0f172a;
        border-color: #3b82f6;
        color: #e2e8f0;
        box-shadow: 0 0 0 0.25rem rgba(59, 130, 246, 0.25);
      }

      /* Button styling */
      .btn-primary {
        background-color: #3b82f6;
        border-color: #3b82f6;
      }

      .btn-primary:hover {
        background-color: #1e40af;
        border-color: #1e40af;
      }

      /* Text colors */
      .text-muted {
        color: #94a3b8 !important;
      }

      /* Table styling */
      .table {
        color: #e2e8f0;
        border-color: #334155;
      }

      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: rgba(51, 65, 85, 0.2);
      }

      /* Value boxes */
      .value-box {
        background-color: #1e293b;
        border-left: 4px solid #3b82f6;
      }
    "))
  ),

  # Loading screen overlay — fades out when Shiny connects
  header = tagList(
    tags$div(
      id = "loading-screen",
      style = paste0(
        "position:fixed; top:0; left:0; width:100%; height:100%;",
        "background:#0f172a; z-index:9999; display:flex;",
        "align-items:center; justify-content:center; flex-direction:column;"
      ),
      tags$div(
        class = "spinner-border text-primary", role = "status",
        style = "width:3rem; height:3rem;",
        tags$span(class = "visually-hidden", "Loading...")
      ),
      tags$h4("Loading FIA-NEFIN Explorer...", class = "mt-3", style = "color: #94a3b8;"),
      tags$p("Preparing data and map tiles", class = "small", style = "color: #64748b;")

    ),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          $('#loading-screen').fadeOut(600, function() { $(this).remove(); });
        }, 800);
      });
    "))
  ),

  # Tab 1: Overview -----------------------------------------------------------
  nav_panel(
    title = "Overview",
    icon  = icon("house"),
    value = "tab_overview",
    overview_ui("overview")
  ),

  # Tab 2: Methods ------------------------------------------------------------
  nav_panel(
    title = "Methods",
    icon  = icon("book"),
    value = "tab_methods",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Sources"),
        card_body(
          h6("FIA (Forest Inventory and Analysis)"),
          p(class = "small",
            "Downloaded from USDA Forest Service FIA DataMart. 7,345 plots from",
            " 7 northeastern states (ME, NH, VT, MA, CT, RI, NY), measurement",
            " years 2020-2024. Above-ground live biomass calculated from the",
            " component ratio method (DRYBIO_AG field, converted from lb/acre to",
            " Mg/ha using factor 0.001121). Each plot uses a fixed-radius nested",
            " design with four 7.3 m subplots."
          ),
          p(class = "small",
            "FIA plot coordinates are administratively fuzzed by the USDA Forest",
            " Service - displaced up to 1 mile (1.609 km) from the true location",
            " to protect landowner privacy. All 7,345 plots in this analysis have",
            " coord_source = 'fuzzed'. This spatial uncertainty propagates to all",
            " remote sensing covariate extractions."
          ),
          h6("NEFIN (Northeast Forest Inventory Network)"),
          p(class = "small",
            "Plot data provided by the Forest Ecosystem Monitoring Cooperative",
            " (FEMC) at the University of Vermont. 457 plots; 93.7% remeasured in",
            " 2024, with network origins in the 1960s. Plots target unmanaged,",
            " late-successional, and old-growth forests. Not probability-sampled -",
            " this introduces compositional bias toward large trees and high",
            " biomass stands. True GPS coordinates (no administrative fuzzing)."
          ),
          h6("Training and Test Sets"),
          p(class = "small",
            "Three training scenarios: FIA-only (7,345 plots), NEFIN-only (317",
            " plots), and Pooled (7,662 plots). Independent test set: 140 held-out",
            " NEFIN plots, stratified by biomass quartile to ensure balanced",
            " evaluation across the biomass distribution."
          )
        )
      ),
      card(
        card_header("Modeling & Validation"),
        card_body(
          h6("Covariates"),
          tags$dl(
            class = "small",
            tags$dt("ETH Global Canopy Height 2020"),
            tags$dd("Lang et al. (2023). 10 m resolution. Confirmed as the top predictor",
                    " in all 6 models at 100% normalized variable importance."),
            tags$dt("Sentinel-2 Spectral (10m)"),
            tags$dd("NDVI, EVI, NBR, NDWI, and raw bands (B2, B3, B4). Median composite 2020-2022."),
            tags$dt("MODIS Spectral (250m)"),
            tags$dd("NDVI, EVI, NBR, NDWI, NIR, Red, Green, Blue, SWIR1. Annual median composites."),
            tags$dt("Climate - Daymet V4"),
            tags$dd("Mean temperature (tmean), min/max temperature (tmin, tmax), and total",
                    " annual precipitation (ppt). Daily surface weather interpolated at 1 km. Not PRISM."),
            tags$dt("Topography"),
            tags$dd("Elevation, slope, aspect from 10m and 250m DEMs.")
          ),
          h6("Models"),
          p(class = "small",
            "Random Forest (ranger, mtry = floor(p/3), regression default) and",
            " XGBoost trained on each of three scenarios at two spatial scales:",
            " fine (10m covariates) and coarse (250m covariates). 6 models total."
          ),
          h6("Spatial Cross-Validation"),
          p(class = "small",
            "Spatial leave-one-block-out CV with 25 km x 25 km blocks, 10 km",
            " buffer between training and test folds, 10 folds, seed 42.",
            " Prevents spatial autocorrelation from inflating performance estimates."
          ),
          h6("Monte Carlo Coordinate Uncertainty"),
          p(class = "small",
            "For each FIA plot, 100 random locations drawn uniformly within the",
            " 1-mile (1.609 km) fuzz radius. Remote sensing covariates extracted",
            " at each jittered location. Standard deviation across replicates",
            " quantifies covariate extraction uncertainty due to coordinate fuzzing."
          ),
          h6("Reproducibility"),
          p(class = "small",
            "Random seed: 42 for all subsampling, CV, and Monte Carlo operations.",
            " Raster CRS: EPSG:5070 (NAD83 Conus Albers). Display CRS: EPSG:4326.",
            " Predictions at 10m (fine) and 250m (coarse) over Chittenden County, VT."
          )
        )
      )
    ),
    card(
      card_header("References"),
      card_body(
        tags$ul(
          class = "small",
          tags$li("Bechtold, W.A. & Patterson, P.L. (2005). The enhanced Forest Inventory",
                  " and Analysis program - national sampling design and estimation",
                  " procedures. USDA Forest Service, SRS-GTR-80."),
          tags$li("Lang, N., Jetz, W., Schindler, K., & Wegner, J.D. (2023). A",
                  " high-resolution canopy height model of the Earth. Nature Ecology",
                  " & Evolution, 7, 1778-1789."),
          tags$li("Thornton, P.E., et al. (2022). Daymet: Daily surface weather data",
                  " on a 1-km grid for North America, Version 4 R1. ORNL DAAC."),
          tags$li("Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32."),
          tags$li("Chen, T. & Guestrin, C. (2016). XGBoost: A scalable tree boosting",
                  " system. Proceedings of KDD 2016."),
          tags$li("Roberts, D.R., et al. (2017). Cross-validation strategies for data",
                  " with temporal, spatial, hierarchical, or phylogenetic structure.",
                  " Ecography, 40(8), 913-929."),
          tags$li("Drusch, M., et al. (2012). Sentinel-2: ESA's optical high-resolution",
                  " mission for GMES operational services. Remote Sensing of",
                  " Environment, 120, 25-36."),
          tags$li("FEMC (2024). Northeast Forest Inventory Network (NEFIN) plot data.",
                  " Forest Ecosystem Monitoring Cooperative, University of Vermont."),
          tags$li("Wright, M.N. & Ziegler, A. (2017). ranger: A fast implementation",
                  " of random forests for high dimensional data in C++ and R.",
                  " Journal of Statistical Software, 77(1), 1-17.")
        )
      )
    )
  ),

  # Tab 3: Dataset Comparison -------------------------------------------------
  nav_panel(
    title = "Dataset Comparison",
    icon  = icon("chart-bar"),
    value = "tab_comparison",

    layout_sidebar(
      sidebar = sidebar(
        id    = "comparison_sidebar",
        width = 280,

        h4("Data Filters", class = "text-primary"),

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
        h5("Display Options"),

        checkboxInput("show_ci",    "Show confidence intervals", value = TRUE),
        checkboxInput("show_tests", "Show statistical tests",    value = TRUE),

        hr(),
        uiOutput("data_summary_text")
      ),

      # Main content
      card(
        card_body(
          class = "bg-dark",
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


  # Footer -------------------------------------------------------------------
  nav_spacer(),

  nav_item(
    tags$a(
      icon("github"),
      "GitHub",
      href   = "https://github.com/Soarinwest/FIA_NEFIN/",
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
      "<div style='font-size: 0.85em; color: #94a3b8;'>",
      "<strong style='color: #e2e8f0;'>Current Selection:</strong><br/>",
      "Total: ", scales::comma(n_total), " plots<br/>",
      "FIA: ", scales::comma(n_fia), "<br/>",
      "NEFIN: ", scales::comma(n_nefin), "<br/>",
      "</div>"
    ))
  })

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
