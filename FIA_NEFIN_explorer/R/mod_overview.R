# ============================================================================
# mod_overview.R - Tab 1: Overview
# ============================================================================

overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Row 1: Value boxes
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title    = "FIA Plots",
        value    = "7,345",
        showcase = bsicons::bs_icon("tree"),
        theme    = value_box_theme(bg = "#1e3a5f", fg = "#e2e8f0")
      ),
      value_box(
        title    = "NEFIN Plots",
        value    = "457",
        showcase = bsicons::bs_icon("geo-alt"),
        theme    = value_box_theme(bg = "#4a3728", fg = "#e2e8f0")
      ),
      value_box(
        title    = "States",
        value    = "7",
        showcase = bsicons::bs_icon("map"),
        theme    = value_box_theme(bg = "#1a3a36", fg = "#e2e8f0")
      ),
      value_box(
        title    = "Study Period",
        value    = "2020-2024",
        showcase = bsicons::bs_icon("calendar3"),
        theme    = value_box_theme(bg = "#2d3748", fg = "#e2e8f0")
      )
    ),

    # Row 2: About + Datasets + Map (3 balanced columns)
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("About"),
        card_body(
          tags$p(class = "small",
            "This project investigates how training data composition influences",
            " model performance in spatial biomass estimation. We compare two",
            " fundamentally different forest inventory datasets - not as",
            " interchangeable data sources, but as distinct sampling distributions",
            " over the same landscape."
          ),
          tags$p(class = "small", style = "color:#94a3b8;",
            "How does the distribution of training data affect model bias,",
            " error structure, and ability to generalize across feature space",
            " - particularly for rare but ecologically important conditions?"
          ),
          tags$blockquote(
            class = "small mb-0",
            style = "border-left:3px solid #3b82f6; padding-left:12px; color:#94a3b8; font-style:italic;",
            "Model performance is governed more by structural representativeness",
            " of training data than by coordinate precision or spatial resolution."
          )
        )
      ),
      card(
        card_header("Datasets"),
        card_body(
          tags$dl(
            class = "mb-0",
            tags$dt(style = "color:#3b82f6;", "FIA (Forest Inventory and Analysis)"),
            tags$dd(class = "small mb-2",
              "A systematic, probability-based survey covering all US forest land.",
              " 7,345 northeastern plots (ME, NH, VT, MA, CT, RI, NY),",
              " measurement years 2020-2024. Fixed-radius nested design with",
              " four 7.3 m subplots. Coordinates are administratively fuzzed",
              " up to 1 mile (1.6 km) to protect landowner privacy."
            ),
            tags$dt(style = "color:#f59e0b;", "NEFIN (NE Forest Inventory Network)"),
            tags$dd(class = "small mb-0",
              "A collaborative research network targeting unmanaged,",
              " late-successional, and old-growth forests. 457 plots, 93.7%",
              " measured in 2024, with origins in the 1960s. Not probability-sampled",
              " - plots are in forests of ecological interest, introducing",
              " compositional bias toward large trees. True GPS coordinates."
            )
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Study Area"),
        card_body(
          style = "padding:8px; text-align:center; overflow:hidden;",
          tags$img(
            src   = "figures/Fig1_Study_Area.png",
            style = "width:100%; height:auto; border-radius:4px;"
          )
        )
      )
    ),

    # Row 3: Key findings + analytical approach
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(
          tags$span(bsicons::bs_icon("graph-up"), " Key Findings")
        ),
        card_body(
          tags$ul(
            class = "small mb-0",
            tags$li("Sampling design defines model bias: FIA underpredicts high-biomass",
                    " stands (Q4), NEFIN overpredicts low-biomass stands (Q1)"),
            tags$li("Pooling FIA + NEFIN reduces test-set RMSE by ~12-15% vs FIA alone,",
                    " but introduces a compromise bias structure"),
            tags$li("Coordinate fuzzing (1-mile displacement) is not the dominant error",
                    " source - training data distribution is"),
            tags$li("Bias structure persists across spatial aggregation scales",
                    " (100 ha to 100,000 ha hexagons)"),
            tags$li("ETH Global Canopy Height 2020 is the top predictor at 100%",
                    " normalized importance in all 6 models")
          )
        )
      ),
      card(
        card_header(
          tags$span(bsicons::bs_icon("gear"), " Analytical Approach")
        ),
        card_body(
          tags$ul(
            class = "small mb-0",
            tags$li("Three controlled training scenarios: FIA-only (7,345 plots),",
                    " NEFIN-only (317 plots), Pooled (7,662 plots)"),
            tags$li("Independent test set: 140 held-out NEFIN plots,",
                    " stratified by biomass quartile"),
            tags$li("Random Forest (mtry = floor(p/3)) and XGBoost at fine (10m)",
                    " and coarse (250m) spatial scales"),
            tags$li("Spatial block CV: 25 km blocks, 10 km buffer, 10 folds, seed 42"),
            tags$li("Multi-scale hexagon aggregation: 100 ha to 100,000 ha (DGGRID H3)"),
            tags$li("Monte Carlo coordinate uncertainty: 100 jittered draws within",
                    " 1-mile (1.609 km) FIA fuzz radius per plot"),
            tags$li("Covariates: ETH Canopy Height, Sentinel-2 spectral (10m),",
                    " MODIS spectral (250m), Daymet V4 climate, topography"),
            tags$li("Applications: carbon accounting, biomass mapping, remote sensing",
                    " model design, data integration across sampling frameworks")
          )
        )
      )
    )
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static content - no reactive logic needed
  })
}
