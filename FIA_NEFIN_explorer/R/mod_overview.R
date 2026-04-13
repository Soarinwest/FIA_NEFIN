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

    # Row 2: Research framing (left) + study area map (right)
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Training Data Composition and Spatial Biomass Estimation"),
        card_body(
          p(
            "This project investigates how training data composition",
            " influences model performance and inference in spatial biomass estimation.",
            " We compare two fundamentally different forest inventory datasets",
            " - not as interchangeable data sources, but as",
            " distinct sampling distributions over the same landscape."
          ),
          tags$dl(
            tags$dt(style = "color:#3b82f6;", "FIA (Forest Inventory and Analysis)"),
            tags$dd(class = "small mb-2",
              "Probability-sampled, spatially representative. 7,345 plots across 7 NE states.",
              " Coordinates administratively fuzzed up to 1 mile."
            ),
            tags$dt(style = "color:#f59e0b;", "NEFIN (Northeast Forest Inventory Network)"),
            tags$dd(class = "small mb-2",
              "Targeted dataset with high structural fidelity in rare, high-biomass forests.",
              " 457 plots, 93.7% measured in 2024. True GPS coordinates, no fuzzing."
            )
          ),
          h6("Core Question"),
          p(
            "How does the distribution of training data affect",
            " model bias, error structure, and ability to generalize across feature space",
            " - particularly for rare but ecologically important conditions?"
          ),
          h6("Key Hypothesis"),
          tags$blockquote(
            class = "small",
            style = "border-left:3px solid #3b82f6; padding-left:12px; color:#94a3b8; font-style:italic;",
            "Model performance is governed more by structural representativeness",
            " of training data than by coordinate precision or spatial resolution."
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Study Area - Northeastern US"),
        card_body(
          style = "padding:8px; text-align:center; aspect-ratio:4/3; overflow:hidden;",
          tags$img(
            src   = "figures/Fig1_Study_Area.png",
            style = "width:100%; height:100%; object-fit:contain; border-radius:4px;"
          )
        )
      )
    ),

    # Row 3: Key findings + analytical approach
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          tags$span(bsicons::bs_icon("graph-up"), " Key Findings")
        ),
        card_body(
          tags$dl(
            tags$dt("Sampling design defines model bias"),
            tags$dd(class = "small",
              "FIA underpredicts high-biomass stands (Q4);",
              " NEFIN overpredicts low-biomass stands (Q1).",
              " Each dataset's bias mirrors its training distribution."
            ),
            tags$dt("Pooling improves performance with tradeoffs"),
            tags$dd(class = "small",
              "Combining FIA + NEFIN reduces test-set RMSE by ~12-15%",
              " vs FIA alone, but introduces a compromise bias structure."
            ),
            tags$dt("Coordinate fuzzing is not the dominant error source"),
            tags$dd(class = "small",
              "Monte Carlo simulation of FIA's 1-mile fuzz radius shows",
              " training data distribution matters more than coordinate precision."
            ),
            tags$dt("Bias increases with spatial aggregation"),
            tags$dd(class = "small",
              "FIA-NEFIN agreement improves at coarser hexagon scales,",
              " but systematic bias persists across all scales."
            )
          )
        )
      ),
      card(
        card_header(
          tags$span(bsicons::bs_icon("gear"), " Analytical Approach")
        ),
        card_body(
          tags$ul(
            class = "small",
            tags$li("Multi-scale aggregation from 100 ha to 100,000 ha hexagon grids (DGGRID H3)"),
            tags$li("Controlled experiments with FIA-only, NEFIN-only, and Pooled training scenarios"),
            tags$li("Spatial cross-validation with 25 km blocks, 10 km buffer, 10 folds, seed 42"),
            tags$li("Distributional analysis including quantile comparisons, tail enrichment, and ECDFs"),
            tags$li("Monte Carlo uncertainty simulation with 100 jittered coordinate draws per FIA plot"),
            tags$li("Random Forest and XGBoost models at fine (10m) and coarse (250m) scales"),
            tags$li("ETH Global Canopy Height 2020 is the top predictor at 100% importance in all models")
          ),
          h6("Takeaway", class = "mt-3"),
          tags$blockquote(
            style = "border-left:3px solid #14b8a6; padding-left:12px; color:#94a3b8; font-style:italic;",
            "In spatial modeling, what your data represent matters more",
            " than how precisely they are located."
          ),
          tags$p(class = "text-muted small mt-2",
            "Applications: carbon accounting, biomass mapping, remote sensing model design,",
            " data integration across sampling frameworks,",
            " and any ML system trained on non-representative data."
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
