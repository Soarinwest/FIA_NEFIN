# ============================================================================
# mod_overview.R — Tab 1: Overview
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
        theme    = value_box_theme(bg = "#E69F00", fg = "white")
      ),
      value_box(
        title    = "NEFIN Plots",
        value    = "457",
        showcase = bsicons::bs_icon("geo-alt"),
        theme    = value_box_theme(bg = "#56B4E9", fg = "white")
      ),
      value_box(
        title    = "States",
        value    = "7",
        showcase = bsicons::bs_icon("map"),
        theme    = value_box_theme(bg = "#009E73", fg = "white")
      ),
      value_box(
        title    = "Study Period",
        value    = "2020\u20132024",
        showcase = bsicons::bs_icon("calendar3"),
        theme    = value_box_theme(bg = "#555555", fg = "white")
      )
    ),

    # Row 2: Research framing + study area map
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("About This Study"),
        card_body(
          p(
            "This app explores two complementary forest inventory networks in the",
            "northeastern United States: the ", strong("USDA Forest Inventory and Analysis (FIA)"),
            " program and the ", strong("Northeast Forest Inventory Network (NEFIN)"), "."
          ),
          p(
            "FIA is a systematic probability-based survey covering all forest land in the US.",
            "Its 7,345 northeastern plots span 2020\u20132024, providing a statistically rigorous",
            "baseline but constrained by fixed plot protocols and deliberate coordinate fuzzing",
            "for landowner privacy."
          ),
          p(
            "NEFIN is a research network of 457 intensively measured plots, 93.7% remeasured",
            "in 2024, with origins in the 1960s. NEFIN plots target old-growth and late-successional",
            "stands, capturing large trees systematically underrepresented in FIA."
          ),
          p(
            "Key research questions:"
          ),
          tags$ul(
            tags$li("Does NEFIN overrepresent large trees relative to FIA?"),
            tags$li("At what spatial scale do FIA and NEFIN biomass estimates agree?"),
            tags$li("Does pooling FIA + NEFIN improve remote sensing-based biomass predictions?"),
            tags$li("How does FIA coordinate fuzzing propagate to covariate extraction uncertainty?")
          ),
          p(
            tags$em(
              "All coordinate fuzzing is pre-applied in the source data (FIA plots displaced",
              "up to 1 mile from true location). The Spatial Explorer tab visualizes this",
              "uncertainty directly."
            )
          )
        )
      ),
      card(
        card_header("Study Area — Northeastern US"),
        card_body(
          tags$img(
            src   = "figures/Fig1_Study_Area.png",
            style = "max-width: 100%; border-radius: 4px;"
          )
        )
      )
    ),

    # Row 3: Three info cards
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header(
          tags$span(
            bsicons::bs_icon("database"),
            " What is FIA?"
          )
        ),
        card_body(
          p(
            "The ", strong("Forest Inventory and Analysis"), " program (USDA Forest Service)",
            "conducts the only comprehensive, nationally consistent survey of US forests.",
            "Each plot uses a fixed-radius nested design with four 7.3m subplots."
          ),
          p(
            "Coordinates are ", strong("administratively fuzzed"), " up to 1 mile (1.6 km)",
            "from the true location to protect landowner privacy. This spatial uncertainty",
            "propagates to all covariate extractions."
          ),
          tags$dl(
            tags$dt("Coverage"), tags$dd("7 northeastern states (ME, NH, VT, MA, CT, RI, NY)"),
            tags$dt("Measurement years"), tags$dd("2020\u20132024"),
            tags$dt("Plot count"), tags$dd("7,345 plots"),
            tags$dt("Biomass"), tags$dd("Above-ground live biomass (Mg/ha)")
          )
        )
      ),
      card(
        card_header(
          tags$span(
            bsicons::bs_icon("binoculars"),
            " What is NEFIN?"
          )
        ),
        card_body(
          p(
            "The ", strong("Northeast Forest Inventory Network"), " is a collaborative",
            "research network targeting unmanaged, late-successional, and old-growth forests",
            "across the northeastern US and southeastern Canada."
          ),
          p(
            "Unlike FIA, NEFIN plots are ", strong("not probability-sampled"), " \u2014 they",
            "are located in forests of particular ecological interest. This introduces",
            "compositional bias toward large trees and high biomass stands."
          ),
          tags$dl(
            tags$dt("Plot count"), tags$dd("457 plots (93.7% measured in 2024)"),
            tags$dt("Historical depth"), tags$dd("Network origins in the 1960s"),
            tags$dt("Coordinates"), tags$dd("True GPS coordinates (no fuzzing)"),
            tags$dt("Strength"), tags$dd("Captures large-tree tail missing from FIA")
          )
        )
      ),
      card(
        card_header(
          tags$span(
            bsicons::bs_icon("question-circle"),
            " Why Compare?"
          )
        ),
        card_body(
          p(
            "FIA and NEFIN are ", strong("complementary"), " in critical ways.",
            "FIA provides unbiased regional coverage; NEFIN provides access to",
            "forest structures that FIA rarely encounters."
          ),
          p(
            "When combined (", em("pooled training"), "), they improve remote",
            "sensing-based biomass predictions by capturing the full biomass",
            "distribution \u2014 reducing test-set RMSE by ~12\u201315% versus FIA alone."
          ),
          tags$ul(
            tags$li(strong("Scale dependency:"), " FIA\u2013NEFIN agreement improves at coarser hexagon scales"),
            tags$li(strong("Large-tree bias:"), " NEFIN P99 DBH exceeds FIA by 10\u201340 cm for key species"),
            tags$li(strong("Covariate coverage:"), " ETH canopy height (10m) is the top predictor in all models"),
            tags$li(strong("Climate:"), " Daymet V4 temperature and precipitation are secondary predictors")
          )
        )
      )
    )
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No reactive logic — all content is static
  })
}
