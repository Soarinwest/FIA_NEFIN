# ============================================================================
# Custom ggplot2 Theme and Color Palettes
# ============================================================================

# Ensure ggplot2 is loaded
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 package is required but not installed")
}

# Slate palette constants for plots
SLATE_BG    <- "#1e293b"
SLATE_DEEP  <- "#0f172a"
SLATE_TEXT  <- "#e2e8f0"
SLATE_MUTED <- "#94a3b8"
SLATE_GRID  <- "#334155"

# Custom theme for all plots (dark slate) ------------------------------------
theme_fia_nefin <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Text
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 2, color = SLATE_TEXT),
      plot.subtitle = ggplot2::element_text(
        color = SLATE_MUTED, size = base_size - 1),
      axis.title = ggplot2::element_text(face = "bold", color = SLATE_TEXT),
      axis.text = ggplot2::element_text(color = SLATE_MUTED),
      legend.title = ggplot2::element_text(face = "bold", color = SLATE_TEXT),
      legend.text = ggplot2::element_text(color = SLATE_MUTED),

      # Grid
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = SLATE_GRID, linewidth = 0.3),

      # Legend
      legend.position = "top",
      legend.justification = "left",

      # Background
      plot.background = ggplot2::element_rect(fill = SLATE_BG, color = NA),
      panel.background = ggplot2::element_rect(fill = SLATE_BG, color = NA),
      legend.background = ggplot2::element_rect(fill = SLATE_BG, color = NA),
      legend.key = ggplot2::element_rect(fill = SLATE_BG, color = NA),

      # Facet strips
      strip.background = ggplot2::element_rect(fill = SLATE_DEEP, color = NA),
      strip.text = ggplot2::element_text(color = SLATE_TEXT, face = "bold")
    )
}

# Set as default theme
ggplot2::theme_set(theme_fia_nefin())

# Color scale functions -------------------------------------------------------

scale_color_dataset <- function(...) {
  scale_color_manual(
    values = DATASET_COLORS,
    name = "Dataset",
    ...
  )
}

scale_fill_dataset <- function(...) {
  scale_fill_manual(
    values = DATASET_COLORS,
    name = "Dataset",
    ...
  )
}

# Helper for significance colors
scale_color_significance <- function(...) {
  scale_color_manual(
    values = c("TRUE" = "#10b981", "FALSE" = "#64748b"),
    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant"),
    name = "Significance",
    ...
  )
}

# Scenario colors (for modeling tab — matches manuscript Fig S2)
SCENARIO_COLORS <- c(
  "FIA Only"   = "#3b82f6",
  "NEFIN Only" = "#f59e0b",
  "Pooled"     = "#14b8a6"
)

# Plotly dark layout helper — apply after ggplotly()
plotly_dark_layout <- function(p) {
  plotly::layout(p,
    paper_bgcolor = SLATE_BG,
    plot_bgcolor  = SLATE_BG,
    font = list(color = SLATE_TEXT)
  )
}
