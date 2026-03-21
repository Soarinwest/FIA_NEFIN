# ============================================================================
# Custom ggplot2 Theme and Color Palettes
# ============================================================================

# Custom theme for all plots -------------------------------------------------
theme_fia_nefin <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "gray40", size = base_size - 1),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      
      # Grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      
      # Legend
      legend.position = "top",
      legend.justification = "left",
      
      # Background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Set as default theme
theme_set(theme_fia_nefin())

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
    values = c("TRUE" = "#009E73", "FALSE" = "#999999"),
    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant"),
    name = "Significance",
    ...
  )
}
