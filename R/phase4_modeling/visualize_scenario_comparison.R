# =============================================================================
# VISUALIZE 10M SCENARIO COMPARISON - CRS FIXED
# =============================================================================
# Uses terra::crs() to reset CRS and avoid transformation errors
# =============================================================================

library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(viridis)

Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")


# =============================================================================
# SETTINGS
# =============================================================================

pred_dir <- "data/predictions/phase4/scenario_comparison"
fig_dir <- file.path(pred_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  VISUALIZING 10M SCENARIO COMPARISON (CRS FIXED)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD PREDICTIONS AND FIX CRS
# =============================================================================

cat("Loading predictions and fixing CRS...\n")

predictions <- list()

# Helper function to fix CRS
fix_crs <- function(r) {
  # Set to simple EPSG:5070 without problematic datum info
  crs(r) <- "EPSG:5070"
  return(r)
}

if (file.exists(file.path(pred_dir, "biomass_10m_fia_only.tif"))) {
  predictions$fia <- rast(file.path(pred_dir, "biomass_10m_fia_only.tif"))
  predictions$fia <- fix_crs(predictions$fia)
  cat("  ✓ FIA only (CRS fixed)\n")
}

if (file.exists(file.path(pred_dir, "biomass_10m_nefin_only.tif"))) {
  predictions$nefin <- rast(file.path(pred_dir, "biomass_10m_nefin_only.tif"))
  predictions$nefin <- fix_crs(predictions$nefin)
  cat("  ✓ NEFIN only (CRS fixed)\n")
}

if (file.exists(file.path(pred_dir, "biomass_10m_pooled.tif"))) {
  predictions$pooled <- rast(file.path(pred_dir, "biomass_10m_pooled.tif"))
  predictions$pooled <- fix_crs(predictions$pooled)
  cat("  ✓ Pooled (CRS fixed)\n")
}

if (length(predictions) == 0) {
  stop("No predictions found! Run compare_10m_scenarios.R first.")
}

cat("\n✓ Loaded", length(predictions), "predictions\n")

# Mask non-forest pixels (biomass = 0) to NA for forest-only comparison
cat("  Masking non-forest pixels (biomass = 0 → NA) for comparison...\n")
for (name in names(predictions)) {
  predictions[[name]] <- ifel(predictions[[name]] <= 0, NA, predictions[[name]])
}
cat("  ✓ Done\n\n")

# =============================================================================
# COMMON THEME
# =============================================================================

theme_map <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom",
    axis.text = element_text(size = 7),
    panel.grid = element_line(color = "gray90", linewidth = 0.3)
  )

# =============================================================================
# PLOT 1: THREE-PANEL COMPARISON
# =============================================================================

cat("Creating figures...\n")
cat("  1/4: Three-panel comparison...\n")

plots <- list()

if ("fia" %in% names(predictions)) {
  plots$fia <- ggplot() +
    geom_spatraster(data = predictions$fia) +
    scale_fill_viridis_c(
      option = "mako",
      name = "Biomass\n(Mg/ha)",
      na.value = "transparent",
      limits = c(0, 300)
    ) +
    labs(
      title = "FIA Only (Fuzzed Coordinates)",
      subtitle = "~10,000 plots with coordinate fuzzing",
      x = NULL, y = NULL
    ) +
    theme_map
}

if ("nefin" %in% names(predictions)) {
  plots$nefin <- ggplot() +
    geom_spatraster(data = predictions$nefin) +
    scale_fill_viridis_c(
      option = "mako",
      name = "Biomass\n(Mg/ha)",
      na.value = "transparent",
      limits = c(0, 300)
    ) +
    labs(
      title = "NEFIN Only (Precise Coordinates)",
      subtitle = "~1,500 plots with exact locations",
      x = NULL, y = NULL
    ) +
    theme_map
}

if ("pooled" %in% names(predictions)) {
  plots$pooled <- ggplot() +
    geom_spatraster(data = predictions$pooled) +
    scale_fill_viridis_c(
      option = "mako",
      name = "Biomass\n(Mg/ha)",
      na.value = "transparent",
      limits = c(0, 300)
    ) +
    labs(
      title = "Pooled (FIA + NEFIN)",
      subtitle = "Combined dataset",
      x = NULL, y = NULL
    ) +
    theme_map
}

if (length(plots) == 3) {
  p_combined <- plots$fia + plots$nefin + plots$pooled +
    plot_layout(ncol = 3, guides = "collect") &
    theme(legend.position = "bottom")
  
  p_combined <- p_combined +
    plot_annotation(
      title = "Fine Scale (10m) Biomass: Dataset Comparison",
      subtitle = "Chittenden County, Vermont - Testing coordinate fuzzing effects",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    )
  
  ggsave(
    filename = file.path(fig_dir, "scenario_comparison_3panel.png"),
    plot = p_combined,
    width = 18,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  cat("    ✓ Saved\n")
}

# =============================================================================
# PLOT 2: DISTRIBUTION COMPARISON
# =============================================================================

cat("  2/4: Distribution comparison...\n")

dfs <- list()

for (name in names(predictions)) {
  df <- as.data.frame(predictions[[name]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "biomass"
  df$scenario <- switch(name,
                        fia = "FIA Only",
                        nefin = "NEFIN Only",
                        pooled = "Pooled"
  )
  dfs[[name]] <- df
}

combined_df <- do.call(rbind, dfs)

p_dist <- ggplot(combined_df, aes(x = biomass, fill = scenario)) +
  geom_density(alpha = 0.6, linewidth = 1) +
  scale_fill_manual(
    values = c(
      "FIA Only" = "#D32F2F",
      "NEFIN Only" = "#1976D2",
      "Pooled" = "#388E3C"
    ),
    name = "Dataset"
  ) +
  labs(
    title = "Biomass Distribution by Training Dataset",
    subtitle = "All predictions at 10m resolution",
    x = "Predicted Biomass (Mg/ha)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "top"
  )

ggsave(
  filename = file.path(fig_dir, "scenario_distribution_comparison.png"),
  plot = p_dist,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("    ✓ Saved\n")

# =============================================================================
# PLOT 3: DIFFERENCE MAP (FIA vs NEFIN)
# =============================================================================

if ("fia" %in% names(predictions) && "nefin" %in% names(predictions)) {
  
  cat("  3/4: FIA vs NEFIN difference map...\n")
  
  diff_fia_nefin <- predictions$fia - predictions$nefin
  
  p_diff <- ggplot() +
    geom_spatraster(data = diff_fia_nefin) +
    scale_fill_distiller(
      palette = "RdBu",
      name = "Difference\n(Mg/ha)\nFIA - NEFIN",
      na.value = "transparent",
      limits = c(-100, 100)
    ) +
    labs(
      title = "FIA vs NEFIN: Difference in Biomass Predictions",
      subtitle = "Positive = FIA predicts more (effect of coordinate fuzzing)",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      legend.position = "right",
      panel.grid = element_line(color = "gray90", linewidth = 0.3)
    )
  
  ggsave(
    filename = file.path(fig_dir, "difference_fia_vs_nefin.png"),
    plot = p_diff,
    width = 10,
    height = 8,
    dpi = 300,
    bg = "white"
  )
  
  cat("    ✓ Saved\n")
}

# =============================================================================
# PLOT 4: SUMMARY STATISTICS
# =============================================================================

cat("  4/4: Summary statistics...\n")

stats_df <- data.frame(
  Dataset = character(),
  Mean = numeric(),
  SD = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(predictions)) {
  stats_df <- rbind(stats_df, data.frame(
    Dataset = switch(name,
                     fia = "FIA Only",
                     nefin = "NEFIN Only",
                     pooled = "Pooled"
    ),
    Mean = global(predictions[[name]], "mean", na.rm = TRUE)[[1]],
    SD = global(predictions[[name]], "sd", na.rm = TRUE)[[1]],
    stringsAsFactors = FALSE
  ))
}

p_stats <- ggplot(stats_df, aes(x = Dataset, y = Mean, fill = Dataset)) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    width = 0.2,
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c(
      "FIA Only" = "#D32F2F",
      "NEFIN Only" = "#1976D2",
      "Pooled" = "#388E3C"
    )
  ) +
  labs(
    title = "Mean Biomass by Training Dataset",
    subtitle = "Error bars show ± 1 standard deviation",
    x = NULL,
    y = "Biomass (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  ) +
  geom_text(
    aes(label = paste0(round(Mean, 1), " ± ", round(SD, 1))),
    vjust = -0.5,
    size = 4
  ) +
  ylim(0, max(stats_df$Mean + stats_df$SD) * 1.15)

ggsave(
  filename = file.path(fig_dir, "scenario_summary_stats.png"),
  plot = p_stats,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("    ✓ Saved\n")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY STATISTICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print(stats_df)

# Difference statistics
if ("fia" %in% names(predictions) && "nefin" %in% names(predictions)) {
  diff <- predictions$fia - predictions$nefin
  cat("\nFIA vs NEFIN Difference:\n")
  cat("  Mean:", round(global(diff, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("  SD:", round(global(diff, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("  Mean absolute:", round(global(abs(diff), "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  VISUALIZATION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Created figures:\n")
cat("  • scenario_comparison_3panel.png\n")
cat("  • scenario_distribution_comparison.png\n")
cat("  • difference_fia_vs_nefin.png\n")
cat("  • scenario_summary_stats.png\n\n")

cat("Output directory:", fig_dir, "\n\n")