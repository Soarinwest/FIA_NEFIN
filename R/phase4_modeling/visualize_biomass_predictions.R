# =============================================================================
# VISUALIZE BIOMASS PREDICTIONS - PROJ CLEARED FIRST
# =============================================================================
# CRITICAL: Clear PostgreSQL PROJ paths BEFORE loading ANY packages
# =============================================================================

# MUST BE FIRST - before any library() calls
Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")
Sys.setenv(PROJ_NETWORK = "OFF")

# Now safe to load packages
library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(viridis)

# =============================================================================
# SETTINGS
# =============================================================================

pred_dir <- "data/predictions/phase4"
fig_dir <- file.path(pred_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

FINE_MODEL <- "rf_fine_scale_10m_pooled"
COARSE_MODEL <- "rf_coarse_scale_250m_pooled"

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  VISUALIZING BIOMASS PREDICTIONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD PREDICTIONS
# =============================================================================

cat("Loading predictions...\n")

fine_path <- file.path(pred_dir, paste0("biomass_fine_", FINE_MODEL, ".tif"))
coarse_path <- file.path(pred_dir, paste0("biomass_coarse_", COARSE_MODEL, ".tif"))
diff_path <- file.path(pred_dir, paste0("biomass_difference_", FINE_MODEL, "_vs_", COARSE_MODEL, ".tif"))
abs_diff_path <- file.path(pred_dir, paste0("biomass_abs_difference_", FINE_MODEL, "_vs_", COARSE_MODEL, ".tif"))

fine <- rast(fine_path)
coarse <- rast(coarse_path)
difference <- rast(diff_path)
abs_diff <- rast(abs_diff_path)

cat("✓ Loaded all predictions\n")

# Mask non-forest pixels (biomass = 0) to NA for forest-only comparison
# Difference rasters are at fine resolution (coarse was resampled to fine)
cat("  Masking non-forest pixels (biomass = 0 → NA)...\n")
fine <- ifel(fine <= 0, NA, fine)
coarse <- ifel(coarse <= 0, NA, coarse)
fine_mask <- is.na(fine)  # same grid as difference/abs_diff
difference <- ifel(fine_mask, NA, difference)
abs_diff <- ifel(fine_mask, NA, abs_diff)
cat("  ✓ Done\n\n")

# =============================================================================
# CREATE PLOTS
# =============================================================================

cat("Creating visualizations...\n\n")

theme_map <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    axis.text = element_text(size = 8),
    panel.grid = element_line(color = "gray90", linewidth = 0.3)
  )

# Plot 1: Fine scale
cat("  1/9: Fine scale map...\n")
p1 <- ggplot() +
  geom_spatraster(data = fine) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Biomass\n(Mg/ha)",
    na.value = "transparent",
    limits = c(0, 300)
  ) +
  labs(
    title = "Forest Biomass - Fine Scale (10m)",
    subtitle = "Chittenden County, Vermont",
    x = NULL, y = NULL
  ) +
  theme_map

ggsave(file.path(fig_dir, "biomass_fine_10m.png"), p1, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Plot 2: Coarse scale
cat("  2/9: Coarse scale map...\n")
p2 <- ggplot() +
  geom_spatraster(data = coarse) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Biomass\n(Mg/ha)",
    na.value = "transparent",
    limits = c(0, 300)
  ) +
  labs(
    title = "Forest Biomass - Coarse Scale (250m)",
    subtitle = "Chittenden County, Vermont",
    x = NULL, y = NULL
  ) +
  theme_map

ggsave(file.path(fig_dir, "biomass_coarse_250m.png"), p2,
       width = 10, height = 8, dpi = 300, bg = "white")

# Plot 3: Side-by-side
cat("  3/9: Side-by-side comparison...\n")
p_combined <- p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Fine vs Coarse Scale Biomass Predictions",
    subtitle = "Chittenden County, Vermont - Random Forest Models",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

ggsave(file.path(fig_dir, "biomass_comparison_side_by_side.png"), p_combined,
       width = 16, height = 8, dpi = 300, bg = "white")

# Plot 4: Difference
cat("  4/9: Difference map...\n")
p3 <- ggplot() +
  geom_spatraster(data = difference) +
  scale_fill_distiller(
    palette = "RdBu",
    name = "Difference\n(Mg/ha)\nFine - Coarse",
    na.value = "transparent",
    limits = c(-120, 120)
  ) +
  labs(
    title = "Biomass Prediction Difference: Fine (10m) - Coarse (250m)",
    subtitle = "Positive = Fine scale predicts more biomass",
    x = NULL, y = NULL
  ) +
  theme_map

ggsave(file.path(fig_dir, "biomass_difference.png"), p3,
       width = 10, height = 8, dpi = 300, bg = "white")

# Plot 5: Absolute difference
cat("  5/9: Absolute difference map...\n")
p4 <- ggplot() +
  geom_spatraster(data = abs_diff) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Absolute\nDifference\n(Mg/ha)",
    na.value = "transparent",
    direction = -1
  ) +
  labs(
    title = "Absolute Biomass Prediction Difference",
    subtitle = "Magnitude of disagreement between scales",
    x = NULL, y = NULL
  ) +
  theme_map

ggsave(file.path(fig_dir, "biomass_absolute_difference.png"), p4,
       width = 10, height = 8, dpi = 300, bg = "white")

# Plot 6: Distributions
cat("  6/9: Distribution comparison...\n")
fine_df <- as.data.frame(fine, xy = TRUE, na.rm = TRUE)
coarse_df <- as.data.frame(coarse, xy = TRUE, na.rm = TRUE)

names(fine_df)[3] <- "biomass"
names(coarse_df)[3] <- "biomass"

fine_df$scale <- "Fine (10m)"
coarse_df$scale <- "Coarse (250m)"

combined_df <- rbind(fine_df, coarse_df)

p5 <- ggplot(combined_df, aes(x = biomass, fill = scale)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  scale_fill_manual(
    values = c("Fine (10m)" = "#2E7D32", "Coarse (250m)" = "#1565C0"),
    name = "Scale"
  ) +
  labs(
    title = "Biomass Distribution by Scale",
    subtitle = "Comparison of fine (10m) vs coarse (250m) predictions",
    x = "Predicted Biomass (Mg/ha)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "biomass_distribution_comparison.png"), p5,
       width = 10, height = 6, dpi = 300, bg = "white")

# Plot 7: Density
cat("  7/9: Density plot...\n")
p6 <- ggplot(combined_df, aes(x = biomass, fill = scale, color = scale)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(
    values = c("Fine (10m)" = "#2E7D32", "Coarse (250m)" = "#1565C0"),
    name = "Scale"
  ) +
  scale_color_manual(
    values = c("Fine (10m)" = "#2E7D32", "Coarse (250m)" = "#1565C0"),
    name = "Scale"
  ) +
  labs(
    title = "Biomass Density Distribution by Scale",
    subtitle = "Kernel density estimates",
    x = "Predicted Biomass (Mg/ha)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "biomass_density_comparison.png"), p6,
       width = 10, height = 6, dpi = 300, bg = "white")

# Plot 8: Summary stats
cat("  8/9: Summary statistics...\n")
stats_df <- data.frame(
  Scale = c("Fine (10m)", "Coarse (250m)"),
  Mean = c(
    global(fine, "mean", na.rm = TRUE)[[1]],
    global(coarse, "mean", na.rm = TRUE)[[1]]
  ),
  SD = c(
    global(fine, "sd", na.rm = TRUE)[[1]],
    global(coarse, "sd", na.rm = TRUE)[[1]]
  )
)

p7 <- ggplot(stats_df, aes(x = Scale, y = Mean, fill = Scale)) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    width = 0.2,
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c("Fine (10m)" = "#2E7D32", "Coarse (250m)" = "#1565C0")
  ) +
  labs(
    title = "Mean Biomass by Scale",
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

ggsave(file.path(fig_dir, "biomass_summary_stats.png"), p7,
       width = 8, height = 6, dpi = 300, bg = "white")

# Plot 9: 4-panel comprehensive
cat("  9/9: Comprehensive 4-panel figure...\n")

p1_panel <- ggplot() +
  geom_spatraster(data = fine) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Biomass (Mg/ha)",
    na.value = "transparent"
  ) +
  labs(title = "Fine Scale (10m)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    axis.text = element_text(size = 7)
  )

p2_panel <- ggplot() +
  geom_spatraster(data = coarse) +
  scale_fill_viridis_c(
    option = "mako",
    name = "Biomass (Mg/ha)",
    na.value = "transparent"
  ) +
  labs(title = "Coarse Scale (250m)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    axis.text = element_text(size = 7)
  )

p3_panel <- ggplot() +
  geom_spatraster(data = difference) +
  scale_fill_distiller(
    palette = "RdBu",
    name = "Difference (Mg/ha)",
    na.value = "transparent"
  ) +
  labs(title = "Difference (Fine - Coarse)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    axis.text = element_text(size = 7)
  )

p4_panel <- ggplot() +
  geom_spatraster(data = abs_diff) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Abs. Diff. (Mg/ha)",
    na.value = "transparent",
    direction = -1
  ) +
  labs(title = "Absolute Difference", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    axis.text = element_text(size = 7)
  )

p_comprehensive <- (p1_panel | p2_panel) / (p3_panel | p4_panel) +
  plot_annotation(
    title = "Forest Biomass Predictions: Scale Comparison",
    subtitle = "Chittenden County, Vermont",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    )
  )

ggsave(file.path(fig_dir, "biomass_4panel_comprehensive.png"), p_comprehensive,
       width = 14, height = 12, dpi = 300, bg = "white")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY STATISTICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print(stats_df)

cat("\nDifference Statistics:\n")
cat("  Mean difference:", round(global(difference, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
cat("  SD of difference:", round(global(difference, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
cat("  Mean absolute difference:", round(global(abs_diff, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VISUALIZATION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Created 9 figures in:", fig_dir, "\n\n")