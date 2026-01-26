# =============================================================================
# Hexagon Scale Impact: When Does Precision Matter?
# =============================================================================
# Creates comprehensive visualization showing:
# 1. Scale-dependent effects
# 2. Recommendations for NEFIN vs FIA usage
# 3. Visual map examples at different scales
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  HEXAGON SCALE IMPACT ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD ALL RESULTS
# =============================================================================

cat("Loading analysis results...\n")

# Direct comparison metrics
comparison <- read_csv("data/processed/comparison_metrics.csv",
                       show_col_types = FALSE)

# Monte Carlo uncertainty
mc_results <- read_csv("data/processed/monte_carlo/plot_uncertainty.csv",
                       show_col_types = FALSE)

# Summary statistics
summary_stats <- readRDS("data/processed/summary_statistics/all_metrics.rds")

cat("  ✓ All results loaded\n\n")

# =============================================================================
# SCALE-DEPENDENT METRICS
# =============================================================================

cat("Computing scale-dependent metrics...\n")

scale_metrics <- comparison %>%
  mutate(
    # Extract area for sorting
    area_ha_num = area_ha,
    
    # NDVI uncertainty from MC
    ndvi_uncertainty = mean(mc_results$ndvi_s2_sd, na.rm = TRUE),
    
    # Relative improvement
    rel_improvement = pct_improved / 100,
    
    # Precision value score (higher = more valuable)
    precision_score = case_when(
      pct_improved >= 99 ~ "Critical",
      pct_improved >= 95 ~ "High Value",
      pct_improved >= 90 ~ "Moderate Value",
      TRUE ~ "Low Value"
    ),
    precision_score = factor(precision_score,
                             levels = c("Critical", "High Value", 
                                        "Moderate Value", "Low Value"))
  ) %>%
  arrange(area_ha_num)

cat("  ✓ Metrics computed\n\n")

# =============================================================================
# RECOMMENDATION THRESHOLDS
# =============================================================================

cat("Determining recommendation thresholds...\n")

# Find where improvement drops below 95%
threshold_95 <- scale_metrics %>%
  filter(pct_improved < 95) %>%
  arrange(area_ha_num) %>%
  slice(1)

# Find where improvement drops below 90%
threshold_90 <- scale_metrics %>%
  filter(pct_improved < 90) %>%
  arrange(area_ha_num) %>%
  slice(1)

recommendations <- tibble(
  category = c("Essential", "Recommended", "Optional", "Not Needed"),
  scale_range = c(
    paste0("< ", threshold_95$scale),
    paste0(threshold_95$scale, " - ", threshold_90$scale),
    paste0(threshold_90$scale, " - 50kha"),
    "> 50kha"
  ),
  rationale = c(
    paste0(">", round(threshold_95$pct_improved, 0), "% improvement"),
    "Substantial improvement (90-95%)",
    "Moderate improvement (<90%)",
    "Minimal benefit from precision"
  )
)

cat("  ✓ Thresholds determined\n\n")

# =============================================================================
# VISUALIZATION 1: Scale Dependency
# =============================================================================

cat("Creating scale dependency visualization...\n")

# Create figures directory if it doesn't exist
dir.create("data/processed/figures", showWarnings = FALSE, recursive = TRUE)

# Multi-panel plot showing all key metrics vs scale
p1 <- scale_metrics %>%
  ggplot(aes(x = area_ha_num)) +
  geom_line(aes(y = rmse), size = 1) +
  geom_point(aes(y = rmse, color = precision_score), size = 3) +
  scale_x_log10(
    breaks = scale_metrics$area_ha_num,
    labels = scale_metrics$scale
  ) +
  scale_color_manual(
    values = c("Critical" = "#d62728",
               "High Value" = "#ff7f0e",
               "Moderate Value" = "#2ca02c",
               "Low Value" = "#1f77b4")
  ) +
  labs(
    title = "A. RMSE by Spatial Scale",
    x = "Hex Scale",
    y = "RMSE (Mg/ha)",
    color = "Precision Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- scale_metrics %>%
  ggplot(aes(x = area_ha_num, y = pct_improved)) +
  geom_line(size = 1) +
  geom_point(aes(color = precision_score), size = 3) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "orange", alpha = 0.5) +
  scale_x_log10(
    breaks = scale_metrics$area_ha_num,
    labels = scale_metrics$scale
  ) +
  scale_color_manual(
    values = c("Critical" = "#d62728",
               "High Value" = "#ff7f0e",
               "Moderate Value" = "#2ca02c",
               "Low Value" = "#1f77b4")
  ) +
  labs(
    title = "B. Improvement from Augmentation",
    x = "Hex Scale",
    y = "% Hexes Improved",
    color = "Precision Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- scale_metrics %>%
  ggplot(aes(x = area_ha_num, y = correlation)) +
  geom_line(size = 1) +
  geom_point(aes(color = precision_score), size = 3) +
  scale_x_log10(
    breaks = scale_metrics$area_ha_num,
    labels = scale_metrics$scale
  ) +
  scale_color_manual(
    values = c("Critical" = "#d62728",
               "High Value" = "#ff7f0e",
               "Moderate Value" = "#2ca02c",
               "Low Value" = "#1f77b4")
  ) +
  labs(
    title = "C. Correlation: Baseline vs Augmented",
    x = "Hex Scale",
    y = "Pearson r",
    color = "Precision Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine
combined_plot <- (p1 / p2 / p3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Scale-Dependent Effects of Coordinate Precision",
    subtitle = "Impact of augmenting FIA with NEFIN's precise coordinates"
  )

ggsave("data/processed/figures/scale_dependency_comprehensive.png",
       combined_plot, width = 12, height = 14, dpi = 300)

cat("  ✓ Scale dependency plot saved\n\n")

# =============================================================================
# VISUALIZATION 2: Decision Matrix
# =============================================================================

cat("Creating decision matrix...\n")

decision_data <- scale_metrics %>%
  mutate(
    uncertainty_category = case_when(
      area_ha_num < 1000 ~ "High uncertainty from fuzzing",
      area_ha_num < 10000 ~ "Moderate uncertainty",
      TRUE ~ "Low uncertainty (averaging effect)"
    ),
    recommendation = case_when(
      pct_improved >= 95 ~ "Use NEFIN",
      pct_improved >= 90 ~ "Consider NEFIN",
      TRUE ~ "FIA adequate"
    )
  )

p4 <- decision_data %>%
  ggplot(aes(x = scale, y = 1, fill = recommendation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(pct_improved), "%")),
            size = 5, fontface = "bold") +
  scale_fill_manual(
    values = c("Use NEFIN" = "#2ca02c",
               "Consider NEFIN" = "#ff7f0e",
               "FIA adequate" = "#1f77b4")
  ) +
  labs(
    title = "Decision Matrix: When to Use NEFIN vs FIA",
    subtitle = "Based on scale-dependent precision requirements",
    x = "Spatial Scale",
    y = "",
    fill = "Recommendation"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

ggsave("data/processed/figures/decision_matrix.png",
       p4, width = 14, height = 4, dpi = 300)

cat("  ✓ Decision matrix saved\n\n")

# =============================================================================
# VISUALIZATION 3: Uncertainty Quantification
# =============================================================================

cat("Creating uncertainty quantification plot...\n")

# MC uncertainty distribution
ndvi_uncertainty_dist <- mc_results %>%
  ggplot(aes(x = ndvi_s2_sd)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = mean(mc_results$ndvi_s2_sd, na.rm = TRUE),
             color = "red", linetype = "dashed", size = 1) +
  annotate("text",
           x = mean(mc_results$ndvi_s2_sd, na.rm = TRUE) * 1.2,
           y = Inf,
           label = sprintf("Mean: ±%.4f NDVI", 
                           mean(mc_results$ndvi_s2_sd, na.rm = TRUE)),
           vjust = 2, hjust = 0, size = 5, color = "red") +
  labs(
    title = "NDVI Uncertainty from Coordinate Fuzzing",
    subtitle = "Distribution across 7,345 FIA plots (100 replicates each)",
    x = "NDVI Uncertainty (SD)",
    y = "Number of Plots"
  ) +
  theme_minimal()

ggsave("data/processed/figures/ndvi_uncertainty_distribution.png",
       ndvi_uncertainty_dist, width = 10, height = 6, dpi = 300)

cat("  ✓ Uncertainty plot saved\n\n")

# =============================================================================
# SAVE RECOMMENDATION TABLE
# =============================================================================

output_dir <- "data/processed/recommendations"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(recommendations, file.path(output_dir, "scale_recommendations.csv"))
write_csv(scale_metrics, file.path(output_dir, "scale_metrics_complete.csv"))

cat("✓ Saved recommendation tables\n\n")

# =============================================================================
# CREATE SUMMARY REPORT
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  RECOMMENDATIONS SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("WHEN TO USE NEFIN:\n\n")
print(recommendations)
cat("\n")

cat("KEY METRICS:\n\n")
cat(sprintf("Coordinate fuzzing uncertainty: ±%.4f NDVI (±%.1f%%)\n",
            mean(mc_results$ndvi_s2_sd, na.rm = TRUE),
            100 * mean(mc_results$ndvi_s2_cv, na.rm = TRUE)))
cat(sprintf("NEFIN precision advantage: %.1fx reduction\n",
            mean(mc_results$ndvi_s2_sd, na.rm = TRUE) / 
              sd(filter(readRDS("data/processed/summary_statistics/all_metrics.rds")$diagnostic$summary_by_dataset, 
                        dataset == "NEFIN")$ndvi_s2, na.rm = TRUE)))
cat(sprintf("Critical scale threshold: %s (%.0f ha)\n",
            threshold_95$scale, threshold_95$area_ha))
cat("\n")

cat("PRACTICAL GUIDANCE:\n\n")
cat("1. Fine-scale analyses (<1,000 ha):\n")
cat("   → ESSENTIAL to use precise coordinates\n")
cat("   → Fuzzing introduces substantial spatial error\n\n")

cat("2. Landscape analyses (1,000-10,000 ha):\n")
cat("   → RECOMMENDED to use precise coordinates\n")
cat("   → Meaningful improvement in estimates\n\n")

cat("3. Regional analyses (>50,000 ha):\n")
cat("   → FIA adequate for most applications\n")
cat("   → Spatial averaging reduces fuzzing impact\n\n")

cat("4. NDVI-based applications:\n")
cat("   → Coordinate precision critical (10% uncertainty)\n")
cat("   → Use NEFIN when available at any scale\n\n")

cat("5. Allometric equation development:\n")
cat("   → Check large tree analysis results\n")
cat("   → NEFIN may provide better representation\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  ANALYSIS COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Figures saved in: data/processed/figures/\n")
cat("  • scale_dependency_comprehensive.png\n")
cat("  • decision_matrix.png\n")
cat("  • ndvi_uncertainty_distribution.png\n\n")

cat("Recommendations saved in: data/processed/recommendations/\n")
cat("  • scale_recommendations.csv\n")
cat("  • scale_metrics_complete.csv\n\n")
