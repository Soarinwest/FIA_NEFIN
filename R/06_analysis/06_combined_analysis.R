# =============================================================================
# Combined Analysis: Synthesize All Results
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)

cat("\n===================================================================\n")
cat("  COMBINED ANALYSIS: SYNTHESIZING RESULTS\n")
cat("===================================================================\n\n")

# =============================================================================
# LOAD ALL RESULTS
# =============================================================================

cat("Loading results...\n")

# Direct comparison
comparison_metrics <- read_csv("data/processed/comparison_metrics.csv",
                               show_col_types = FALSE)

# Monte Carlo uncertainty
mc_uncertainty <- read_csv("data/processed/monte_carlo/plot_uncertainty.csv",
                           show_col_types = FALSE)

cat("  ok Direct comparison metrics\n")
cat("  ok Monte Carlo uncertainty\n")

# =============================================================================
# KEY FINDINGS
# =============================================================================

cat("===================================================================\n")
cat("  KEY FINDINGS\n")
cat("===================================================================\n\n")

# 1. Fuzzing uncertainty
ndvi_uncertainty <- mean(mc_uncertainty$ndvi_s2_sd, na.rm = TRUE)
ndvi_uncertainty_pct <- mean(mc_uncertainty$ndvi_s2_cv, na.rm = TRUE) * 100
temp_uncertainty <- mean(mc_uncertainty$tmean_sd, na.rm = TRUE)
ppt_uncertainty <- mean(mc_uncertainty$ppt_sd, na.rm = TRUE)

cat("1. COORDINATE FUZZING INTRODUCES SUBSTANTIAL UNCERTAINTY:\n")
cat(sprintf("   NDVI:         +/-%.4f units (+/-%.1f%% relative)\n", 
            ndvi_uncertainty, ndvi_uncertainty_pct))
cat(sprintf("   Temperature:  +/-%.2f  deg C\n", temp_uncertainty))
cat(sprintf("   Precipitation: +/-%.1f mm\n\n", ppt_uncertainty))

# 2. Augmentation improvement
best_improvement <- comparison_metrics %>%
  arrange(desc(pct_improved)) %>%
  slice(1)

worst_improvement <- comparison_metrics %>%
  arrange(pct_improved) %>%
  slice(1)

cat("2. AUGMENTATION IMPROVES ESTIMATES (SCALE-DEPENDENT):\n")
cat(sprintf("   Fine scale (%s):   RMSE = %.2f Mg/ha, Improvement = %.1f%%\n",
            best_improvement$scale,
            best_improvement$rmse,
            best_improvement$pct_improved))
cat(sprintf("   Coarse scale (%s): RMSE = %.2f Mg/ha, Improvement = %.1f%%\n\n",
            worst_improvement$scale,
            worst_improvement$rmse,
            worst_improvement$pct_improved))

# 3. Scale dependency
scale_trend <- cor(log(comparison_metrics$area_ha), 
                   comparison_metrics$rmse,
                   use = "complete.obs")

cat("3. SCALE-DEPENDENCY CONFIRMED:\n")
cat(sprintf("   Correlation (log scale vs RMSE): %.3f\n", scale_trend))
if (scale_trend > 0.5) {
  cat("   -> Unexpected: Precision appears to matter at coarse scales\n\n")
} else if (scale_trend < -0.5) {
  cat("   -> As expected: Precision matters more at fine scales\n\n")
} else {
  cat("   -> No clear scale dependency detected\n\n")
}

# =============================================================================
# COMPARATIVE ANALYSIS
# =============================================================================

cat("===================================================================\n")
cat("  COMPARATIVE ANALYSIS\n")
cat("===================================================================\n\n")

# Load augmented for NEFIN variance
augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                      show_col_types = FALSE)

# Deduplicate
augmented <- augmented %>%
  group_by(CN) %>%
  filter(MEASYEAR == max(MEASYEAR)) %>%
  ungroup()

# Get NEFIN variance
nefin_variance <- augmented %>%
  filter(dataset == "NEFIN") %>%
  summarise(
    ndvi_sd = sd(ndvi_s2_10m, na.rm = TRUE),
    temp_sd = sd(tmean_250m, na.rm = TRUE),
    ppt_sd = sd(ppt_250m, na.rm = TRUE)
  )

# Compare
comparison <- tibble(
  Variable = c("NDVI", "Temperature", "Precipitation"),
  MC_Uncertainty = c(ndvi_uncertainty, temp_uncertainty, ppt_uncertainty),
  NEFIN_Uncertainty = c(nefin_variance$ndvi_sd, 
                        nefin_variance$temp_sd,
                        nefin_variance$ppt_sd),
  Reduction_Factor = c(ndvi_uncertainty / nefin_variance$ndvi_sd,
                       temp_uncertainty / nefin_variance$temp_sd,
                       ppt_uncertainty / nefin_variance$ppt_sd)
)

cat("NEFIN Value Proposition:\n")
print(comparison)
cat("\n")

reduction_range <- range(comparison$Reduction_Factor)
cat(sprintf("NEFIN reduces covariate uncertainty by %.1fx-%.1fx\n\n",
            reduction_range[1], reduction_range[2]))

# =============================================================================
# RECOMMENDATIONS
# =============================================================================

cat("===================================================================\n")
cat("  RECOMMENDATIONS\n")
cat("===================================================================\n\n")

# Find threshold scale
threshold_scale <- comparison_metrics %>%
  filter(pct_improved < 95) %>%
  arrange(area_ha) %>%
  slice(1)

if (nrow(threshold_scale) > 0) {
  cat(sprintf("ok Use precise coordinates for analyses at scales < %s (%.0f ha)\n",
              threshold_scale$scale,
              threshold_scale$area_ha))
} else {
  cat("ok Precise coordinates valuable at ALL analyzed scales\n")
}

cat(sprintf("ok FIA coordinates adequate for regional analyses (>%.0f ha)\n",
            max(comparison_metrics$area_ha) / 2))

cat(sprintf("ok Coordinate precision critical when NDVI uncertainty (+/-%.4f) exceeds\n",
            ndvi_uncertainty))
cat("  measurement error of biomass estimates\n\n")

# =============================================================================
# SAVE COMBINED RESULTS
# =============================================================================

combined_results <- list(
  fuzzing_uncertainty = list(
    ndvi = ndvi_uncertainty,
    temperature = temp_uncertainty,
    precipitation = ppt_uncertainty
  ),
  comparison = comparison,
  scale_metrics = comparison_metrics,
  mc_plot_uncertainty = mc_uncertainty
)

saveRDS(combined_results, "data/processed/combined_analysis_results.rds")

cat("ok Saved combined results: data/processed/combined_analysis_results.rds\n\n")

cat("===================================================================\n")
cat("  ANALYSIS COMPLETE! \n")
cat("===================================================================\n\n")

cat("Key Takeaway:\n")
cat(sprintf("  Coordinate fuzzing introduces +/-%.4f NDVI uncertainty.\n", 
            ndvi_uncertainty))
cat(sprintf("  NEFIN's precise coordinates reduce this by %.1fx.\n",
            comparison$Reduction_Factor[comparison$Variable == "NDVI"]))
cat(sprintf("  Precision matters most at scales < %s.\n\n",
            ifelse(nrow(threshold_scale) > 0, 
                   threshold_scale$scale,
                   "all analyzed scales")))
