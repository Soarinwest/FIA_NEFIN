# =============================================================================
# TRACK 1: Compare Baseline vs Augmented - v3
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE F: COMPARE BASELINE VS AUGMENTED\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# COMPARE ACROSS SCALES
# =============================================================================

results <- list()

for (scale in CONFIG$hex_scales) {
  cat("Comparing", scale$name, "...\n")
  
  # Load hex aggregations
  baseline_path <- file.path("data/processed/hex_aggregated",
                             paste0("baseline_hex_", scale$name, ".csv"))
  augmented_path <- file.path("data/processed/hex_aggregated",
                              paste0("augmented_hex_", scale$name, ".csv"))
  
  if (!file.exists(baseline_path) || !file.exists(augmented_path)) {
    cat("  ⚠ Files not found, skipping\n\n")
    next
  }
  
  baseline <- read_csv(baseline_path, show_col_types = FALSE)
  augmented <- read_csv(augmented_path, show_col_types = FALSE)
  
  # Join on hex_id
  comparison <- baseline %>%
    inner_join(augmented, by = "hex_id", suffix = c("_base", "_aug"))
  
  cat("  Hexagons with both datasets:", nrow(comparison), "\n")
  
  if (nrow(comparison) == 0) {
    cat("  ⚠ No overlapping hexagons, skipping\n\n")
    next
  }
  
  # Calculate metrics
  biomass_diff <- comparison$biomass_mean_aug - comparison$biomass_mean_base
  
  # Extract area BEFORE tibble
  scale_name <- scale$name
  
  # Parse area from name
  area_numeric <- as.numeric(gsub("[^0-9]", "", scale_name))
  if (grepl("kha", scale_name)) {
    area_ha <- area_numeric * 1000
  } else {
    area_ha <- area_numeric
  }
  
  metrics <- tibble(
    scale = scale_name,
    area_ha = area_ha,  # Already extracted
    
    # Sample sizes
    n_hexes = nrow(comparison),
    n_plots_base = sum(comparison$n_plots_base),
    n_plots_aug = sum(comparison$n_plots_aug),
    
    # Biomass metrics
    rmse = sqrt(mean(biomass_diff^2, na.rm = TRUE)),
    mae = mean(abs(biomass_diff), na.rm = TRUE),
    bias = mean(biomass_diff, na.rm = TRUE),
    correlation = cor(comparison$biomass_mean_base, 
                      comparison$biomass_mean_aug,
                      use = "complete.obs"),
    
    # Improvement metrics
    pct_improved = 100 * mean(abs(biomass_diff) < 
                                comparison$biomass_se_base,
                              na.rm = TRUE),
    
    # NDVI metrics
    ndvi_rmse = sqrt(mean((comparison$ndvi_s2_mean_aug - 
                             comparison$ndvi_s2_mean_base)^2,
                          na.rm = TRUE)),
    
    # Climate metrics  
    temp_rmse = sqrt(mean((comparison$tmean_mean_aug -
                             comparison$tmean_mean_base)^2,
                          na.rm = TRUE)),
    ppt_rmse = sqrt(mean((comparison$ppt_mean_aug -
                            comparison$ppt_mean_base)^2,
                         na.rm = TRUE))
  )
  
  results[[scale_name]] <- metrics
  
  cat(sprintf("  RMSE: %.2f Mg/ha\n", metrics$rmse))
  cat(sprintf("  Bias: %.2f Mg/ha\n", metrics$bias))
  cat(sprintf("  Correlation: %.3f\n", metrics$correlation))
  cat(sprintf("  %% Improved: %.1f%%\n\n", metrics$pct_improved))
  
  # Save comparison data
  comparison_output <- file.path("data/processed/comparisons",
                                 paste0("comparison_", scale_name, ".csv"))
  dir.create("data/processed/comparisons", showWarnings = FALSE, recursive = TRUE)
  write_csv(comparison, comparison_output)
}

# =============================================================================
# COMBINE RESULTS
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  COMPARISON SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

metrics_all <- bind_rows(results)

# Save summary table
write_csv(metrics_all, "data/processed/comparison_metrics.csv")

cat("Comparison metrics by scale:\n\n")
print(metrics_all %>% 
        select(scale, area_ha, rmse, bias, correlation, pct_improved) %>%
        arrange(area_ha), 
      n = Inf)

cat("\n✓ Saved: data/processed/comparison_metrics.csv\n\n")

# =============================================================================
# QUICK ANALYSIS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  INITIAL FINDINGS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Find scale with biggest improvement
best_scale <- metrics_all %>% 
  arrange(desc(pct_improved)) %>%
  slice(1)

cat("Biggest improvement at:", best_scale$scale, "\n")
cat(sprintf("  RMSE: %.2f Mg/ha\n", best_scale$rmse))
cat(sprintf("  Improvement: %.1f%%\n\n", best_scale$pct_improved))

# Find scale with smallest RMSE
best_rmse <- metrics_all %>%
  arrange(rmse) %>%
  slice(1)

cat("Lowest RMSE at:", best_rmse$scale, "\n")
cat(sprintf("  RMSE: %.2f Mg/ha\n", best_rmse$rmse))
cat(sprintf("  Correlation: %.3f\n\n", best_rmse$correlation))

# Test if RMSE decreases with scale
if (nrow(metrics_all) > 3) {
  scale_trend <- cor(log(metrics_all$area_ha), metrics_all$rmse, 
                     use = "complete.obs")
  cat("Scale vs RMSE correlation:", sprintf("%.3f", scale_trend), "\n")
  if (scale_trend < 0) {
    cat("  → RMSE decreases as scale increases (precision matters at fine scales)\n")
  } else if (scale_trend > 0) {
    cat("  → RMSE increases as scale increases (unexpected pattern)\n")
  } else {
    cat("  → No clear trend with scale\n")
  }
}

cat("\n")
cat("Next steps:\n")
cat("  1. Run Monte Carlo to quantify uncertainty: R/06_analysis/03_monte_carlo_generate_jitter.R\n")
cat("  2. Create visualizations\n\n")