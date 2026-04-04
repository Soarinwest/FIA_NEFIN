# =============================================================================
# Filter Empty Hexagons and Recalculate Metrics
# =============================================================================
# Remove hexagons with 0 plots and update all comparison metrics
# =============================================================================

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  FILTER EMPTY HEXAGONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

scales <- c("100ha", "500ha", "1kha", "2_4kha", "5kha", "10kha", "50kha", "64kha", "100kha")

# =============================================================================
# FILTER BASELINE HEX DATA
# =============================================================================

cat("Step 1: Filtering baseline (FIA-only) hexagons...\n\n")

for (scale in scales) {
  input_file <- paste0("data/processed/hex_aggregated/fia_hex_", scale, ".csv")
  output_file <- paste0("data/processed/hex_aggregated/fia_hex_", scale, "_filtered.csv")
  
  if (file.exists(input_file)) {
    hex <- read_csv(input_file, show_col_types = FALSE)
    
    before <- nrow(hex)
    hex_filtered <- hex %>% filter(n_plots > 0)
    after <- nrow(hex_filtered)
    removed <- before - after
    
    write_csv(hex_filtered, output_file)
    
    cat(sprintf("  %s: %5d → %5d hexes (removed %d empty, %.1f%%)\n",
                scale, before, after, removed, 100*removed/before))
  }
}

cat("\n")

# =============================================================================
# FILTER AUGMENTED HEX DATA
# =============================================================================

cat("Step 2: Filtering augmented (FIA+NEFIN) hexagons...\n\n")

for (scale in scales) {
  input_file <- paste0("data/processed/hex_aggregated/augmented_hex_", scale, ".csv")
  output_file <- paste0("data/processed/hex_aggregated/augmented_hex_", scale, "_filtered.csv")
  
  if (file.exists(input_file)) {
    hex <- read_csv(input_file, show_col_types = FALSE)
    
    before <- nrow(hex)
    hex_filtered <- hex %>% filter(n_plots > 0)
    after <- nrow(hex_filtered)
    removed <- before - after
    
    write_csv(hex_filtered, output_file)
    
    cat(sprintf("  %s: %5d → %5d hexes (removed %d empty, %.1f%%)\n",
                scale, before, after, removed, 100*removed/before))
  }
}

cat("\n")

# =============================================================================
# RECALCULATE COMPARISON METRICS
# =============================================================================

cat("Step 3: Recalculating comparison metrics...\n\n")

comparison_results <- list()

for (scale in scales) {
  baseline_file <- paste0("data/processed/hex_aggregated/fia_hex_", scale, "_filtered.csv")
  augmented_file <- paste0("data/processed/hex_aggregated/augmented_hex_", scale, "_filtered.csv")
  
  if (file.exists(baseline_file) && file.exists(augmented_file)) {
    baseline <- read_csv(baseline_file, show_col_types = FALSE)
    augmented <- read_csv(augmented_file, show_col_types = FALSE)
    
    # Calculate metrics
    metrics <- tibble(
      scale = scale,
      n_hexes = nrow(baseline),
      
      # RMSE
      rmse_baseline = sqrt(mean((baseline$biomass_mean - baseline$biomass_mean)^2, na.rm = TRUE)),
      rmse_augmented = sqrt(mean((augmented$biomass_mean - augmented$biomass_mean)^2, na.rm = TRUE)),
      
      # Mean SE
      mean_se_baseline = mean(baseline$biomass_se, na.rm = TRUE),
      mean_se_augmented = mean(augmented$biomass_se, na.rm = TRUE),
      
      # Correlation
      correlation = cor(baseline$biomass_mean, augmented$biomass_mean, use = "complete.obs"),
      
      # Improvement
      pct_improved = 100 * sum(augmented$biomass_se < baseline$biomass_se, na.rm = TRUE) / n_hexes
    )
    
    comparison_results[[scale]] <- metrics
    
    cat(sprintf("  %s: %d hexes, %.1f%% improved, r=%.3f\n",
                scale, metrics$n_hexes, metrics$pct_improved, metrics$correlation))
  }
}

# Combine and save
comparison_df <- bind_rows(comparison_results)
write_csv(comparison_df, "data/processed/comparison_metrics_filtered.csv")

cat("\n✓ Saved: comparison_metrics_filtered.csv\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Filtered files created with '_filtered' suffix:\n")
cat("  - fia_hex_[scale]_filtered.csv\n")
cat("  - augmented_hex_[scale]_filtered.csv\n")
cat("  - comparison_metrics_filtered.csv\n\n")

cat("Next steps:\n")
cat("  1. Use filtered files for all analyses\n")
cat("  2. Create spatial maps\n")
cat("  3. Document methodology\n\n")

cat("Filter complete!\n\n")
