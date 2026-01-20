# =============================================================================
# Monte Carlo: Analyze Uncertainty from Jitter Library
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: ANALYZE UNCERTAINTY DISTRIBUTIONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD EXTRACTED DATA
# =============================================================================

extracted_dir <- "data/processed/monte_carlo/extracted"

if (!dir.exists(extracted_dir)) {
  stop("Extracted data not found! Run 04_monte_carlo_extract_covariates_v2.R first")
}

extracted_files <- list.files(extracted_dir, pattern = "^rep_\\d{4}_covariates\\.csv$",
                              full.names = TRUE)

cat("Found", length(extracted_files), "extracted replicates\n\n")

if (length(extracted_files) == 0) {
  stop("No extracted files found!")
}

cat("Loading all replicates (this may take a moment)...\n")

all_data <- lapply(extracted_files, function(f) {
  read_csv(f, show_col_types = FALSE)
})

jittered <- bind_rows(all_data)

cat("  Total jittered locations:", nrow(jittered), "\n")
cat("  Unique plots:", n_distinct(jittered$CN), "\n")
cat("  Replicates per plot:", length(extracted_files), "\n\n")

# =============================================================================
# LOAD BASELINE (TRUE COORDINATES)
# =============================================================================

cat("Loading baseline (true coordinates)...\n")

baseline <- read_csv("data/processed/baseline_with_covariates.csv",
                     show_col_types = FALSE)

cat("  Baseline plots:", nrow(baseline), "\n\n")

# =============================================================================
# CALCULATE UNCERTAINTY PER PLOT
# =============================================================================

cat("Calculating uncertainty distributions per plot...\n")

uncertainty <- jittered %>%
  group_by(CN) %>%
  summarise(
    # NDVI MODIS uncertainty
    ndvi_modis_mean = mean(ndvi_modis, na.rm = TRUE),
    ndvi_modis_sd = sd(ndvi_modis, na.rm = TRUE),
    ndvi_modis_min = min(ndvi_modis, na.rm = TRUE),
    ndvi_modis_max = max(ndvi_modis, na.rm = TRUE),
    ndvi_modis_range = max(ndvi_modis, na.rm = TRUE) - min(ndvi_modis, na.rm = TRUE),
    ndvi_modis_cv = sd(ndvi_modis, na.rm = TRUE) / abs(mean(ndvi_modis, na.rm = TRUE)),
    
    # NDVI S2 uncertainty
    ndvi_s2_mean = mean(ndvi_s2, na.rm = TRUE),
    ndvi_s2_sd = sd(ndvi_s2, na.rm = TRUE),
    ndvi_s2_min = min(ndvi_s2, na.rm = TRUE),
    ndvi_s2_max = max(ndvi_s2, na.rm = TRUE),
    ndvi_s2_range = max(ndvi_s2, na.rm = TRUE) - min(ndvi_s2, na.rm = TRUE),
    ndvi_s2_cv = sd(ndvi_s2, na.rm = TRUE) / abs(mean(ndvi_s2, na.rm = TRUE)),
    
    # Temperature uncertainty
    tmean_mean = mean(tmean, na.rm = TRUE),
    tmean_sd = sd(tmean, na.rm = TRUE),
    tmean_range = max(tmean, na.rm = TRUE) - min(tmean, na.rm = TRUE),
    
    # Precipitation uncertainty
    ppt_mean = mean(ppt, na.rm = TRUE),
    ppt_sd = sd(ppt, na.rm = TRUE),
    ppt_range = max(ppt, na.rm = TRUE) - min(ppt, na.rm = TRUE),
    
    # Sample size
    n_valid = sum(!is.na(ndvi_s2)),
    
    .groups = "drop"
  )

# Join with baseline to get original values
uncertainty <- uncertainty %>%
  left_join(baseline %>% select(CN, biomass, ndvi_modis, ndvi_s2, tmean, ppt),
            by = "CN",
            suffix = c("", "_original")) %>%
  mutate(
    # Calculate error from original
    ndvi_modis_error = abs(ndvi_modis_mean - ndvi_modis_original),
    ndvi_s2_error = abs(ndvi_s2_mean - ndvi_s2_original),
    tmean_error = abs(tmean_mean - tmean_original),
    ppt_error = abs(ppt_mean - ppt_original)
  )

cat("  Calculated uncertainty for", nrow(uncertainty), "plots\n\n")

# =============================================================================
# SAVE UNCERTAINTY RESULTS
# =============================================================================

output_path <- "data/processed/monte_carlo/plot_uncertainty.csv"
write_csv(uncertainty, output_path)

cat("✓ Saved:", output_path, "\n\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  UNCERTAINTY SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("NDVI (Sentinel-2) Uncertainty:\n")
cat(sprintf("  Mean SD across plots:   %.4f NDVI units\n",
            mean(uncertainty$ndvi_s2_sd, na.rm = TRUE)))
cat(sprintf("  Median SD:              %.4f NDVI units\n",
            median(uncertainty$ndvi_s2_sd, na.rm = TRUE)))
cat(sprintf("  Mean range:             %.4f NDVI units\n",
            mean(uncertainty$ndvi_s2_range, na.rm = TRUE)))
cat(sprintf("  Mean CV:                %.2f%%\n",
            100 * mean(uncertainty$ndvi_s2_cv, na.rm = TRUE)))
cat(sprintf("  Mean error from true:   %.4f NDVI units\n\n",
            mean(uncertainty$ndvi_s2_error, na.rm = TRUE)))

cat("Temperature Uncertainty:\n")
cat(sprintf("  Mean SD across plots:   %.2f °C\n",
            mean(uncertainty$tmean_sd, na.rm = TRUE)))
cat(sprintf("  Median SD:              %.2f °C\n",
            median(uncertainty$tmean_sd, na.rm = TRUE)))
cat(sprintf("  Mean range:             %.2f °C\n",
            mean(uncertainty$tmean_range, na.rm = TRUE)))
cat(sprintf("  Mean error from true:   %.2f °C\n\n",
            mean(uncertainty$tmean_error, na.rm = TRUE)))

cat("Precipitation Uncertainty:\n")
cat(sprintf("  Mean SD across plots:   %.1f mm\n",
            mean(uncertainty$ppt_sd, na.rm = TRUE)))
cat(sprintf("  Median SD:              %.1f mm\n",
            median(uncertainty$ppt_sd, na.rm = TRUE)))
cat(sprintf("  Mean range:             %.1f mm\n",
            mean(uncertainty$ppt_range, na.rm = TRUE)))
cat(sprintf("  Mean error from true:   %.1f mm\n\n",
            mean(uncertainty$ppt_error, na.rm = TRUE)))

# =============================================================================
# COMPARE TO NEFIN OBSERVED VARIANCE
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  COMPARISON TO NEFIN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load augmented to get NEFIN variance
augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                      show_col_types = FALSE)

# Deduplicate
augmented <- augmented %>%
  group_by(CN) %>%
  filter(MEASYEAR == max(MEASYEAR)) %>%
  ungroup()

# Calculate variance by dataset
variance_by_dataset <- augmented %>%
  group_by(dataset) %>%
  summarise(
    ndvi_s2_sd = sd(ndvi_s2, na.rm = TRUE),
    tmean_sd = sd(tmean, na.rm = TRUE),
    ppt_sd = sd(ppt, na.rm = TRUE),
    .groups = "drop"
  )

cat("Observed variance:\n")
print(variance_by_dataset)
cat("\n")

# Monte Carlo average uncertainty
mc_ndvi_sd <- mean(uncertainty$ndvi_s2_sd, na.rm = TRUE)
mc_temp_sd <- mean(uncertainty$tmean_sd, na.rm = TRUE)
mc_ppt_sd <- mean(uncertainty$ppt_sd, na.rm = TRUE)

# FIA observed variance (from your Phase 1 results)
fia_ndvi_sd <- variance_by_dataset$ndvi_s2_sd[variance_by_dataset$dataset == "FIA"]
nefin_ndvi_sd <- variance_by_dataset$ndvi_s2_sd[variance_by_dataset$dataset == "NEFIN"]

cat("Fuzzing vs Observed Variance:\n\n")

cat("NDVI (S2):\n")
cat(sprintf("  FIA observed variance:    %.4f\n", fia_ndvi_sd))
cat(sprintf("  Monte Carlo fuzzing SD:   %.4f\n", mc_ndvi_sd))
cat(sprintf("  NEFIN observed variance:  %.4f\n", nefin_ndvi_sd))
cat(sprintf("  \n"))
cat(sprintf("  Fuzzing explains %.1f%% of FIA variance\n",
            100 * (mc_ndvi_sd / fia_ndvi_sd)))
cat(sprintf("  NEFIN variance is %.1f%% of fuzzing uncertainty\n\n",
            100 * (nefin_ndvi_sd / mc_ndvi_sd)))

# =============================================================================
# KEY FINDING
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  KEY FINDING\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

fuzzing_contribution <- 100 * (mc_ndvi_sd / fia_ndvi_sd)

cat(sprintf("FIA's ±%.0fm coordinate fuzzing introduces:\n", 
            CONFIG$monte_carlo$jitter_radius_m))
cat(sprintf("  • ±%.4f NDVI uncertainty (1 SD)\n", mc_ndvi_sd))
cat(sprintf("  • ±%.2f °C temperature uncertainty\n", mc_temp_sd))
cat(sprintf("  • ±%.1f mm precipitation uncertainty\n\n", mc_ppt_sd))

cat(sprintf("This explains %.1f%% of FIA's observed NDVI variance (%.4f)\n",
            fuzzing_contribution, fia_ndvi_sd))

if (fuzzing_contribution > 50) {
  cat("\n→ Coordinate fuzzing is a MAJOR source of NDVI variance! ✓\n")
  cat("→ Precise coordinates (like NEFIN) substantially reduce uncertainty\n")
} else if (fuzzing_contribution > 25) {
  cat("\n→ Coordinate fuzzing is a MODERATE source of NDVI variance\n")
  cat("→ Precise coordinates provide meaningful improvement\n")
} else {
  cat("\n→ Coordinate fuzzing is a MINOR source of NDVI variance\n")
  cat("→ Other factors (site heterogeneity) dominate\n")
}

cat("\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO ANALYSIS COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Output files:\n")
cat("  •", output_path, "\n")
cat("  •", extracted_dir, "(individual replicates)\n\n")

cat("Next: Combine with Phase 1 results for complete story\n")
cat("  Rscript R/06_analysis/06_combined_analysis.R\n\n")