# =============================================================================
# PHASE 1: Summary Statistics - Complete Metrics Framework
# =============================================================================
# Implements all metrics from the framework:
# 1. Variance/Uncertainty Metrics
# 2. Bias/Shift Metrics  
# 3. Bias-Variance Tradeoff
# 4. Diagnostic Metrics
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)
library(tidyr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 1: SUMMARY STATISTICS - METRICS FRAMEWORK\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading datasets...\n")

baseline <- read_csv("data/processed/baseline_with_covariates.csv", 
                     show_col_types = FALSE)
augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                     show_col_types = FALSE)

# Remove temporal replicates from augmented
augmented <- augmented %>%
  group_by(CN) %>%
  filter(MEASYEAR == max(MEASYEAR)) %>%
  ungroup()

cat("  Baseline: ", nrow(baseline), "plots\n")
cat("  Augmented:", nrow(augmented), "plots\n\n")

# =============================================================================
# 1. VARIANCE / UNCERTAINTY METRICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  1. VARIANCE / UNCERTAINTY METRICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

variance_metrics <- list()

# 1.1 Standard Error (SE) Ratio
cat("1.1 Computing SE ratio by hex scale...\n")

se_results <- list()

for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  # Load hex aggregations
  baseline_hex <- read_csv(
    file.path("data/processed/hex_aggregated", 
              paste0("baseline_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  # Join
  comparison <- baseline_hex %>%
    inner_join(augmented_hex, by = "hex_id", suffix = c("_base", "_aug"))
  
  # Calculate SE ratio
  se_ratio_data <- comparison %>%
    mutate(
      se_ratio = biomass_se_aug / biomass_se_base,
      se_improved = se_ratio < 1
    )
  
  se_results[[scale_name]] <- tibble(
    scale = scale_name,
    mean_se_ratio = mean(se_ratio_data$se_ratio, na.rm = TRUE),
    median_se_ratio = median(se_ratio_data$se_ratio, na.rm = TRUE),
    pct_improved = 100 * mean(se_ratio_data$se_improved, na.rm = TRUE),
    n_hexes = nrow(se_ratio_data)
  )
}

se_summary <- bind_rows(se_results)
variance_metrics$se_ratio <- se_summary

cat("  ✓ SE ratio computed\n")
print(se_summary)
cat("\n")

# 1.2 Coefficient of Variation (CV)
cat("1.2 Computing Coefficient of Variation...\n")

cv_baseline <- sd(baseline$biomass, na.rm = TRUE) / 
               mean(baseline$biomass, na.rm = TRUE)

cv_augmented <- sd(augmented$biomass, na.rm = TRUE) /
                mean(augmented$biomass, na.rm = TRUE)

cv_comparison <- tibble(
  dataset = c("Baseline", "Augmented"),
  cv = c(cv_baseline, cv_augmented),
  cv_pct = c(cv_baseline * 100, cv_augmented * 100)
)

variance_metrics$cv <- cv_comparison

cat("  Baseline CV:  ", sprintf("%.4f (%.2f%%)\n", cv_baseline, cv_baseline * 100))
cat("  Augmented CV: ", sprintf("%.4f (%.2f%%)\n", cv_augmented, cv_augmented * 100))
cat("  Change:       ", sprintf("%.4f\n\n", cv_augmented - cv_baseline))

# 1.3 Bootstrap Variance Reduction
cat("1.3 Bootstrap variance reduction...\n")

n_boot <- 1000
set.seed(42)

bootstrap_baseline <- replicate(n_boot, {
  idx <- sample(1:nrow(baseline), replace = TRUE)
  mean(baseline$biomass[idx], na.rm = TRUE)
})

bootstrap_augmented <- replicate(n_boot, {
  idx <- sample(1:nrow(augmented), replace = TRUE)
  mean(augmented$biomass[idx], na.rm = TRUE)
})

var_baseline <- var(bootstrap_baseline)
var_augmented <- var(bootstrap_augmented)
delta_var <- var_baseline - var_augmented
delta_var_pct <- (delta_var / var_baseline) * 100

bootstrap_summary <- tibble(
  dataset = c("Baseline", "Augmented"),
  var_bootstrap = c(var_baseline, var_augmented),
  delta_var = c(0, delta_var),
  delta_var_pct = c(0, delta_var_pct)
)

variance_metrics$bootstrap <- bootstrap_summary

cat("  Baseline variance:  ", sprintf("%.4f\n", var_baseline))
cat("  Augmented variance: ", sprintf("%.4f\n", var_augmented))
cat("  Δ Variance:         ", sprintf("%.4f (%.2f%% reduction)\n\n", 
                                      delta_var, delta_var_pct))

# 1.4 Effective Sample Size
cat("1.4 Effective sample size...\n")

# For unweighted data, ESS = n
# But calculate anyway for completeness
ess_baseline <- nrow(baseline)
ess_augmented <- nrow(augmented)

ess_summary <- tibble(
  dataset = c("Baseline", "Augmented"),
  n = c(ess_baseline, ess_augmented),
  ess = c(ess_baseline, ess_augmented),
  ess_ratio = c(1, ess_augmented / ess_baseline)
)

variance_metrics$ess <- ess_summary

cat("  Baseline ESS:  ", ess_baseline, "\n")
cat("  Augmented ESS: ", ess_augmented, "\n")
cat("  Ratio:         ", sprintf("%.3f\n\n", ess_augmented / ess_baseline))

# =============================================================================
# 2. BIAS / SHIFT METRICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  2. BIAS / SHIFT METRICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

shift_metrics <- list()

# 2.1 Mean Shift (ΔMean)
cat("2.1 Computing mean shift by scale...\n")

mean_shift_results <- list()

for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  # Load comparison
  comparison_path <- file.path("data/processed/comparisons",
                               paste0("comparison_", scale_name, ".csv"))
  
  if (!file.exists(comparison_path)) next
  
  comparison <- read_csv(comparison_path, show_col_types = FALSE)
  
  delta_mean <- comparison$biomass_mean_aug - comparison$biomass_mean_base
  
  # Bootstrap CI for delta_mean
  boot_delta <- replicate(1000, {
    idx <- sample(1:nrow(comparison), replace = TRUE)
    mean(delta_mean[idx], na.rm = TRUE)
  })
  
  ci_lower <- quantile(boot_delta, 0.025)
  ci_upper <- quantile(boot_delta, 0.975)
  
  mean_shift_results[[scale_name]] <- tibble(
    scale = scale_name,
    mean_delta = mean(delta_mean, na.rm = TRUE),
    median_delta = median(delta_mean, na.rm = TRUE),
    sd_delta = sd(delta_mean, na.rm = TRUE),
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

mean_shift_summary <- bind_rows(mean_shift_results)
shift_metrics$mean_shift <- mean_shift_summary

cat("  ✓ Mean shift computed\n")
print(mean_shift_summary %>% select(scale, mean_delta, median_delta, ci_lower, ci_upper))
cat("\n")

# 2.2 Standardized Mean Difference (SMD)
cat("2.2 Computing Standardized Mean Difference (SMD)...\n")

smd_results <- list()

for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  baseline_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("baseline_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  comparison <- baseline_hex %>%
    inner_join(augmented_hex, by = "hex_id", suffix = c("_base", "_aug"))
  
  # Calculate SMD
  mean_base <- mean(comparison$biomass_mean_base, na.rm = TRUE)
  mean_aug <- mean(comparison$biomass_mean_aug, na.rm = TRUE)
  sd_base <- sd(comparison$biomass_mean_base, na.rm = TRUE)
  sd_aug <- sd(comparison$biomass_mean_aug, na.rm = TRUE)
  
  sd_pooled <- sqrt((sd_base^2 + sd_aug^2) / 2)
  smd <- (mean_aug - mean_base) / sd_pooled
  
  # Interpretation
  if (abs(smd) < 0.1) {
    interpretation <- "negligible"
  } else if (abs(smd) < 0.25) {
    interpretation <- "moderate"
  } else {
    interpretation <- "large"
  }
  
  smd_results[[scale_name]] <- tibble(
    scale = scale_name,
    smd = smd,
    interpretation = interpretation
  )
}

smd_summary <- bind_rows(smd_results)
shift_metrics$smd <- smd_summary

cat("  ✓ SMD computed\n")
print(smd_summary)
cat("\n")

# 2.3 Quantile Shift
cat("2.3 Computing quantile shifts...\n")

quantiles <- c(0.25, 0.5, 0.75, 0.9, 0.95)

quantile_shift <- tibble(
  quantile = quantiles,
  baseline = quantile(baseline$biomass, quantiles, na.rm = TRUE),
  augmented = quantile(augmented$biomass, quantiles, na.rm = TRUE)
) %>%
  mutate(
    delta = augmented - baseline,
    pct_change = (delta / baseline) * 100
  )

shift_metrics$quantiles <- quantile_shift

cat("  ✓ Quantile shifts computed\n")
print(quantile_shift)
cat("\n")

# =============================================================================
# 3. BIAS-VARIANCE TRADEOFF
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  3. BIAS-VARIANCE TRADEOFF\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

tradeoff_metrics <- list()

# 3.1 MSE* Proxy
cat("3.1 Computing MSE* proxy...\n")

mse_results <- list()

for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  baseline_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("baseline_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  comparison <- baseline_hex %>%
    inner_join(augmented_hex, by = "hex_id", suffix = c("_base", "_aug"))
  
  # MSE* = Var(estimate) + (bias)^2
  # Use bootstrap to get Var(estimate)
  var_base <- var(comparison$biomass_mean_base, na.rm = TRUE)
  var_aug <- var(comparison$biomass_mean_aug, na.rm = TRUE)
  
  # Use mean shift as proxy for bias
  delta_mean <- mean(comparison$biomass_mean_aug - comparison$biomass_mean_base, 
                     na.rm = TRUE)
  
  mse_star_base <- var_base + 0^2  # Baseline has no "bias" relative to itself
  mse_star_aug <- var_aug + delta_mean^2
  
  improvement <- (mse_star_base - mse_star_aug) / mse_star_base * 100
  
  mse_results[[scale_name]] <- tibble(
    scale = scale_name,
    mse_star_base = mse_star_base,
    mse_star_aug = mse_star_aug,
    improvement_pct = improvement
  )
}

mse_summary <- bind_rows(mse_results)
tradeoff_metrics$mse_star <- mse_summary

cat("  ✓ MSE* computed\n")
print(mse_summary)
cat("\n")

# =============================================================================
# 4. DIAGNOSTIC METRICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  4. DIAGNOSTIC METRICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

diagnostic_metrics <- list()

# 4.1 Landscape Composition Shift
cat("4.1 Testing landscape composition shifts...\n")

# Separate FIA and NEFIN plots
fia_plots <- augmented %>% filter(dataset == "FIA")
nefin_plots <- augmented %>% filter(dataset == "NEFIN")

# KS tests for each covariate
composition_tests <- tibble(
  covariate = c("NDVI (S2)", "Temperature", "Precipitation", "Biomass"),
  ks_statistic = c(
    ks.test(fia_plots$ndvi_s2, nefin_plots$ndvi_s2)$statistic,
    ks.test(fia_plots$tmean, nefin_plots$tmean)$statistic,
    ks.test(fia_plots$ppt, nefin_plots$ppt)$statistic,
    ks.test(fia_plots$biomass, nefin_plots$biomass)$statistic
  ),
  p_value = c(
    ks.test(fia_plots$ndvi_s2, nefin_plots$ndvi_s2)$p.value,
    ks.test(fia_plots$tmean, nefin_plots$tmean)$p.value,
    ks.test(fia_plots$ppt, nefin_plots$ppt)$p.value,
    ks.test(fia_plots$biomass, nefin_plots$biomass)$p.value
  )
) %>%
  mutate(
    significant = p_value < 0.05,
    interpretation = ifelse(significant, 
                           "Distributions differ significantly",
                           "Distributions similar")
  )

diagnostic_metrics$composition <- composition_tests

cat("  ✓ Composition tests completed\n")
print(composition_tests)
cat("\n")

# 4.2 Summary Statistics by Dataset
cat("4.2 Summary statistics by dataset...\n")

summary_stats <- augmented %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    biomass_mean = mean(biomass, na.rm = TRUE),
    biomass_sd = sd(biomass, na.rm = TRUE),
    ndvi_s2_mean = mean(ndvi_s2, na.rm = TRUE),
    ndvi_s2_sd = sd(ndvi_s2, na.rm = TRUE),
    tmean_mean = mean(tmean, na.rm = TRUE),
    tmean_sd = sd(tmean, na.rm = TRUE),
    ppt_mean = mean(ppt, na.rm = TRUE),
    ppt_sd = sd(ppt, na.rm = TRUE),
    .groups = "drop"
  )

diagnostic_metrics$summary_by_dataset <- summary_stats

cat("  ✓ Summary statistics computed\n")
print(summary_stats)
cat("\n")

# =============================================================================
# SAVE ALL RESULTS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SAVING RESULTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

output_dir <- "data/processed/summary_statistics"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save individual metric tables
write_csv(se_summary, file.path(output_dir, "se_ratio_by_scale.csv"))
write_csv(cv_comparison, file.path(output_dir, "cv_comparison.csv"))
write_csv(bootstrap_summary, file.path(output_dir, "bootstrap_variance.csv"))
write_csv(mean_shift_summary, file.path(output_dir, "mean_shift_by_scale.csv"))
write_csv(smd_summary, file.path(output_dir, "smd_by_scale.csv"))
write_csv(quantile_shift, file.path(output_dir, "quantile_shifts.csv"))
write_csv(mse_summary, file.path(output_dir, "mse_star_by_scale.csv"))
write_csv(composition_tests, file.path(output_dir, "composition_tests.csv"))
write_csv(summary_stats, file.path(output_dir, "summary_by_dataset.csv"))

# Save combined metrics object
saveRDS(list(
  variance = variance_metrics,
  shift = shift_metrics,
  tradeoff = tradeoff_metrics,
  diagnostic = diagnostic_metrics
), file.path(output_dir, "all_metrics.rds"))

cat("✓ All results saved to:", output_dir, "\n\n")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY REPORT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("KEY FINDINGS:\n\n")

cat("1. VARIANCE REDUCTION:\n")
cat("   - Mean SE ratio across scales:", 
    sprintf("%.3f\n", mean(se_summary$mean_se_ratio)))
cat("   - Bootstrap variance reduction:",
    sprintf("%.2f%%\n", delta_var_pct))
cat("   - Interpretation:", 
    ifelse(mean(se_summary$mean_se_ratio) < 1, 
           "NEFIN reduces uncertainty ✓\n\n",
           "NEFIN does not reduce uncertainty ✗\n\n"))

cat("2. ESTIMATE SHIFTS:\n")
cat("   - Mean shift across scales:",
    sprintf("%.2f Mg/ha\n", mean(mean_shift_summary$mean_delta)))
cat("   - Median SMD:",
    sprintf("%.3f (%s)\n", 
            median(smd_summary$smd),
            smd_summary$interpretation[1]))
cat("   - P90 shift:",
    sprintf("%.2f Mg/ha (%.1f%%)\n",
            quantile_shift$delta[quantile_shift$quantile == 0.9],
            quantile_shift$pct_change[quantile_shift$quantile == 0.9]))

cat("\n3. BIAS-VARIANCE TRADEOFF:\n")
cat("   - Mean MSE* improvement:",
    sprintf("%.2f%%\n", mean(mse_summary$improvement_pct)))
cat("   - Interpretation:",
    ifelse(mean(mse_summary$improvement_pct) > 0,
           "Augmentation improves estimates ✓\n\n",
           "Augmentation does not improve estimates ✗\n\n"))

cat("4. COMPOSITION DIFFERENCES:\n")
cat("   - Significant composition shifts in:\n")
for (i in 1:nrow(composition_tests)) {
  if (composition_tests$significant[i]) {
    cat("     •", composition_tests$covariate[i], "\n")
  }
}
cat("\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 1 COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Next steps:\n")
cat("  1. Review results in:", output_dir, "\n")
cat("  2. Run Phase 2 (within-hex variance): R/06_analysis/08_within_hex_variance.R\n")
cat("  3. Start Monte Carlo (overnight): R/06_analysis/03_monte_carlo_generate_jitter.R\n\n")
