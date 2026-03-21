# =============================================================================
# PHASE 4: FUZZING IMPACT ANALYSIS
# =============================================================================
# Quantifies how coordinate fuzzing (FIA) vs precise locations (NEFIN)
# affects biomass prediction accuracy at different spatial scales.
#
# ANALYSES:
#   1. Test set predictions from all models (same held-out NEFIN plots)
#   2. Observed vs predicted scatterplots per scenario/scale
#   3. Error stratified by biomass class (quantiles)
#   4. Error stratified by terrain complexity (slope/TRI)
#   5. Scale interaction plot: does fuzzing hurt more at 10m than 250m?
#   6. Paired statistical tests (same plots, different models)
#
# INPUT:
#   - Saved models: data/processed/phase4_models/*.rds
#   - Test data: data/processed/phase4_modeling/test_data_with_water_urban.csv
#   - CV results: data/processed/phase4_cv_results/cv_summary.csv
#   - Fold results: data/processed/phase4_cv_results/fold_results.csv
#
# OUTPUT:
#   - Figures: manuscript_figures/phase4/fuzzing_analysis/
#   - Summary CSV: data/processed/phase4_cv_results/fuzzing_impact_summary.csv
# =============================================================================

Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")

source("R/00_config/config.R")
source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(randomForest)
library(xgboost)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: FUZZING IMPACT ANALYSIS\n")
cat("  Does coordinate precision affect biomass prediction?\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Output directories
fig_dir <- "manuscript_figures/phase4/fuzzing_analysis"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

model_dir <- "data/processed/phase4_models"
data_dir <- "data/processed/phase4_modeling"

# =============================================================================
# STEP 1: LOAD TEST DATA AND ALL MODELS
# =============================================================================

cat("Step 1: Loading test data and models...\n\n")

# Load test data (same held-out NEFIN plots for all models)
test_file <- file.path(data_dir, "test_data_with_water_urban.csv")
if (!file.exists(test_file)) {
  test_file <- file.path(data_dir, "test_data.csv")
}
test_data <- read_csv(test_file, show_col_types = FALSE) %>%
  mutate(CN = as.character(CN))

# Keep only forest plots for comparison (water/urban not relevant here)
test_forest <- test_data %>% filter(biomass > 0)
cat("  Test set:", nrow(test_forest), "forest plots\n")
cat("  Biomass range:", round(min(test_forest$biomass), 1), "to", 
    round(max(test_forest$biomass), 1), "Mg/ha\n")
cat("  Mean:", round(mean(test_forest$biomass), 1), "Mg/ha\n\n")

# Load all RF models (focus on RF for cleaner comparison)
model_files <- list.files(model_dir, pattern = "^rf_.*\\.rds$", full.names = TRUE)
cat("  Found", length(model_files), "RF models:\n")
for (f in model_files) cat("    •", basename(f), "\n")
cat("\n")

# Parse model info and load
models <- list()
for (f in model_files) {
  m <- readRDS(f)
  fname <- tools::file_path_sans_ext(basename(f))
  
  # Parse scale and scenario from filename
  # Format: rf_fine_scale_(10m)_fia_only, rf_coarse_scale_(250m)_pooled, etc.
  scale_label <- if (grepl("fine", fname)) "10m" else "250m"
  scenario <- if (grepl("fia_only", fname)) "FIA Only" 
              else if (grepl("nefin_only", fname)) "NEFIN Only"
              else if (grepl("pooled", fname)) "Pooled"
              else "Unknown"
  
  models[[fname]] <- list(
    model = m$model,
    scaling_means = m$scaling_means,
    scaling_sds = m$scaling_sds,
    covariates = m$covariates,
    metadata = m$metadata,
    scale = scale_label,
    scenario = scenario,
    filename = fname
  )
  
  cat("  Loaded:", fname, "\n")
  cat("    Scale:", scale_label, "| Scenario:", scenario, 
      "| Covariates:", length(m$covariates), "\n")
}
cat("\n")

# =============================================================================
# STEP 2: PREDICT ON TEST SET WITH ALL MODELS
# =============================================================================

cat("Step 2: Predicting on test set with all models...\n\n")

all_predictions <- data.frame()

for (name in names(models)) {
  m <- models[[name]]
  covs <- m$covariates
  
  # Check covariate availability
  available <- intersect(covs, names(test_forest))
  if (length(available) < length(covs)) {
    cat("  ⚠", name, ": missing", length(covs) - length(available), "covariates\n")
    covs <- available
  }
  
  if (length(covs) == 0) {
    cat("  ✗", name, ": no covariates available, skipping\n")
    next
  }
  
  # Prepare test data (standardize using training means/sds)
  test_scaled <- test_forest
  for (cov in covs) {
    if (cov %in% names(m$scaling_means) && cov %in% names(m$scaling_sds)) {
      test_scaled[[cov]] <- (test_scaled[[cov]] - m$scaling_means[cov]) / m$scaling_sds[cov]
    }
  }
  
  # Remove rows with NA covariates
  complete_idx <- complete.cases(test_scaled[, covs])
  test_complete <- test_scaled[complete_idx, ]
  
  if (nrow(test_complete) < 10) {
    cat("  ✗", name, ": too few complete cases (", nrow(test_complete), ")\n")
    next
  }
  
  # Predict
  X_test <- as.matrix(test_complete[, covs])
  preds <- predict(m$model, X_test)
  preds <- pmax(preds, 0)  # clamp negatives
  
  observed <- test_complete$biomass
  
  # Metrics
  rmse <- sqrt(mean((preds - observed)^2))
  mae <- mean(abs(preds - observed))
  ss_res <- sum((observed - preds)^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r2 <- 1 - ss_res / ss_tot
  bias <- mean(preds - observed)
  
  cat(sprintf("  %-40s  RMSE=%.1f  R²=%.3f  MAE=%.1f  Bias=%.1f  (n=%d)\n",
              name, rmse, r2, mae, bias, nrow(test_complete)))
  
  # Store per-plot predictions
  plot_preds <- data.frame(
    CN = test_complete$CN,
    observed = observed,
    predicted = preds,
    residual = preds - observed,
    abs_error = abs(preds - observed),
    model = name,
    scale = m$scale,
    scenario = m$scenario,
    rmse = rmse,
    r2 = r2,
    mae = mae,
    bias = bias
  )
  
  # Add terrain info if available
  terrain_cols <- c("slope_10m", "slope_250m", "elevation_10m", "elevation_250m",
                    "tri_10m", "tri_250m")
  for (tc in terrain_cols) {
    if (tc %in% names(test_complete)) {
      # Use raw (unstandardized) values
      if (tc %in% names(m$scaling_means)) {
        plot_preds[[tc]] <- test_complete[[tc]] * m$scaling_sds[tc] + m$scaling_means[tc]
      } else {
        plot_preds[[tc]] <- test_complete[[tc]]
      }
    }
  }
  
  all_predictions <- bind_rows(all_predictions, plot_preds)
}

cat("\n  Total plot-level predictions:", nrow(all_predictions), "\n\n")

# =============================================================================
# STEP 3: OBSERVED VS PREDICTED SCATTERPLOTS
# =============================================================================

cat("Step 3: Observed vs predicted plots...\n\n")

# Faceted by scale and scenario
max_val <- max(c(all_predictions$observed, all_predictions$predicted), na.rm = TRUE) * 1.05

p_obs_pred <- ggplot(all_predictions, aes(x = observed, y = predicted)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.3, size = 1.2, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 0.8) +
  facet_grid(scale ~ scenario) +
  geom_text(
    data = all_predictions %>% 
      distinct(scale, scenario, rmse, r2) %>%
      mutate(label = paste0("RMSE = ", round(rmse, 1), "\nR² = ", round(r2, 3))),
    aes(label = label),
    x = max_val * 0.05, y = max_val * 0.92,
    hjust = 0, size = 3, fontface = "bold"
  ) +
  coord_fixed(xlim = c(0, max_val), ylim = c(0, max_val)) +
  labs(
    title = "Observed vs Predicted Biomass",
    subtitle = "Same held-out NEFIN test plots across all models",
    x = "Observed biomass (Mg/ha)",
    y = "Predicted biomass (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(file.path(fig_dir, "obs_vs_pred_all_models.png"), p_obs_pred,
       width = 11, height = 7, dpi = 300)
cat("  ✓ obs_vs_pred_all_models.png\n")

# =============================================================================
# STEP 4: ERROR BY BIOMASS CLASS
# =============================================================================

cat("\nStep 4: Error by biomass class...\n\n")

# Create biomass quantile classes
all_predictions <- all_predictions %>%
  mutate(
    biomass_class = cut(observed,
      breaks = quantile(observed, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
      include.lowest = TRUE
    )
  )

# RMSE by biomass class per scenario and scale
error_by_class <- all_predictions %>%
  group_by(scale, scenario, biomass_class) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean(residual^2)),
    mae = mean(abs_error),
    bias = mean(residual),
    mean_obs = mean(observed),
    .groups = "drop"
  )

cat("  Error by biomass quartile:\n\n")
print(error_by_class %>% 
        select(scale, scenario, biomass_class, n, rmse, bias) %>%
        arrange(scale, biomass_class, scenario), n = 30)
cat("\n")

p_error_class <- ggplot(error_by_class, 
                        aes(x = biomass_class, y = rmse, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ scale) +
  scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                               "NEFIN Only" = "#2ECC71", 
                               "Pooled" = "#3498DB")) +
  labs(
    title = "Prediction Error by Biomass Class",
    subtitle = "Does fuzzing hurt more for high-biomass stands?",
    x = "Biomass quartile", y = "RMSE (Mg/ha)", fill = "Scenario"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "error_by_biomass_class.png"), p_error_class,
       width = 10, height = 6, dpi = 300)
cat("  ✓ error_by_biomass_class.png\n")

# Bias by class (does FIA systematically over/under-predict?)
p_bias_class <- ggplot(error_by_class, 
                       aes(x = biomass_class, y = bias, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ scale) +
  scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                               "NEFIN Only" = "#2ECC71", 
                               "Pooled" = "#3498DB")) +
  labs(
    title = "Prediction Bias by Biomass Class",
    subtitle = "Positive = overprediction, Negative = underprediction",
    x = "Biomass quartile", y = "Mean bias (Mg/ha)", fill = "Scenario"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "bias_by_biomass_class.png"), p_bias_class,
       width = 10, height = 6, dpi = 300)
cat("  ✓ bias_by_biomass_class.png\n")

# =============================================================================
# STEP 5: ERROR BY TERRAIN COMPLEXITY
# =============================================================================

cat("\nStep 5: Error by terrain complexity...\n\n")

# Find the best terrain column available
slope_col <- intersect(c("slope_10m", "slope_250m"), names(all_predictions))

if (length(slope_col) > 0) {
  
  # Use finest resolution slope available
  slope_use <- slope_col[1]
  cat("  Using terrain metric:", slope_use, "\n")
  
  all_predictions <- all_predictions %>%
    mutate(
      slope_val = .data[[slope_use]],
      terrain_class = cut(slope_val,
        breaks = quantile(slope_val, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
        labels = c("Flat", "Moderate", "Steep"),
        include.lowest = TRUE
      )
    )
  
  # RMSE by terrain class
  error_by_terrain <- all_predictions %>%
    filter(!is.na(terrain_class)) %>%
    group_by(scale, scenario, terrain_class) %>%
    summarise(
      n = n(),
      rmse = sqrt(mean(residual^2)),
      mae = mean(abs_error),
      bias = mean(residual),
      .groups = "drop"
    )
  
  cat("\n  Error by terrain:\n\n")
  print(error_by_terrain %>% 
          select(scale, scenario, terrain_class, n, rmse) %>%
          arrange(scale, terrain_class, scenario), n = 30)
  cat("\n")
  
  p_error_terrain <- ggplot(error_by_terrain,
                            aes(x = terrain_class, y = rmse, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~ scale) +
    scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                                 "NEFIN Only" = "#2ECC71", 
                                 "Pooled" = "#3498DB")) +
    labs(
      title = "Prediction Error by Terrain Complexity",
      subtitle = "Fuzzing should matter more on steep/heterogeneous terrain",
      x = paste0("Terrain class (", slope_use, ")"),
      y = "RMSE (Mg/ha)", fill = "Scenario"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom"
    )
  
  ggsave(file.path(fig_dir, "error_by_terrain.png"), p_error_terrain,
         width = 10, height = 6, dpi = 300)
  cat("  ✓ error_by_terrain.png\n")
  
} else {
  cat("  ⚠ No slope/terrain covariates found in test data — skipping\n")
}

# =============================================================================
# STEP 6: SCALE INTERACTION — THE KEY PLOT
# =============================================================================

cat("\nStep 6: Scale × Scenario interaction (key finding)...\n\n")

# Summary table: RMSE and R² by scale × scenario
interaction_summary <- all_predictions %>%
  group_by(scale, scenario) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean(residual^2)),
    r2 = {
      ss_res <- sum(residual^2)
      ss_tot <- sum((observed - mean(observed))^2)
      1 - ss_res / ss_tot
    },
    mae = mean(abs_error),
    bias = mean(residual),
    .groups = "drop"
  )

cat("  Scale × Scenario summary:\n\n")
print(interaction_summary, n = 20)
cat("\n")

# RMSE improvement relative to FIA
improvement <- interaction_summary %>%
  select(scale, scenario, rmse) %>%
  pivot_wider(names_from = scenario, values_from = rmse) %>%
  mutate(
    nefin_improvement = `FIA Only` - `NEFIN Only`,
    nefin_pct = round(nefin_improvement / `FIA Only` * 100, 1),
    pooled_improvement = `FIA Only` - Pooled,
    pooled_pct = round(pooled_improvement / `FIA Only` * 100, 1)
  )

cat("  RMSE improvement over FIA (fuzzed):\n\n")
print(improvement)
cat("\n")

# The key plot: RMSE by scenario, faceted by scale
p_interaction <- ggplot(interaction_summary, 
                        aes(x = scenario, y = rmse, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(rmse, 1), "\n(R²=", round(r2, 3), ")")),
            vjust = -0.3, size = 3.2, fontface = "bold") +
  facet_wrap(~ scale, scales = "free_y") +
  scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                               "NEFIN Only" = "#2ECC71", 
                               "Pooled" = "#3498DB")) +
  labs(
    title = "Effect of Coordinate Precision on Prediction Accuracy",
    subtitle = "Hypothesis: fuzzing hurts at 10m but not at 250m",
    x = NULL, y = "RMSE (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 10)
  ) +
  ylim(0, max(interaction_summary$rmse) * 1.25)

ggsave(file.path(fig_dir, "scale_interaction_rmse.png"), p_interaction,
       width = 9, height = 5.5, dpi = 300)
cat("  ✓ scale_interaction_rmse.png\n")

# Paired comparison: improvement at each scale
improvement_long <- improvement %>%
  select(scale, nefin_improvement, pooled_improvement) %>%
  pivot_longer(-scale, names_to = "comparison", values_to = "rmse_reduction") %>%
  mutate(comparison = ifelse(grepl("nefin", comparison), 
                             "NEFIN vs FIA", "Pooled vs FIA"))

p_improvement <- ggplot(improvement_long, 
                        aes(x = scale, y = rmse_reduction, fill = comparison)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("NEFIN vs FIA" = "#2ECC71", 
                               "Pooled vs FIA" = "#3498DB")) +
  labs(
    title = "RMSE Improvement from Precise Coordinates",
    subtitle = "Positive = precise coordinates reduce error",
    x = "Resolution", y = "RMSE reduction (Mg/ha)", fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "rmse_improvement_by_scale.png"), p_improvement,
       width = 7, height = 5, dpi = 300)
cat("  ✓ rmse_improvement_by_scale.png\n")

# =============================================================================
# STEP 7: STATISTICAL SIGNIFICANCE (PAIRED TESTS)
# =============================================================================

cat("\nStep 7: Statistical significance tests...\n\n")

cat("  Paired tests: same plots, different models\n")
cat("  H0: no difference in absolute prediction error\n\n")

# For each scale, compare FIA vs NEFIN and FIA vs Pooled
stat_results <- data.frame()

for (s in c("10m", "250m")) {
  
  # Get plot-level errors
  fia_errors <- all_predictions %>% 
    filter(scale == s, scenario == "FIA Only") %>%
    select(CN, fia_abs_error = abs_error)
  
  nefin_errors <- all_predictions %>%
    filter(scale == s, scenario == "NEFIN Only") %>%
    select(CN, nefin_abs_error = abs_error)
  
  pooled_errors <- all_predictions %>%
    filter(scale == s, scenario == "Pooled") %>%
    select(CN, pooled_abs_error = abs_error)
  
  # Merge by plot (paired comparison)
  paired_fn <- inner_join(fia_errors, nefin_errors, by = "CN")
  paired_fp <- inner_join(fia_errors, pooled_errors, by = "CN")
  
  # Paired Wilcoxon tests (non-parametric, no normality assumption)
  if (nrow(paired_fn) >= 10) {
    test_fn <- wilcox.test(paired_fn$fia_abs_error, paired_fn$nefin_abs_error,
                           paired = TRUE, alternative = "greater")
    
    cat(sprintf("  %s: FIA vs NEFIN (n=%d paired plots)\n", s, nrow(paired_fn)))
    cat(sprintf("    Mean |error| FIA: %.1f  NEFIN: %.1f\n",
                mean(paired_fn$fia_abs_error), mean(paired_fn$nefin_abs_error)))
    cat(sprintf("    Wilcoxon p = %.4f  %s\n\n",
                test_fn$p.value,
                ifelse(test_fn$p.value < 0.05, "★ SIGNIFICANT", "(not significant)")))
    
    stat_results <- bind_rows(stat_results, data.frame(
      scale = s, comparison = "FIA vs NEFIN",
      n_paired = nrow(paired_fn),
      mean_error_fia = mean(paired_fn$fia_abs_error),
      mean_error_other = mean(paired_fn$nefin_abs_error),
      p_value = test_fn$p.value,
      significant = test_fn$p.value < 0.05
    ))
  }
  
  if (nrow(paired_fp) >= 10) {
    test_fp <- wilcox.test(paired_fp$fia_abs_error, paired_fp$pooled_abs_error,
                           paired = TRUE, alternative = "greater")
    
    cat(sprintf("  %s: FIA vs Pooled (n=%d paired plots)\n", s, nrow(paired_fp)))
    cat(sprintf("    Mean |error| FIA: %.1f  Pooled: %.1f\n",
                mean(paired_fp$fia_abs_error), mean(paired_fp$pooled_abs_error)))
    cat(sprintf("    Wilcoxon p = %.4f  %s\n\n",
                test_fp$p.value,
                ifelse(test_fp$p.value < 0.05, "★ SIGNIFICANT", "(not significant)")))
    
    stat_results <- bind_rows(stat_results, data.frame(
      scale = s, comparison = "FIA vs Pooled",
      n_paired = nrow(paired_fp),
      mean_error_fia = mean(paired_fp$fia_abs_error),
      mean_error_other = mean(paired_fp$pooled_abs_error),
      p_value = test_fp$p.value,
      significant = test_fp$p.value < 0.05
    ))
  }
}

# Plot paired error distributions
p_paired <- all_predictions %>%
  select(CN, scale, scenario, abs_error) %>%
  ggplot(aes(x = scenario, y = abs_error, fill = scenario)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.3) +
  facet_wrap(~ scale) +
  scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                               "NEFIN Only" = "#2ECC71", 
                               "Pooled" = "#3498DB")) +
  labs(
    title = "Distribution of Absolute Prediction Errors",
    subtitle = "Paired comparison: same test plots across all scenarios",
    x = NULL, y = "Absolute error (Mg/ha)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "none"
  )

ggsave(file.path(fig_dir, "paired_error_distributions.png"), p_paired,
       width = 9, height = 5.5, dpi = 300)
cat("  ✓ paired_error_distributions.png\n")

# =============================================================================
# STEP 8: RESIDUAL MAP (SPATIAL PATTERNS)
# =============================================================================

cat("\nStep 8: Spatial residual patterns...\n\n")

# Get coordinates for test plots
if ("lon" %in% names(test_forest) && "lat" %in% names(test_forest)) {
  
  residual_spatial <- all_predictions %>%
    inner_join(test_forest %>% select(CN, lon, lat), by = "CN")
  
  p_resid_map <- ggplot(residual_spatial, aes(x = lon, y = lat, color = residual)) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_color_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0,
      limits = c(-quantile(abs(residual_spatial$residual), 0.95),
                 quantile(abs(residual_spatial$residual), 0.95)),
      oob = scales::squish,
      name = "Residual\n(Mg/ha)"
    ) +
    facet_grid(scale ~ scenario) +
    coord_quickmap() +
    labs(
      title = "Spatial Distribution of Prediction Residuals",
      subtitle = "Red = overprediction, Blue = underprediction",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 13)
    )
  
  ggsave(file.path(fig_dir, "residual_spatial_map.png"), p_resid_map,
         width = 12, height = 7, dpi = 300)
  cat("  ✓ residual_spatial_map.png\n")
}

# =============================================================================
# STEP 9: CV FOLD RESULTS (USE EXISTING)
# =============================================================================

cat("\nStep 9: Cross-validation fold-level analysis...\n\n")

fold_file <- "data/processed/phase4_cv_results/fold_results.csv"
if (file.exists(fold_file)) {
  
  fold_df <- read_csv(fold_file, show_col_types = FALSE)
  
  # Filter to RF only for consistency
  fold_rf <- fold_df %>% filter(model_type == "rf")
  
  if (nrow(fold_rf) > 0) {
    
    # Parse scale label
    fold_rf <- fold_rf %>%
      mutate(scale_label = ifelse(grepl("Fine|fine|10m", scale), "10m", "250m"))
    
    # CV RMSE distributions per scenario at each scale
    p_cv_folds <- ggplot(fold_rf, aes(x = scenario, y = rmse, fill = scenario)) +
      geom_boxplot(outlier.shape = 21) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      facet_wrap(~ scale_label) +
      scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                                   "NEFIN Only" = "#2ECC71", 
                                   "Pooled" = "#3498DB")) +
      labs(
        title = "Cross-Validation RMSE Across Folds",
        subtitle = "10 spatial folds per model — shows variance in performance",
        x = NULL, y = "Fold RMSE (Mg/ha)"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 13),
        legend.position = "none"
      )
    
    ggsave(file.path(fig_dir, "cv_fold_rmse_by_scenario.png"), p_cv_folds,
           width = 9, height = 5.5, dpi = 300)
    cat("  ✓ cv_fold_rmse_by_scenario.png\n")
    
    # Same for R²
    p_cv_r2 <- ggplot(fold_rf, aes(x = scenario, y = r2, fill = scenario)) +
      geom_boxplot(outlier.shape = 21) +
      geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
      facet_wrap(~ scale_label) +
      scale_fill_manual(values = c("FIA Only" = "#E74C3C", 
                                   "NEFIN Only" = "#2ECC71", 
                                   "Pooled" = "#3498DB")) +
      labs(
        title = "Cross-Validation R² Across Folds",
        subtitle = "Higher is better — precision should help more at 10m",
        x = NULL, y = "Fold R²"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 13),
        legend.position = "none"
      )
    
    ggsave(file.path(fig_dir, "cv_fold_r2_by_scenario.png"), p_cv_r2,
           width = 9, height = 5.5, dpi = 300)
    cat("  ✓ cv_fold_r2_by_scenario.png\n")
  }
} else {
  cat("  ⚠ fold_results.csv not found, skipping\n")
}

# =============================================================================
# STEP 10: SAVE SUMMARY
# =============================================================================

cat("\nStep 10: Saving summaries...\n\n")

# Save interaction summary
write_csv(interaction_summary, 
          "data/processed/phase4_cv_results/fuzzing_impact_summary.csv")
cat("  ✓ fuzzing_impact_summary.csv\n")

# Save statistical test results
if (nrow(stat_results) > 0) {
  write_csv(stat_results, 
            "data/processed/phase4_cv_results/fuzzing_significance_tests.csv")
  cat("  ✓ fuzzing_significance_tests.csv\n")
}

# Save plot-level predictions
write_csv(all_predictions,
          "data/processed/phase4_cv_results/test_predictions_all_models.csv")
cat("  ✓ test_predictions_all_models.csv\n")

# Save improvement summary
write_csv(improvement,
          "data/processed/phase4_cv_results/fuzzing_rmse_improvement.csv")
cat("  ✓ fuzzing_rmse_improvement.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  FUZZING IMPACT ANALYSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("KEY RESULTS:\n\n")

for (i in seq_len(nrow(improvement))) {
  s <- improvement$scale[i]
  cat(sprintf("  %s resolution:\n", s))
  cat(sprintf("    FIA (fuzzed):   RMSE = %.1f Mg/ha\n", improvement$`FIA Only`[i]))
  cat(sprintf("    NEFIN (precise): RMSE = %.1f Mg/ha  (%.1f%% %s)\n", 
              improvement$`NEFIN Only`[i], abs(improvement$nefin_pct[i]),
              ifelse(improvement$nefin_pct[i] > 0, "better", "worse")))
  cat(sprintf("    Pooled:         RMSE = %.1f Mg/ha  (%.1f%% %s)\n\n",
              improvement$Pooled[i], abs(improvement$pooled_pct[i]),
              ifelse(improvement$pooled_pct[i] > 0, "better", "worse")))
}

if (nrow(stat_results) > 0) {
  cat("STATISTICAL TESTS:\n\n")
  for (i in seq_len(nrow(stat_results))) {
    cat(sprintf("  %s %s: p = %.4f %s\n",
                stat_results$scale[i], stat_results$comparison[i],
                stat_results$p_value[i],
                ifelse(stat_results$significant[i], "★", "")))
  }
}

cat("\nFIGURES:\n")
figs <- list.files(fig_dir, pattern = "\\.png$")
for (f in figs) cat("  •", f, "\n")

cat("\nDATA:\n")
cat("  • data/processed/phase4_cv_results/fuzzing_impact_summary.csv\n")
cat("  • data/processed/phase4_cv_results/fuzzing_significance_tests.csv\n")
cat("  • data/processed/phase4_cv_results/fuzzing_rmse_improvement.csv\n")
cat("  • data/processed/phase4_cv_results/test_predictions_all_models.csv\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")
