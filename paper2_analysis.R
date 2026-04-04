# =============================================================================
# paper2_analysis.R
# =============================================================================
# Journal manuscript: Does FIA coordinate fuzzing limit biomass model
# performance, and can local networks with true locations recover it?
#
# Generates all key results tables and figures for Paper 2.
# Run from project root: source("paper2_analysis.R")
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

out_dir <- "outputs/paper2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  PAPER 2 ANALYSIS: FIA Fuzzing vs NEFIN True Locations\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cv       <- read_csv("data/processed/phase4_cv_results/cv_summary.csv",
                     show_col_types = FALSE)
fuzz_imp <- read_csv("data/processed/phase4_cv_results/fuzzing_impact_summary.csv",
                     show_col_types = FALSE)
fuzz_pct <- read_csv("data/processed/phase4_cv_results/fuzzing_rmse_improvement.csv",
                     show_col_types = FALSE)
fuzz_sig <- read_csv("data/processed/phase4_cv_results/fuzzing_significance_tests.csv",
                     show_col_types = FALSE)
test_all <- read_csv("data/processed/phase4_cv_results/test_predictions_all_models.csv",
                     show_col_types = FALSE)
fold_res <- read_csv("data/processed/phase4_cv_results/fold_results.csv",
                     show_col_types = FALSE)

cat("Data loaded.\n\n")

# =============================================================================
# TABLE 1: CROSS-VALIDATION PERFORMANCE BY SCENARIO × SCALE
# Headline table for Paper 2 results section
# =============================================================================

cat("── Table 1: CV Performance ─────────────────────────────────\n")

# RF models only (cleaner than XGBoost across folds)
table1 <- cv %>%
  filter(model_type == "rf") %>%
  select(scale, scenario, n_train, n_test,
         cv_rmse_mean, cv_rmse_sd, cv_r2_mean, cv_r2_sd,
         test_rmse, test_r2, test_mae) %>%
  arrange(scale, scenario) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(table1)
write_csv(table1, file.path(out_dir, "table1_cv_performance.csv"))
cat("   Saved table1_cv_performance.csv\n\n")

# =============================================================================
# TABLE 2: RMSE IMPROVEMENT AND SIGNIFICANCE
# The core "does NEFIN help" result
# =============================================================================

cat("── Table 2: RMSE Improvement & Significance ────────────────\n")

table2 <- fuzz_pct %>%
  left_join(
    fuzz_sig %>%
      select(scale, comparison, p_value, significant) %>%
      mutate(scale = ifelse(scale == "10m", "Fine Scale (10m)", "Coarse Scale (250m)")),
    by = c("scale" = "scale")
  ) %>%
  mutate(
    scale_label  = ifelse(scale == "10m", "Fine (10m)", "Coarse (250m)"),
    nefin_pct = round(`nefin_pct`, 1),
    pooled_pct = round(`pooled_pct`, 1)
  )

# Clean table for manuscript
t2_clean <- data.frame(
  Scale  = c("Fine (10m)", "Coarse (250m)"),
  FIA_RMSE = c(fuzz_imp$rmse[fuzz_imp$scale=="10m"  & fuzz_imp$scenario=="FIA Only"],
                          fuzz_imp$rmse[fuzz_imp$scale=="250m" & fuzz_imp$scenario=="FIA Only"]),
  NEFIN_RMSE = c(fuzz_imp$rmse[fuzz_imp$scale=="10m"  & fuzz_imp$scenario=="NEFIN Only"],
                          fuzz_imp$rmse[fuzz_imp$scale=="250m" & fuzz_imp$scenario=="NEFIN Only"]),
  Pooled_RMSE = c(fuzz_imp$rmse[fuzz_imp$scale=="10m"  & fuzz_imp$scenario=="Pooled"],
                          fuzz_imp$rmse[fuzz_imp$scale=="250m" & fuzz_imp$scenario=="Pooled"]),
  NEFIN_improvement = round(c(fuzz_pct$nefin_pct[fuzz_pct$scale=="10m"],
                                fuzz_pct$nefin_pct[fuzz_pct$scale=="250m"]), 1),
  Pooled_improvement = round(c(fuzz_pct$pooled_pct[fuzz_pct$scale=="10m"],
                                fuzz_pct$pooled_pct[fuzz_pct$scale=="250m"]), 1),
  p_FIA_vs_NEFIN  = c(fuzz_sig$p_value[fuzz_sig$scale=="10m"  & fuzz_sig$comparison=="FIA vs NEFIN"],
                           fuzz_sig$p_value[fuzz_sig$scale=="250m" & fuzz_sig$comparison=="FIA vs NEFIN"]),
  p_FIA_vs_Pooled = c(fuzz_sig$p_value[fuzz_sig$scale=="10m"  & fuzz_sig$comparison=="FIA vs Pooled"],
                           fuzz_sig$p_value[fuzz_sig$scale=="250m" & fuzz_sig$comparison=="FIA vs Pooled"])
) %>%
  mutate(across(c(FIA_RMSE, NEFIN_RMSE, Pooled_RMSE), ~ round(.x, 1)),
         across(c(p_FIA_vs_NEFIN, p_FIA_vs_Pooled), ~ signif(.x, 3)))

print(t2_clean)
write_csv(t2_clean, file.path(out_dir, "table2_rmse_improvement.csv"))
cat("   Saved table2_rmse_improvement.csv\n\n")

# =============================================================================
# TABLE 3: PERFORMANCE BY BIOMASS CLASS × SCENARIO × SCALE
# The key structural-representativeness finding
# =============================================================================

cat("── Table 3: Performance by Biomass Class ───────────────────\n")

# Restrict to RF models (test set, not fold CV)
rf_test <- test_all  # test_predictions_all_models only has RF

table3 <- rf_test %>%
  group_by(scale, scenario, biomass_class) %>%
  summarise(
    n = n(),
    mean_obs = round(mean(observed), 1),
    MAE = round(mean(abs_error), 1),
    bias = round(mean(residual), 1),
    .groups  = "drop"
  ) %>%
  # Add FIA MAE for computing improvement
  group_by(scale, biomass_class) %>%
  mutate(
    FIA_MAE = MAE[scenario == "FIA Only"],
    pct_improvement = round(100 * (FIA_MAE - MAE) / FIA_MAE, 1)
  ) %>%
  ungroup() %>%
  arrange(scale, biomass_class, scenario)

print(table3)
write_csv(table3, file.path(out_dir, "table3_by_biomass_class.csv"))
cat("  Saved table3_by_biomass_class.csv\n\n")

# Paired t-test: FIA vs NEFIN, stratified by biomass half
cat("  Paired t-tests (low vs high biomass):\n")
for (sc in c("10m", "250m")) {
  fia_err  <- rf_test %>% filter(scale == sc, scenario == "FIA Only")  %>% arrange(CN)
  nef_err  <- rf_test %>% filter(scale == sc, scenario == "NEFIN Only") %>% arrange(CN)

  low_cn  <- fia_err %>% filter(biomass_class %in% c("Q1 (Low)", "Q2")) %>% pull(CN)
  high_cn <- fia_err %>% filter(biomass_class %in% c("Q3", "Q4 (High)")) %>% pull(CN)

  t_low  <- t.test(fia_err$abs_error[fia_err$CN %in% low_cn],
                   nef_err$abs_error[nef_err$CN %in% low_cn],
                   paired = TRUE)
  t_high <- t.test(fia_err$abs_error[fia_err$CN %in% high_cn],
                   nef_err$abs_error[nef_err$CN %in% high_cn],
                   paired = TRUE)

  cat(sprintf("  Scale %s | Low biomass:  t=%.2f, p=%.4f  → FIA %s\n",
              sc, t_low$statistic, t_low$p.value,
              ifelse(t_low$statistic < 0, "BETTER", "WORSE")))
  cat(sprintf("          | High biomass: t=%.2f, p=%.4f  → FIA %s\n",
              t_high$statistic, t_high$p.value,
              ifelse(t_high$statistic < 0, "BETTER", "WORSE")))
}
cat("\n")

# =============================================================================
# TABLE 4: TERRAIN ANALYSIS
# =============================================================================

cat("── Table 4: Performance by Terrain ─────────────────────────\n")

table4 <- rf_test %>%
  group_by(scale, scenario, terrain_class) %>%
  summarise(MAE = round(mean(abs_error), 1), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = MAE) %>%
  mutate(
    NEFIN_improvement_pct = round(100 * (`FIA Only` - `NEFIN Only`) / `FIA Only`, 1),
    Pooled_improvement_pct = round(100 * (`FIA Only` - Pooled) / `FIA Only`, 1)
  ) %>%
  arrange(scale, terrain_class)

print(table4)
write_csv(table4, file.path(out_dir, "table4_by_terrain.csv"))
cat("   Saved table4_by_terrain.csv\n\n")

# =============================================================================
# FIGURE 1: MAE by Biomass Class × Scenario (main finding, both scales)
# =============================================================================

cat("── Figure 1: MAE by Biomass Class ──────────────────────────\n")

fig1_data <- rf_test %>%
  group_by(scale, scenario, biomass_class) %>%
  summarise(MAE = mean(abs_error), se = sd(abs_error)/sqrt(n()), .groups = "drop") %>%
  mutate(
    scenario = factor(scenario, levels = c("FIA Only", "Pooled", "NEFIN Only")),
    scale_label = ifelse(scale == "10m", "Fine scale (10m)", "Coarse scale (250m)")
  )

fig1 <- ggplot(fig1_data, aes(x = biomass_class, y = MAE, fill = scenario)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = MAE - se, ymax = MAE + se),
                position = position_dodge(0.8), width = 0.25, linewidth = 0.5) +
  facet_wrap(~ scale_label) +
  scale_fill_manual(
    values = c("FIA Only"    = "#E57373",
               "Pooled"      = "#81C784",
               "NEFIN Only"  = "#64B5F6"),
    name = "Training scenario"
  ) +
  labs(
    title = "Model error by biomass class and training scenario",
    subtitle = "NEFIN advantage is concentrated in high-biomass stands; FIA is better for low biomass",
    x = "Biomass quartile (test set)",
    y = "Mean absolute error (Mg/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.subtitle = element_text(color = "gray40", size = 10)
  )

ggsave(file.path(out_dir, "fig1_mae_by_biomass_class.png"),
       fig1, width = 10, height = 5, dpi = 300)
cat(" Saved fig1_mae_by_biomass_class.png\n\n")

# =============================================================================
# FIGURE 2: Fold-level R² distributions (uncertainty in model performance)
# =============================================================================

cat("── Figure 2: CV Fold R² Distributions ──────────────────────\n")

fold_rf <- fold_res %>%
  filter(model_type == "rf") %>%
  mutate(
    scenario = factor(scenario, levels = c("FIA Only", "Pooled", "NEFIN Only")),
    scale_label = ifelse(scale == "Fine Scale (10m)", "Fine scale (10m)", "Coarse scale (250m)")
  )

fig2 <- ggplot(fold_rf, aes(x = scenario, y = r2, fill = scenario)) +
  geom_boxplot(width = 0.5, outlier.shape = 21, outlier.size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  facet_wrap(~ scale_label) +
  scale_fill_manual(
    values = c("FIA Only"= "#E57373",
               "Pooled" = "#81C784",
               "NEFIN Only" = "#64B5F6")
  ) +
  labs(
    title = "Cross-validation R² across 10 spatial folds",
    subtitle = "NEFIN-only shows higher mean R² but greater fold-to-fold variance",
    x = NULL,
    y = expression(R^2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text  = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "gray40", size = 10)
  )

ggsave(file.path(out_dir, "fig2_fold_r2_distributions.png"),
       fig2, width = 9, height = 5, dpi = 300)
cat("   Saved fig2_fold_r2_distributions.png\n\n")

# =============================================================================
# FIGURE 3: Observed vs Predicted (all scenarios, both scales)
# =============================================================================

cat("── Figure 3: Observed vs Predicted ─────────────────────────\n")

fig3_data <- rf_test %>%
  mutate(
    scenario = factor(scenario, levels = c("FIA Only", "Pooled", "NEFIN Only")),
    scale_label = ifelse(scale == "10m", "Fine scale (10m)", "Coarse scale (250m)")
  )

fig3 <- ggplot(fig3_data, aes(x = observed, y = predicted, color = scenario)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  facet_grid(scale_label ~ scenario) +
  scale_color_manual(
    values = c("FIA Only" = "#E57373",
               "Pooled" = "#4CAF50",
               "NEFIN Only" = "#2196F3")
  ) +
  labs(
    title = "Observed vs predicted aboveground biomass",
    subtitle = "Dashed line = 1:1. FIA consistently underestimates high-biomass stands.",
    x = "Observed biomass (Mg/ha)",
    y = "Predicted biomass (Mg/ha)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(color = "gray40", size = 9)
  )

ggsave(file.path(out_dir, "fig3_obs_vs_pred.png"),
       fig3, width = 10, height = 6, dpi = 300)
cat("   Saved fig3_obs_vs_pred.png\n\n")

# =============================================================================
# KEY NARRATIVE NUMBERS (paste into methods/results)
# =============================================================================

cat("══════════════════════════════════════════════════════════════\n")
cat("  KEY NUMBERS FOR MANUSCRIPT\n")
cat("══════════════════════════════════════════════════════════════\n\n")

fia_bias  <- mean(rf_test$residual[rf_test$scenario == "FIA Only"])
nef_bias  <- mean(rf_test$residual[rf_test$scenario == "NEFIN Only"])
test_mean <- mean(rf_test$observed[rf_test$scenario == "FIA Only"])

cat(sprintf("Test set mean biomass: %.1f Mg/ha\n", test_mean))
cat(sprintf("FIA systematic bias: %.1f Mg/ha (underestimate)\n", fia_bias))
cat(sprintf("NEFIN systematic bias: %.1f Mg/ha\n", nef_bias))
cat("\nRMSE improvement (NEFIN vs FIA):\n")
cat(sprintf("  Fine scale (10m):         %.1f%%  (p = %.4f)\n",
            fuzz_pct$nefin_pct[fuzz_pct$scale == "10m"],
            fuzz_sig$p_value[fuzz_sig$scale == "10m" & fuzz_sig$comparison == "FIA vs NEFIN"]))
cat(sprintf("  Coarse scale (250m):      %.1f%%  (p = %.4f)\n",
            fuzz_pct$nefin_pct[fuzz_pct$scale == "250m"],
            fuzz_sig$p_value[fuzz_sig$scale == "250m" & fuzz_sig$comparison == "FIA vs NEFIN"]))
cat("\nBiomass-class breakdown (pooled across scales):\n")
for (bc in c("Q1 (Low)", "Q2", "Q3", "Q4 (High)")) {
  fia_m <- rf_test %>% filter(scenario=="FIA Only", biomass_class==bc) %>% pull(abs_error) %>% mean()
  nef_m <- rf_test %>% filter(scenario=="NEFIN Only", biomass_class==bc) %>% pull(abs_error) %>% mean()
  obs_m <- rf_test %>% filter(scenario=="FIA Only", biomass_class==bc) %>% pull(observed)  %>% mean()
  cat(sprintf("  %-12s (mean obs=%.0f): FIA MAE=%.1f, NEFIN MAE=%.1f  → %s\n",
              bc, obs_m, fia_m, nef_m,
              ifelse(fia_m < nef_m,
                     sprintf("FIA better (+%.0f%%)", 100*(nef_m-fia_m)/fia_m),
                     sprintf("NEFIN better (+%.0f%%)", 100*(fia_m-nef_m)/fia_m))))
}

cat("\n Paper 2 analysis complete. Outputs in:", out_dir, "\n\n")