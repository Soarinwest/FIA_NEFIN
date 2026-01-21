# =============================================================================
# Phase 2: Within-Hex Variance Analysis
# =============================================================================
# Question: Do hexes with more NEFIN plots have lower variance?
# This tests if precision helps even in aggregated data
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)
library(ggplot2)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 2: WITHIN-HEX VARIANCE ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD HEX-AGGREGATED DATA
# =============================================================================

cat("Loading hex-aggregated data...\n\n")

results_by_scale <- list()

for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  cat("Processing", scale_name, "...\n")
  
  # Load augmented hex aggregations (has NEFIN breakdown)
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  # Create NEFIN proportion groups
  hex_analysis <- augmented_hex %>%
    mutate(
      nefin_pct_group = case_when(
        pct_nefin == 0 ~ "0% (FIA only)",
        pct_nefin > 0 & pct_nefin <= 5 ~ "0-5%",
        pct_nefin > 5 & pct_nefin <= 10 ~ "5-10%",
        pct_nefin > 10 & pct_nefin <= 20 ~ "10-20%",
        pct_nefin > 20 & pct_nefin <= 50 ~ "20-50%",
        pct_nefin > 50 ~ ">50%",
        TRUE ~ "Other"
      ),
      nefin_pct_group = factor(nefin_pct_group,
                              levels = c("0% (FIA only)", "0-5%", "5-10%", 
                                       "10-20%", "20-50%", ">50%"))
    ) %>%
    filter(!is.na(nefin_pct_group))
  
  # Summarize by group
  group_summary <- hex_analysis %>%
    group_by(nefin_pct_group) %>%
    summarise(
      n_hexes = n(),
      mean_biomass_sd = mean(biomass_sd, na.rm = TRUE),
      mean_biomass_se = mean(biomass_se, na.rm = TRUE),
      median_biomass_sd = median(biomass_sd, na.rm = TRUE),
      mean_ndvi_sd = sd(ndvi_s2_mean, na.rm = TRUE),
      mean_plots = mean(n_plots, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scale = scale_name)
  
  results_by_scale[[scale_name]] <- group_summary
  
  cat("  Hexes analyzed:", nrow(hex_analysis), "\n")
  cat("  Groups with NEFIN:", sum(hex_analysis$pct_nefin > 0), "\n\n")
}

# Combine all scales
all_results <- bind_rows(results_by_scale)

# =============================================================================
# STATISTICAL TEST
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  STATISTICAL TESTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Test: Does biomass SD decrease with NEFIN %?
for (scale in CONFIG$hex_scales) {
  scale_name <- scale$name
  
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale_name, ".csv")),
    show_col_types = FALSE
  )
  
  # Filter hexes with enough plots
  test_data <- augmented_hex %>%
    filter(n_plots >= 3, !is.na(biomass_sd), !is.na(pct_nefin))
  
  if (nrow(test_data) > 30) {
    # Linear model: biomass_sd ~ pct_nefin
    model <- lm(biomass_sd ~ pct_nefin, data = test_data)
    
    cat(scale_name, ":\n")
    cat("  N hexes:", nrow(test_data), "\n")
    cat("  Coefficient (pct_nefin):", sprintf("%.4f", coef(model)[2]), "\n")
    cat("  P-value:", sprintf("%.4f", summary(model)$coefficients[2, 4]), "\n")
    
    if (summary(model)$coefficients[2, 4] < 0.05) {
      if (coef(model)[2] < 0) {
        cat("  → NEFIN reduces within-hex variance ✓\n\n")
      } else {
        cat("  → NEFIN increases within-hex variance ✗\n\n")
      }
    } else {
      cat("  → No significant effect\n\n")
    }
  }
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_dir <- "data/processed/within_hex_analysis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(all_results, file.path(output_dir, "variance_by_nefin_percentage.csv"))

cat("✓ Saved results:", file.path(output_dir, "variance_by_nefin_percentage.csv"), "\n\n")

# =============================================================================
# VISUALIZATION
# =============================================================================

cat("Creating visualizations...\n")

# Plot 1: Biomass SD vs NEFIN %
p1 <- all_results %>%
  ggplot(aes(x = nefin_pct_group, y = mean_biomass_sd, 
             fill = scale, group = scale)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Within-Hex Biomass Variability by NEFIN Percentage",
    x = "NEFIN Percentage in Hex",
    y = "Mean Biomass SD (Mg/ha)",
    fill = "Hex Scale"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "biomass_sd_by_nefin_pct.png"),
       p1, width = 10, height = 6, dpi = 300)

# Plot 2: Trend line for each scale
p2_data <- bind_rows(lapply(CONFIG$hex_scales, function(scale) {
  augmented_hex <- read_csv(
    file.path("data/processed/hex_aggregated",
              paste0("augmented_hex_", scale$name, ".csv")),
    show_col_types = FALSE
  )
  
  augmented_hex %>%
    filter(n_plots >= 3, !is.na(biomass_sd)) %>%
    mutate(scale = scale$name)
}))

p2 <- p2_data %>%
  ggplot(aes(x = pct_nefin, y = biomass_sd, color = scale)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~scale, scales = "free_y") +
  labs(
    title = "Biomass SD vs NEFIN Percentage by Scale",
    x = "% NEFIN in Hex",
    y = "Biomass SD (Mg/ha)"
  ) +
  theme_minimal()

ggsave(file.path(output_dir, "biomass_sd_trend_by_scale.png"),
       p2, width = 12, height = 8, dpi = 300)

cat("✓ Saved visualizations\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Find strongest effect
best_reduction <- all_results %>%
  filter(nefin_pct_group == "0% (FIA only)") %>%
  select(scale, fia_only_sd = mean_biomass_sd)

nefin_effect <- all_results %>%
  filter(nefin_pct_group == ">50%") %>%
  select(scale, nefin_high_sd = mean_biomass_sd) %>%
  left_join(best_reduction, by = "scale") %>%
  mutate(
    reduction_pct = 100 * (fia_only_sd - nefin_high_sd) / fia_only_sd
  ) %>%
  arrange(desc(reduction_pct))

if (nrow(nefin_effect) > 0) {
  cat("Within-hex variance reduction (FIA-only vs >50% NEFIN):\n\n")
  print(nefin_effect)
  cat("\n")
  
  if (any(nefin_effect$reduction_pct > 5)) {
    cat("✓ NEFIN demonstrates within-hex variance reduction\n")
    cat("  Effect strongest at:", nefin_effect$scale[1], "\n")
  } else {
    cat("→ Minimal within-hex variance reduction from NEFIN\n")
    cat("  Compositional differences dominate\n")
  }
} else {
  cat("Not enough hexes with >50% NEFIN for comparison\n")
}

cat("\n")
cat("Next: Large tree analysis\n")
cat("  Rscript R/06_analysis/09_large_tree_analysis.R\n\n")
