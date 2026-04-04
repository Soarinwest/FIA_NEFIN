# =============================================================================
# fig_interpretability.R
# Four interpretability figures for Paper 2
#
# Figures produced:
#   fig_S1_variable_importance.png   -- RF permutation importance by scenario
#   fig_S2_residuals_vs_observed.png -- Residuals vs observed AGB
#   fig_S3_partial_dependence.png    -- Partial dependence: NDVI -> AGB
#   fig_S4_biomass_distributions.png -- Training / test biomass distributions
#
# Figures S1 and S3 require saved RF model .rds files (see PATHS section).
# Figures S2 and S4 use only the uploaded CSV outputs and run standalone.
#
# Run from project root: source("fig_interpretability.R")
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(readr)
library(ranger)   # for variable importance extraction
library(pdp)      # for partial dependence (install if needed: install.packages("pdp"))

# -- Colour palette (matches PHASE4_config.R) ---------------------------------
SCENARIO_COLORS <- c(
  "FIA Only"  = "#d62728",
  "Pooled" = "#2ca02c",
  "NEFIN Only" = "#1f77b4"
)
SCENARIO_ORDER <- c("FIA Only", "Pooled", "NEFIN Only")

# -- Shared theme -------------------------------------------------------------
theme_paper <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major= element_line(colour = "grey92", linewidth = 0.35),
      strip.background= element_rect(fill = "grey96", colour = "grey70"),
      strip.text= element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.key.size = unit(0.9, "lines"),
      legend.title= element_text(size = 9),
      legend.text = element_text(size = 9),
      axis.title= element_text(size = 10),
      plot.title  = element_text(size = 12, face = "bold", margin =),
      plot.subtitle = element_text(size = 9, colour = "grey40", margin =),
      plot.caption = element_text(size = 8, colour = "grey50", hjust = 0)
    )
}

# -- Output directory ---------------------------------------------------------
out_dir <- "manuscript_figures/interpretability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# Note: pre-existing importance figures are at:
#   manuscript_figures/phase4/diagnostics/importance_fine10m.png
#   manuscript_figures/phase4/diagnostics/importance_coarse250m.png
#   manuscript_figures/phase4/diagnostics/importance_comparison.png
# This script produces a publication-formatted version with scenario comparison.

# =============================================================================
# PATHS -- update these to match your saved model and data locations
# =============================================================================
PATHS <- list(
  # RF model objects -- saved by PHASE4_02_spatial_cv.R
  # Note: filenames contain parentheses as produced by the pipeline
  model_fia_10m    = "data/processed/phase4_models/rf_fine_scale_(10m)_fia_only.rds",
  model_nefin_10m  = "data/processed/phase4_models/rf_fine_scale_(10m)_nefin_only.rds",
  model_pooled_10m = "data/processed/phase4_models/rf_fine_scale_(10m)_pooled.rds",
  
  # CSV outputs (already uploaded / available)
  test_preds  = "data/processed/phase4_cv_results/test_predictions_all_models.csv",
  fia_plots   = "data/processed/fia_complete.csv",
  nefin_plots = "data/processed/nefin_complete.csv"
)


# =============================================================================

# =============================================================================
# FIGURE S1 -- Variable Importance
# Reads from data/processed/phase4_diagnostics/variable_importance.csv
# No model .rds files required.
# =============================================================================

cat("\n-- Figure S1: Variable importance ------------------------------\n")

IMP_CSV <- "data/processed/phase4_diagnostics/variable_importance.csv"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -- Colours matching PHASE4_config.R -----------------------------------------
SCENARIO_COLORS <- c(
  "FIA Only"   = "#d62728",
  "Pooled"     = "#2ca02c",
  "NEFIN Only" = "#1f77b4"
)
SCENARIO_ORDER <- c("FIA Only", "Pooled", "NEFIN Only")

# -- Load and clean ------------------------------------------------------------
imp_raw <- read_csv(IMP_CSV, show_col_types = FALSE)

# Fix malformed scenario labels produced by PHASE4_diagnostics.R
scenario_map <- c(
  "Scale (10m) Fia Only"   = "FIA Only",
  "Scale (10m) Nefin Only" = "NEFIN Only",
  "Scale (10m) Pooled"     = "Pooled"
)

# Human-readable predictor labels
var_map <- c(
  canopy_height_10m = "Canopy height",
  ndvi_s2_10m = "NDVI (S2)",
  evi_s2_10m= "EVI (S2)",
  nbr_s2_10m= "NBR (S2)",
  ndwi_s2_10m = "NDWI (S2)",
  red_s2_10m = "Red (B4)",
  nir_s2_10m = "NIR (B8)",
  green_s2_10m = "Green (B3)",
  blue_s2_10m = "Blue (B2)",
  elevation_10m = "Elevation",
  slope_10m = "Slope",
  aspect_10m = "Aspect",
  tmean_10m = "Mean temp.",
  tmin_10m = "Min. temp.",
  tmax_10m = "Max. temp.",
  ppt_10m = "Precipitation"
)

# Predictor groups for colour-coding the y-axis labels
group_map <- c(
  "Canopy height" = "Structure",
  "NDVI (S2)" = "Spectral",
  "EVI (S2)" = "Spectral",
  "NBR (S2)" = "Spectral",
  "NDWI (S2)" = "Spectral",
  "Red (B4)" = "Spectral",
  "NIR (B8)" = "Spectral",
  "Green (B3)" = "Spectral",
  "Blue (B2)" = "Spectral",
  "Elevation" = "Topographic",
  "Slope" = "Topographic",
  "Aspect" = "Topographic",
  "Mean temp." = "Climate",
  "Min. temp." = "Climate",
  "Max. temp." = "Climate",
  "Precipitation" = "Climate"
)

GROUP_COLORS <- c(
  "Structure"   = "#6A0572",
  "Spectral"    = "#1565C0",
  "Topographic" = "#2E7D32",
  "Climate"     = "#BF360C"
)

# -- Filter to 10m RF only and reshape -----------------------------------------
rf_10m <- imp_raw %>%
  filter(scale == "Fine (10m)", model_type == "Random Forest") %>%
  mutate(
    scenario  = recode(scenario, !!!scenario_map),
    var_label = recode(variable, !!!var_map),
    group     = recode(var_label, !!!group_map, .default = "Other")
  ) %>%
  filter(scenario %in% SCENARIO_ORDER) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_ORDER))

# Order predictors by mean importance across all three scenarios (ascending = bottom = least)
pred_order <- rf_10m %>%
  group_by(var_label) %>%
  summarise(mean_imp = mean(importance_norm), .groups = "drop") %>%
  arrange(mean_imp) %>%
  pull(var_label)

rf_10m <- rf_10m %>%
  mutate(var_label = factor(var_label, levels = pred_order),
         group= factor(group, levels = c("Structure","Spectral","Topographic","Climate")))

# -- Figure S1 -- dot plot ------------------------------------------------------
fig_S1 <- ggplot(rf_10m,
                 aes(x = importance_norm, y = var_label, colour = scenario)) +
  
  # Light vertical gridlines at 25 / 50 / 75 to give the eye reference
  geom_vline(xintercept = c(25, 50, 75),
             colour = "grey88", linewidth = 0.4, linetype = "solid") +
  
  # Points (dot plot without stems)
  geom_point(
    size= 2.8,
    alpha = 0.92,
    position = position_dodge(width = 0.7)
  ) +
  
  # Scenario colours
  scale_colour_manual(values = SCENARIO_COLORS,
                      name   = "Training scenario",
                      guide  = guide_legend(override.aes = list(size = 3))) +
  
  # x axis: 0-100 with % labels
  scale_x_continuous(
    name   = "Relative importance (% of maximum within scenario)",
    limits = c(-2, 105),
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0", "25", "50", "75", "100"),
    expand = c(0, 0)
  ) +
  
  # Predictor group colour strip on y-axis
  # (achieved via coloured axis text using a secondary colour scale)
  scale_y_discrete(name = NULL) +
  
  labs(
    title = "Fig. S1.  Variable importance -- fine-scale (10 m) RF models",
    subtitle = "IncNodePurity normalised 0-100 within each model. All scenarios use identical predictor sets."
    #,
   #caption  = paste0(
    #  "IncNodePurity. Rankings consistent across all three scenarios: canopy height dominates each. ",
      #"Performance differences reflect training data distribution, not predictor selection. ",
     # "Secondary shift: FIA weights precipitation (88%); NEFIN does not (25%), reflecting narrower environmental coverage."
    #)
  ) +
  
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70"),
    strip.background= element_rect(fill = "grey96", colour = "grey70"),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    legend.position = "bottom",
    legend.key.size = unit(0.9, "lines"),
    legend.title= element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold", margin =),
    plot.subtitle = element_text(size = 8.5, colour = "grey35",
                                      margin =, lineheight = 1.3),
    plot.caption = element_text(size = 8, colour = "grey45", hjust = 0,
                                      margin =, lineheight = 1.3)
  )

# -- Colour-code predictor group labels on y-axis ------------------------------
# Build a named vector of y-axis label colours by group membership
label_colours <- group_map[levels(rf_10m$var_label)]
label_colours <- GROUP_COLORS[label_colours]
names(label_colours) <- levels(rf_10m$var_label)

fig_S1 <- fig_S1 +
  theme(axis.text.y = element_text(size = 9, face = "bold"))

# -- Save ----------------------------------------------------------------------
out_path_s1 <- file.path(out_dir, "fig_S1_variable_importance.png")
ggsave(out_path_s1, fig_S1, width = 10.0, height = 5.8, dpi = 300)
cat(sprintf("  OK %s\n", out_path_s1))

# -- Bonus: coarse-scale (250m) version for supplement -------------------------
scenario_map_250 <- c(
  "Scale (250m) Fia Only" = "FIA Only",
  "Scale (250m) Nefin Only" = "NEFIN Only",
  "Scale (250m) Pooled" = "Pooled"
)

var_map_250 <- c(
  canopy_height_250m = "Canopy height",
  ndvi_modis_250m = "NDVI (MODIS)",
  evi_modis_250m= "EVI (MODIS)",
  nbr_modis_250m= "NBR (MODIS)",
  ndwi_modis_250m = "NDWI (MODIS)",
  red_modis_250m = "Red (MODIS B1)",
  nir_modis_250m = "NIR (MODIS B2)",
  blue_modis_250m = "Blue (MODIS)",
  green_modis_250m = "Green (MODIS)",
  swir1_modis_250m = "SWIR-1 (MODIS)",
  elevation_250m  = "Elevation",
  slope_250m = "Slope",
  aspect_250m = "Aspect",
  tmean_250m = "Mean temp.",
  tmin_250m = "Min. temp.",
  tmax_250m = "Max. temp.",
  ppt_250m = "Precipitation"
)

rf_250m <- imp_raw %>%
  filter(scale == "Coarse (250m)", model_type == "Random Forest") %>%
  mutate(
    scenario = recode(scenario, !!!scenario_map_250),
    var_label = recode(variable, !!!var_map_250),
    group = case_when(
      grepl("Canopy", var_label) ~ "Structure",
      grepl("temp|Precip", var_label) ~ "Climate",
      grepl("Elev|Slope|Aspect", var_label) ~ "Topographic",
      TRUE ~ "Spectral"
    )
  ) %>%
  filter(scenario %in% SCENARIO_ORDER) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_ORDER))

pred_order_250 <- rf_250m %>%
  group_by(var_label) %>%
  summarise(mean_imp = mean(importance_norm), .groups = "drop") %>%
  arrange(mean_imp) %>%
  pull(var_label)

rf_250m <- rf_250m %>%
  mutate(var_label = factor(var_label, levels = pred_order_250))

label_colours_250 <- case_when(
  levels(rf_250m$var_label) == "Canopy height" ~ GROUP_COLORS["Structure"],
  grepl("temp\\.|Precip", levels(rf_250m$var_label)) ~ GROUP_COLORS["Climate"],
  grepl("Elev|Slope|Aspect", levels(rf_250m$var_label)) ~ GROUP_COLORS["Topographic"],
  TRUE ~ GROUP_COLORS["Spectral"]
)

fig_S1b <- ggplot(rf_250m,
                  aes(x = importance_norm, y = var_label, colour = scenario)) +
  geom_vline(xintercept = c(25, 50, 75), colour = "grey88", linewidth = 0.4) +
  geom_point(size = 2.8, alpha = 0.92,
             position = position_dodge(width = 0.7)) +
  scale_colour_manual(values = SCENARIO_COLORS, name = "Training scenario",
                      guide = guide_legend(override.aes = list(size = 3))) +
  scale_x_continuous(name = "Relative importance (% of maximum within scenario)",
                     limits = c(-2, 105), breaks = c(0, 25, 50, 75, 100),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL) +
  labs(title    = "Fig. S1b.  Variable importance -- coarse-scale (250 m) RF models",
       subtitle = "Importance metric: IncNodePurity, normalised 0-100 within each model.",
       caption  = "Canopy height remains dominant at 250m. SWIR-1 (MODIS) is exclusive to the coarse-scale predictor set.") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey70"),
    axis.text.y      = element_text(size = 9, face = "bold"),
    legend.position  = "bottom",
    plot.title       = element_text(size = 12, face = "bold", margin =),
    plot.subtitle    = element_text(size = 8.5, colour = "grey35", margin =),
    plot.caption     = element_text(size = 8, colour = "grey45", hjust = 0, margin =)
  )

out_path_s1b <- file.path(out_dir, "fig_S1b_variable_importance_250m.png")
ggsave(out_path_s1b, fig_S1b, width = 10.0, height = 6.2, dpi = 300)
cat(sprintf("  OK %s\n", out_path_s1b))

# -- XGBoost vs RF importance comparison (Fig S1c) ----------------------------
# All three training scenarios, both algorithms -- dot plot faceted by scenario
# Key argument: predictor rankings are consistent regardless of training data
# source (FIA fuzzed vs NEFIN true GPS vs pooled) AND algorithm choice

scenario_map_10m <- c(
  "Scale (10m) Fia Only"   = "FIA Only",
  "Scale (10m) Nefin Only" = "NEFIN Only",
  "Scale (10m) Pooled"     = "Pooled"
)

both_algorithms <- imp_raw %>%
  filter(scale == "Fine (10m)",
         model_type %in% c("Random Forest", "XGBoost"),
         scenario %in% names(scenario_map_10m)) %>%
  mutate(
    var_label    = recode(variable, !!!var_map),
    scenario_lab = recode(scenario, !!!scenario_map_10m),
    model_label  = model_type,
    var_label    = factor(var_label, levels = pred_order),
    scenario_lab = factor(scenario_lab, levels = SCENARIO_ORDER),
    model_label  = factor(model_label, levels = c("Random Forest", "XGBoost"))
  )

# Spearman rho per scenario (for subtitle)
rho_vals <- both_algorithms %>%
  select(variable, scenario_lab, model_label, importance_norm) %>%
  pivot_wider(names_from = model_label, values_from = importance_norm) %>%
  group_by(scenario_lab) %>%
  summarise(
    rho = cor(`Random Forest`, XGBoost, method = "spearman"),
    .groups = "drop"
  )
rho_str <- paste(
  paste0(rho_vals$scenario_lab, ": rho=", round(rho_vals$rho, 2)),
  collapse = "  |  "
)

fig_S1c <- ggplot(both_algorithms,
                  aes(x = importance_norm, y = var_label,
                      colour = model_label, shape = model_label)) +
  geom_vline(xintercept = c(25, 50, 75), colour = "grey88", linewidth = 0.4) +
  geom_point(size = 2.8, alpha = 0.88,
             position = position_dodge(width = 0.6)) +
  facet_wrap(~ scenario_lab, ncol = 3) +
  scale_colour_manual(
    values = c("Random Forest" = "#2E7D32", "XGBoost" = "#E65100"),
    name   = "Algorithm",
    guide  = guide_legend(override.aes = list(size = 3))
  ) +
  scale_shape_manual(
    values = c("Random Forest" = 16, "XGBoost" = 17),
    name   = "Algorithm"
  ) +
  scale_x_continuous(
    name   = "Relative importance (% of maximum within model)",
    limits = c(-2, 105),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  scale_y_discrete(name = NULL) +
  labs(
    title = "Fig. S1c.  RF vs XGBoost importance -- fine-scale (10 m), all training scenarios",
    subtitle = paste0("Spearman rank correlation between algorithms  |  ", rho_str)
    #,
    #caption  = paste0(
      #"Canopy height (100%) and precipitation rank identically at the top for both algorithms ",
      #"across all three training scenarios, including NEFIN Only (true GPS coordinates). ",
     #"NEFIN Only shows the highest RF-XGBoost agreement (rho = 0.89) and perfect top-3 overlap. ",
     # "This consistency rules out both algorithm choice and coordinate precision ",
     # "as explanations for performance differences between training scenarios."
    #)
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70"),
    strip.background = element_rect(fill = "grey96", colour = "grey70"),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8.5),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 8.5, colour = "grey35"),
    plot.caption = element_text(size = 8, colour = "grey45", hjust = 0)
  )

out_path_s1c <- file.path(out_dir, "fig_S1c_rf_vs_xgb_importance.png")
ggsave(out_path_s1c, fig_S1c, width = 13.0, height = 5.5, dpi = 300)
cat(sprintf("  OK %s\n", out_path_s1c))


# FIGURE S2 -- Residuals vs Observed Biomass
# Runs standalone from test_predictions_all_models.csv
# =============================================================================

cat("\n-- Figure S2: Residuals vs observed ----------------------------\n")

test_preds <- read_csv(PATHS$test_preds, show_col_types = FALSE)

# Focus on 10m RF models (primary results) -- coarse scale shown as inset/supplement
preds_10m <- test_preds %>%
  filter(scale == "10m") %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_ORDER))

# Biomass quartile background shading data
quartile_breaks <- quantile(preds_10m$observed[preds_10m$scenario == "FIA Only"],
                            probs = c(0, 0.25, 0.5, 0.75, 1))

shade_df <- data.frame(
  xmin  = quartile_breaks[-5],
  xmax  = quartile_breaks[-1],
  label = c("Q1\n(Low)", "Q2", "Q3", "Q4\n(High)"),
  fill  = c("grey96", "white", "grey96", "white")
)

fig_S2 <- ggplot(preds_10m, aes(x = observed, y = residual)) +
  # Quartile shading
  geom_rect(data = shade_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.6) +
  scale_fill_identity() +
  # Quartile boundary lines
  geom_vline(xintercept = quartile_breaks[2:4],
             linetype = "dotted", colour = "grey60", linewidth = 0.4) +
  # Zero reference
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.55, linetype = "dashed") +
  # Points
  geom_point(aes(colour = scenario), size = 1.6, alpha = 0.55) +
  # LOESS smooths
  geom_smooth(aes(colour = scenario), method = "loess", se = TRUE,
              linewidth = 1.0, alpha = 0.15, span = 0.75) +
  # Quartile labels at top -- use geom_text with a data frame so facet
  # replication is handled correctly (annotate() + facet_wrap() mismatch)
  geom_text(
    data = data.frame(
      x     = (shade_df$xmin + shade_df$xmax) / 2,
      y     = Inf,
      label = shade_df$label
    ),
    aes(x = x, y = y, label = label),
    vjust = 1.4, size = 2.8, colour = "grey45",
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = SCENARIO_COLORS, name = "Training scenario") +
  scale_x_continuous(
    name   = "Observed AGB (Mg/ha)",
    breaks = seq(0, 600, 100)
  ) +
  scale_y_continuous(
    name   = "Residual -- predicted - observed (Mg/ha)",
    breaks = seq(-500, 300, 100),
    limits = c(-510, 340)
  ) +
  facet_wrap(~ scenario, ncol = 3) +
  labs(
    title = "Fig. S2. Model residuals vs observed biomass -- fine-scale (10 m) RF",
    subtitle= "Positive residuals = overestimation. Dashed line = zero bias. Loess smooth +/- 95% CI."
    #,
    #caption = paste0(
    #  "Test set n = 140 NEFIN plots (35 per biomass quartile). ",
     # "Near-linear negative residual trend reflects RF compression toward the training mean: ",
     # "FIA underestimates high-biomass stands (FIA training mean ~120 Mg/ha); ",
     # "NEFIN overestimates low-biomass stands (NEFIN training mean ~192 Mg/ha). ",
     # "This is training distribution bias, not model noise. Pooled model balances both biases."
    #)
  ) +
  theme_paper() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "fig_S2_residuals_vs_observed.png"),
  fig_S2, width = 11.5, height = 4.2, dpi = 300
)
cat("  OK fig_S2_residuals_vs_observed.png\n")

# Also produce the combined (both scales) version for supplement
preds_both <- test_preds %>%
  mutate(
    scenario = factor(scenario, levels = SCENARIO_ORDER),
    scale_label = ifelse(scale == "10m", "Fine scale (10 m)", "Coarse scale (250 m)")
  )

fig_S2b <- ggplot(preds_both, aes(x = observed, y = residual)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(colour = scenario), size = 1.3, alpha = 0.45) +
  geom_smooth(aes(colour = scenario), method = "loess", se = FALSE,
              linewidth = 0.9, span = 0.75) +
  scale_colour_manual(values = SCENARIO_COLORS, name = "Training scenario") +
  scale_x_continuous(name = "Observed AGB (Mg/ha)", breaks = seq(0, 600, 150)) +
  scale_y_continuous(name = "Residual (Mg/ha)", breaks = seq(-500, 300, 100)) +
  facet_grid(scale_label ~ scenario) +
  labs(
    title = "Fig. S2b. Model residuals vs observed biomass -- both scales",
    caption = "Loess smooth (no CI). Test set n = 140 per scale x scenario combination."
  ) +
  theme_paper() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "fig_S2b_residuals_both_scales.png"),
  fig_S2b, width = 9.5, height = 6.0, dpi = 300
)
cat("  OK fig_S2b_residuals_both_scales.png\n")


# =============================================================================
# LOAD RF MODELS (needed for S3 partial dependence)
# =============================================================================

# Helper: unwrap model from list if saved as a named list (common pipeline pattern)
unwrap_model <- function(obj) {
  if (is.list(obj) && !inherits(obj, c("randomForest", "xgb.Booster", "ranger"))) {
    for (slot in c("model", "final_model", "fit", "rf", "ranger_model")) {
      if (slot %in% names(obj)) return(obj[[slot]])
    }
    # Last resort: return first element
    return(obj[[1]])
  }
  obj
}

models_available <- all(file.exists(
  PATHS$model_fia_10m, PATHS$model_nefin_10m, PATHS$model_pooled_10m
))

if (models_available) {
  cat("  Loading RF models...\n")
  rf_fia <- unwrap_model(readRDS(PATHS$model_fia_10m))
  rf_nefin  <- unwrap_model(readRDS(PATHS$model_nefin_10m))
  rf_pooled <- unwrap_model(readRDS(PATHS$model_pooled_10m))
  cat(sprintf("  Model classes: FIA=%s  NEFIN=%s  Pooled=%s\n",
              class(rf_fia)[1], class(rf_nefin)[1], class(rf_pooled)[1]))
} else {
  cat("  Model .rds files not found -- skipping Fig S3.\n")
}

# =============================================================================
# FIGURE S3 -- Partial Dependence: NDVI -> Predicted AGB
# Requires saved RF model .rds files
# =============================================================================

cat("\n-- Figure S3: Partial dependence (NDVI) ------------------------\n")

if (models_available) {
  
  # The pdp package needs the covariate-joined training data frames used to fit each model.
  # These are produced by PHASE4_01_prep_data.R and contain both biomass and all covariates.
  # Expected paths -- adjust to match your output directory from PHASE4_01_prep_data.R:
  # Training CSVs are produced by PHASE4_01_prep_data.R
  # and live at data/processed/phase4_modeling/
  phase4_train_fia <- "data/processed/phase4_modeling/train_fia_only.csv"
  phase4_train_nefin  <- "data/processed/phase4_modeling/train_nefin_only.csv"
  phase4_train_pooled <- "data/processed/phase4_modeling/train_pooled.csv"
  
  if (!all(file.exists(phase4_train_fia, phase4_train_nefin, phase4_train_pooled))) {
    stop(paste0(
      "Training CSV files for Fig S3 not found at expected paths:\n",
      "  ", phase4_train_fia,    "\n",
      "  ", phase4_train_nefin,  "\n",
      "  ", phase4_train_pooled, "\n",
      "These are produced by PHASE4_01_prep_data.R and should already exist."
    ))
  }
  
  # Exact column names as they appear in the training CSVs (with _10m suffix)
  FINE_COVARIATES <- c(
    "ndvi_s2_10m", "evi_s2_10m", "nbr_s2_10m", "ndwi_s2_10m",
    "red_s2_10m", "green_s2_10m", "blue_s2_10m",
    "canopy_height_10m", "elevation_10m", "slope_10m", "aspect_10m",
    "tmean_10m", "tmin_10m", "tmax_10m", "ppt_10m"
  )
  # PDP predictor (must match a column in FINE_COVARIATES)
  PDP_VAR <- "ndvi_s2_10m"
  
  fia_covs<- read_csv(phase4_train_fia,    show_col_types = FALSE)
  
  # Check covariate columns exist -- detect suffix mismatches early
  missing_fia <- setdiff(FINE_COVARIATES, names(fia_covs))
  if (length(missing_fia) > 0) {
    cat("  Columns in train_fia_only.csv:\n")
    cat("   ", paste(names(fia_covs), collapse = ", "), "\n")
    stop(paste0(
      "Missing covariates in FIA training CSV: ",
      paste(missing_fia, collapse = ", "),
      "\nUpdate FINE_COVARIATES to match actual column names above."
    ))
  }
  nefin_covs<- read_csv(phase4_train_nefin,  show_col_types = FALSE)
  pooled_covs <- read_csv(phase4_train_pooled, show_col_types = FALSE)
  
  # Helper: compute partial dependence manually (faster than pdp::partial for ranger)
  # Returns data.frame with columns: x, yhat, scenario
  compute_pdp <- function(model, train_df, pred_var, scenario_label,
                          n_grid = 60, n_sample = 500) {
    set.seed(42)
    if (nrow(train_df) > n_sample) train_df <- train_df[sample(nrow(train_df), n_sample), ]
    
    grid_vals <- seq(
      quantile(train_df[[pred_var]], 0.02, na.rm = TRUE),
      quantile(train_df[[pred_var]], 0.98, na.rm = TRUE),
      length.out = n_grid
    )
    
    pdp_vals <- sapply(grid_vals, function(v) {
      tmp <- train_df[, FINE_COVARIATES, drop = FALSE]
      tmp[[pred_var]] <- v
      preds <- if (inherits(model, "randomForest")) {
        predict(model, newdata = tmp)
      } else if (inherits(model, "ranger")) {
        predict(model, data = tmp)$predictions
      } else {
        # fallback: try predict() generically
        as.numeric(predict(model, newdata = tmp))
      }
      mean(preds, na.rm = TRUE)
    })
    
    data.frame(x = grid_vals, yhat = pdp_vals, scenario = scenario_label)
  }
  
  cat("  Computing PDP for FIA model...\n")
  pdp_fia <- compute_pdp(rf_fia, fia_covs, PDP_VAR, "FIA Only")
  
  cat("  Computing PDP for NEFIN model...\n")
  pdp_nefin <- compute_pdp(rf_nefin, nefin_covs, PDP_VAR, "NEFIN Only")
  
  cat("  Computing PDP for Pooled model...\n")
  pdp_pooled <- compute_pdp(rf_pooled, pooled_covs, PDP_VAR, "Pooled")
  
  pdp_all <- bind_rows(pdp_fia, pdp_nefin, pdp_pooled) %>%
    mutate(scenario = factor(scenario, levels = SCENARIO_ORDER))
  
  # Rug data: observed NDVI in each training set
  rug_fia   <- data.frame(x = fia_covs[[PDP_VAR]],   scenario = "FIA Only")
  rug_nefin <- data.frame(x = nefin_covs[[PDP_VAR]], scenario = "NEFIN Only")
  rug_pooled <- bind_rows(rug_fia, rug_nefin) %>% mutate(scenario = "Pooled")
  rug_all <- bind_rows(rug_fia, rug_nefin, rug_pooled) %>%
    mutate(scenario = factor(scenario, levels = SCENARIO_ORDER))
  
  # Saturation reference line
  sat_line <- 0.75   # typical optical saturation NDVI (Mg/ha axis is y)
  # We annotate the AGB axis instead -- FIA model typically plateaus around 100-150 Mg/ha
  
  fig_S3 <- ggplot(pdp_all, aes(x = x, y = yhat, colour = scenario)) +
    # Saturation zone shading
    annotate("rect",
             xmin = sat_line, xmax = Inf,
             ymin = -Inf, ymax = Inf,
             fill = "gold", alpha = 0.20) +
    # Optical saturation label placed here but colour overridden via identity scale below
    # Rug marks for training data coverage
    geom_rug(data = rug_all,
             aes(x = x, colour = scenario),
             sides = "b", alpha = 0.25, linewidth = 0.3,
             inherit.aes = FALSE) +
    # PDP lines
    geom_line(linewidth = 1.2, alpha = 0.95) +
    scale_colour_manual(values = SCENARIO_COLORS, name = "Training scenario") +
    # Saturation zone label - outside scenario aesthetic, fixed colour
    geom_text(
      data = data.frame(x = sat_line + 0.005, y = Inf, label = "Optical saturation zone"),
      aes(x = x, y = y, label = label),
      vjust = 1.5, hjust = 0, size = 2.8,
      colour = "goldenrod4", fontface = "italic",
      inherit.aes = FALSE
    ) +
    scale_x_continuous(
      name = "NDVI (Sentinel-2, 10 m)",
      breaks = seq(0.4, 1.0, 0.1),
      limits = c(0.35, 1.0)
    ) +
    scale_y_continuous(
      name  = "Partial dependence -- predicted AGB (Mg/ha)",
      breaks = seq(0, 400, 50)
    ) +
    labs(
      title = "Fig. S3. Partial dependence: NDVI -> predicted AGB -- fine-scale (10 m) RF",
      subtitle= "Marginal effect of NDVI with all other predictors held at training-set means. Rug marks show training data NDVI coverage.",
      #caption = paste0(
        #"Shaded region: NDVI >= 0.75 (optical saturation zone). PDP from n = 500 subsampled training plots. ",
       # "FIA curve flattens at ~140 Mg/ha: absence of high-biomass training plots, not purely sensor limitation. ",
        #"NEFIN curve flat near training mean (~192 Mg/ha): PDP marginalises over all predictors, ",
      #  "reflecting NEFIN mean rather than NDVI-only signal. Pooled is a compromise."
      #)
    ) +
    theme_paper()
  
  ggsave(
    file.path(out_dir, "fig_S3_partial_dependence_ndvi.png"),
    fig_S3, width = 7.5, height = 5.0, dpi = 300
  )
  cat("  OK fig_S3_partial_dependence_ndvi.png\n")
  
} else {
  cat(" Model .rds files not found -- skipping Fig S3.\n")
  cat(" Ensure models are saved with:\n")
  cat(" saveRDS(rf_model, file = '<PATHS$model_fia_10m>')\n")
}


# =============================================================================
# FIGURE S4 -- Biomass Distribution Comparison
# Runs standalone from fia_complete.csv and nefin_complete.csv
# =============================================================================

cat("\n-- Figure S4: Biomass distributions ----------------------------\n")

fia_plots <- read_csv(PATHS$fia_plots,   show_col_types = FALSE)
nefin_plots <- read_csv(PATHS$nefin_plots, show_col_types = FALSE)

test_cns  <- unique(test_preds$CN)
nefin_train_bio <- nefin_plots %>%
  filter(!CN %in% test_cns) %>%
  mutate(group = "NEFIN training\n(n = 317)")
nefin_test_bio  <- nefin_plots %>%
  filter(CN %in% test_cns) %>%
  mutate(group = "NEFIN test set\n(n = 140)")
fia_bio <- fia_plots %>%
  mutate(group = paste0("FIA training\n(n = ", scales::comma(nrow(fia_plots)), ")"))

dist_df <- bind_rows(
  fia_bio %>% select(biomass, group),
  nefin_train_bio %>% select(biomass, group),
  nefin_test_bio  %>% select(biomass, group)
) %>%
  mutate(
    group = factor(group, levels = c(
      paste0("FIA training\n(n = ", scales::comma(nrow(fia_plots)), ")"),
      "NEFIN training\n(n = 317)",
      "NEFIN test set\n(n = 140)"
    )),
    dataset = case_when(
      grepl("FIA",   group) ~ "FIA Only",
      grepl("train", group) ~ "NEFIN Only",
      TRUE ~ "NEFIN Only"
    )
  )

# Summary stats for annotation
summary_stats <- dist_df %>%
  group_by(group) %>%
  summarise(
    med  = median(biomass),
    mn   = mean(biomass),
    p95  = quantile(biomass, 0.95),
    .groups = "drop"
  )

# Colours: FIA = red, NEFIN train = blue, NEFIN test = steel blue
group_colors <- c(
  setNames(
    "#d62728",
    paste0("FIA training\n(n = ", scales::comma(nrow(fia_plots)), ")")
  ),
  "NEFIN training\n(n = 317)" = "#1f77b4",
  "NEFIN test set\n(n = 140)" = "#08519c"
)

fig_S4 <- ggplot(dist_df, aes(x = biomass, colour = group, fill = group)) +
  # Optical saturation reference band (AGB  150-200 Mg/ha for optical indices)
  annotate("rect",
           xmin = 150, xmax = 220,
           ymin = 0, ymax = Inf,
           fill = "gold", alpha = 0.12) +
  annotate("text",
           x = 185, y = Inf, vjust = 1.5,
           label = "Optical\nsat. zone",
           size = 2.8, colour = "goldenrod4", fontface = "italic") +
  # Density curves
  geom_density(linewidth = 0.85, alpha = 0.18, adjust = 1.1) +
  # Median lines
  geom_vline(data = summary_stats,
             aes(xintercept = med, colour = group),
             linetype = "dashed", linewidth = 0.7) +
  # Median labels
  geom_text(data = summary_stats %>%
              mutate(col = unname(group_colors[as.character(group)])),
            aes(x = med, y = Inf, label = paste0("M = ", round(med), " Mg")),
            colour = summary_stats %>%
              mutate(col = unname(group_colors[as.character(group)])) %>%
              pull(col),
            vjust = -0.4, hjust = -0.05, size = 2.8,
            inherit.aes = FALSE, show.legend = FALSE) +
  scale_colour_manual(values = group_colors, name = NULL) +
  scale_fill_manual(values   = group_colors, name = NULL) +
  scale_x_continuous(
    name   = "Aboveground biomass (Mg/ha)",
    breaks = seq(0, 700, 100),
    limits = c(0, 730)
  ) +
  scale_y_continuous(name = "Density") +
  labs(
    title   = "Fig. S4. Biomass distributions: FIA training, NEFIN training, and NEFIN test set",
    subtitle= "Dashed verticals mark group medians. Shaded band = AGB range where optical saturation limits model performance."#,
    #caption = paste0(
     # "FIA n = ", scales::comma(nrow(fia_plots)), "; NEFIN train n = 317; NEFIN test n = 140. ",
    #  "FIA underrepresents high-biomass conditions; NEFIN is centered on them. ",
     # "Model error patterns (Fig. S2) align directly with these distributions: ",
     # "FIA models underestimate where FIA training data is sparse (high AGB); ",
      #"NEFIN models overestimate where NEFIN training data is sparse (low AGB)."
    #)
  ) +
  theme_paper()

ggsave(
  file.path(out_dir, "fig_S4_biomass_distributions.png"),
  fig_S4, width = 10.0, height = 4.5, dpi = 300
)
cat("  OK fig_S4_biomass_distributions.png\n")


# Bonus: panel version showing test set quartile structure overlaid on training density
fig_S4b <- ggplot() +
  # FIA density (full training)
  geom_density(data = fia_bio,
               aes(x = biomass),
               fill = "#d62728", colour = "#d62728",
               alpha = 0.15, linewidth = 0.7, adjust = 1.1) +
  # NEFIN training density
  geom_density(data = nefin_train_bio,
               aes(x = biomass),
               fill = "#1f77b4", colour = "#1f77b4",
               alpha = 0.25, linewidth = 0.7, adjust = 1.1) +
  # NEFIN test set as a rug + points on x-axis
  geom_rug(data = nefin_test_bio,
           aes(x = biomass),
           colour = "#08519c", alpha = 0.6, linewidth = 0.45, length = unit(0.04, "npc")) +
  # Quartile boundaries of test set
  geom_vline(xintercept = quantile(nefin_test_bio$biomass, c(0.25, 0.5, 0.75)),
             colour = "#08519c", linetype = "dashed", linewidth = 0.55) +
  # Pre-compute quantile boundaries outside annotate() for reliability
  geom_text(
    data = data.frame(
      x     = quantile(nefin_test_bio$biomass, c(0.25, 0.5, 0.75)),
      y     = Inf,
      label = c("Q1|Q2", "Q2|Q3", "Q3|Q4")
    ),
    aes(x = x, y = y, label = label),
    vjust = 1.4, hjust = -0.1, size = 2.8, colour = "#08519c",
    inherit.aes = FALSE
  ) +
  # Optical saturation
  annotate("rect", xmin = 150, xmax = 220, ymin = 0, ymax = Inf,
           fill = "gold", alpha = 0.25) +
  scale_x_continuous(
    name   = "Aboveground biomass (Mg/ha)",
    breaks = seq(0, 700, 100), limits = c(0, 730)
  ) +
  scale_y_continuous(name = "Density") +
  annotate("text", x = 700, y = Inf, vjust = 2.5, hjust = 1,
           label = "Red = FIA training   Blue = NEFIN training   Rug = NEFIN test set",
           size = 2.8, colour = "grey40") +
  labs(
    title   = "Fig. S4b. Training distributions and test set quartile boundaries",
    caption = "Dashed verticals are test-set biomass quartile boundaries (Q1/Q2, Q2/Q3, Q3/Q4)."
  ) +
  theme_paper() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "fig_S4b_distributions_with_test_quartiles.png"),
  fig_S4b, width = 10.0, height = 4.0, dpi = 300
)
cat("  OK fig_S4b_distributions_with_test_quartiles.png\n")


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n==============================================================\n")
cat("  Figures saved to:", out_dir, "\n\n")
cat("  Always available (no model objects required):\n")
cat("    fig_S2_residuals_vs_observed.png\n")
cat("    fig_S2b_residuals_both_scales.png\n")
cat("    fig_S4_biomass_distributions.png\n")
cat("    fig_S4b_distributions_with_test_quartiles.png\n\n")
cat("  Requires RF model .rds files:\n")
cat("    fig_S1_variable_importance.png\n")
cat("    fig_S3_partial_dependence_ndvi.png\n\n")
cat("  Model paths expected:\n")
cat("    ", PATHS$model_fia_10m, "\n")
cat("    ", PATHS$model_nefin_10m, "\n")
cat("    ", PATHS$model_pooled_10m, "\n")
cat("\n  If models were not saved during PHASE4_02_spatial_cv.R,\n")
cat("  re-run the final model fits with:\n")
cat("    rf_model <- ranger(biomass ~ ., data = train_df,\n")
cat("                       num.trees = 500, min.node.size = 5,\n")
cat("                       importance = 'permutation')\n")
cat("    saveRDS(rf_model, PATHS$model_fia_10m)\n")
cat("==============================================================\n")