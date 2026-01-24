# =============================================================================
# PHASE 4 - Spatial Validation Plots (AUC & Geospatial Diagnostics)
# =============================================================================
# Creates comprehensive spatial validation figures:
# 1. ROC Curves (AUC)
# 2. Precision-Recall Curves  
# 3. Spatial CV Performance (boxplots by fold)
# 4. Threshold-dependent Probability Maps
# 5. Cumulative Distribution Functions
# 6. Residual Spatial Patterns
# =============================================================================

source("R/00_config/PHASE4_config.R")

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(pROC)
library(PRROC)
library(sf)
library(viridis)
library(rnaturalearth)  # For base maps

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: SPATIAL VALIDATION PLOTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Create output directories
dir.create(paste0(PHASE4_CONFIG$output$dir_figures, "/spatial_cv"), 
           showWarnings = FALSE, recursive = TRUE)
fig_dir <- paste0(PHASE4_CONFIG$output$dir_figures, "/spatial_cv")

# =============================================================================
# LOAD CV RESULTS
# =============================================================================

cat("Step 1: Loading CV results...\n")

# Get all CV result files
cv_files <- list.files(PHASE4_CONFIG$output$dir_cv,
                       pattern = "cv_.*\\.csv",
                       full.names = TRUE)

# Load summary
cv_summary <- read_csv(file.path(PHASE4_CONFIG$output$dir_cv, "cv_summary.csv"),
                       show_col_types = FALSE)

cat("  ✓ Loaded", nrow(cv_summary), "model results\n\n")

# Load all fold predictions
pred_files <- list.files(file.path(PHASE4_CONFIG$output$dir_cv, "fold_predictions"),
                         pattern = "predictions_.*\\.csv",
                         full.names = TRUE)

all_predictions <- lapply(pred_files, function(f) {
  model_id <- gsub("predictions_|\\.csv", "", basename(f))
  df <- read_csv(f, show_col_types = FALSE) %>%
    mutate(
      model_id = model_id,
      CN = as.character(CN)  # Force CN to character for consistency
    )
  
  # Handle classification columns if they exist
  if ("biomass_class" %in% names(df)) {
    df <- df %>%
      mutate(biomass_class = factor(biomass_class, levels = c("Low", "High")))
  }
  
  return(df)
}) %>% bind_rows()

cat("  ✓ Loaded", nrow(all_predictions), "fold predictions\n\n")

# =============================================================================
# FIGURE 1: ROC CURVES (All models)
# =============================================================================

if (PHASE4_CONFIG$classification$enabled) {
  
  cat("Step 2: Creating ROC curves...\n")
  
  # Calculate ROC for each model
  roc_data_list <- list()
  
  for (model_id in unique(all_predictions$model_id)) {
    
    model_preds <- all_predictions %>% filter(model_id == !!model_id)
    
    # Calculate ROC
    roc_obj <- roc(model_preds$biomass_class, model_preds$predicted_prob,
                   levels = c("Low", "High"), direction = "<")
    
    # Extract coordinates
    roc_coords <- coords(roc_obj, "all", ret = c("threshold", "specificity", "sensitivity"))
    
    # Add model info
    roc_coords$model_id <- model_id
    roc_coords$auc <- as.numeric(auc(roc_obj))
    
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    roc_coords$scale <- parts[1]
    roc_coords$scenario <- paste(parts[-1], collapse = "_")
    
    roc_data_list[[model_id]] <- roc_coords
  }
  
  roc_data <- bind_rows(roc_data_list)
  
  # Plot ROC curves by scale
  for (scale_name in unique(roc_data$scale)) {
    
    scale_data <- roc_data %>% filter(scale == scale_name)
    
    # Prepare text labels with positions
    text_data <- scale_data %>% 
      group_by(model_id, scenario) %>% 
      slice(1) %>%
      ungroup() %>%
      arrange(scenario) %>%
      mutate(
        text_y = seq(0.3, 0.1, length.out = n()),
        text_label = sprintf("%s: AUC = %.3f", scenario, auc)
      )
    
    # Create plot
    p_roc <- ggplot(scale_data, 
                    aes(x = 1 - specificity, y = sensitivity, 
                        color = scenario, group = model_id)) +
      geom_line(linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
                  color = "gray50") +
      geom_text(data = text_data,
                aes(x = 0.6, y = text_y, label = text_label),
                hjust = 0, size = 3.5, fontface = "bold") +
      scale_color_manual(values = c(
        "fia_only" = "#d62728",
        "nefin_only" = "#1f77b4", 
        "pooled" = "#2ca02c"
      )) +
      labs(
        title = paste("ROC Curves:", 
                      ifelse(scale_name == "fine", "Fine Scale (10m)", 
                             "Coarse Scale (250m)")),
        subtitle = "Classification of High vs Low Biomass",
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)",
        color = "Scenario"
      ) +
      coord_fixed() +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "none"
      )
    
    ggsave(file.path(fig_dir, paste0("ROC_curve_", scale_name, ".png")),
           p_roc, width = 8, height = 8, dpi = 300)
    
    cat("  ✓ Saved: ROC_curve_", scale_name, ".png\n", sep = "")
  }
  
  cat("\n")
  
  # =============================================================================
  # FIGURE 2: PRECISION-RECALL CURVES
  # =============================================================================
  
  cat("Step 3: Creating Precision-Recall curves...\n")
  
  pr_data_list <- list()
  
  for (model_id in unique(all_predictions$model_id)) {
    
    model_preds <- all_predictions %>% filter(model_id == !!model_id)
    
    # Calculate PR curve
    fg <- model_preds$predicted_prob[model_preds$biomass_class == "High"]
    bg <- model_preds$predicted_prob[model_preds$biomass_class == "Low"]
    
    pr_obj <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
    
    # Extract curve data
    pr_curve <- data.frame(
      recall = pr_obj$curve[, 1],
      precision = pr_obj$curve[, 2],
      threshold = pr_obj$curve[, 3]
    )
    
    pr_curve$model_id <- model_id
    pr_curve$auprc <- pr_obj$auc.integral
    
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    pr_curve$scale <- parts[1]
    pr_curve$scenario <- paste(parts[-1], collapse = "_")
    
    pr_data_list[[model_id]] <- pr_curve
  }
  
  pr_data <- bind_rows(pr_data_list)
  
  # Plot PR curves by scale
  for (scale_name in unique(pr_data$scale)) {
    
    scale_data <- pr_data %>% filter(scale == scale_name)
    
    # Prepare text labels with positions
    text_data <- scale_data %>%
      group_by(model_id, scenario) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(scenario) %>%
      mutate(
        text_y = seq(0.9, 0.7, length.out = n()),
        text_label = sprintf("%s: AUPRC = %.3f", scenario, auprc)
      )
    
    p_pr <- ggplot(scale_data,
                   aes(x = recall, y = precision,
                       color = scenario, group = model_id)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = mean(all_predictions$biomass_class == "High"),
                 linetype = "dashed", color = "gray50") +
      geom_text(data = text_data,
                aes(x = 0.2, y = text_y, label = text_label),
                hjust = 0, size = 3.5, fontface = "bold") +
      scale_color_manual(values = c(
        "fia_only" = "#d62728",
        "nefin_only" = "#1f77b4",
        "pooled" = "#2ca02c"
      )) +
      labs(
        title = paste("Precision-Recall Curves:",
                      ifelse(scale_name == "fine", "Fine Scale (10m)",
                             "Coarse Scale (250m)")),
        subtitle = "Better for imbalanced datasets",
        x = "Recall (Sensitivity)",
        y = "Precision (PPV)",
        color = "Scenario"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "none"
      )
    
    ggsave(file.path(fig_dir, paste0("PR_curve_", scale_name, ".png")),
           p_pr, width = 8, height = 8, dpi = 300)
    
    cat("  ✓ Saved: PR_curve_", scale_name, ".png\n", sep = "")
  }
  
  cat("\n")
}

# =============================================================================
# FIGURE 3: SPATIAL CV PERFORMANCE (Boxplots by Fold)
# =============================================================================

cat("Step 4: Creating spatial CV performance plots...\n")

# Load fold-level regression results
cv_reg_files <- list.files(PHASE4_CONFIG$output$dir_cv,
                           pattern = "cv_regression_.*\\.csv",
                           full.names = TRUE)

cv_reg_data <- lapply(cv_reg_files, function(f) {
  model_id <- gsub("cv_regression_|\\.csv", "", basename(f))
  read_csv(f, show_col_types = FALSE) %>%
    mutate(model_id = model_id)
}) %>% bind_rows()

# Parse model components
cv_reg_data <- cv_reg_data %>%
  mutate(
    scale = sapply(strsplit(model_id, "_"), function(x) x[1]),
    scenario = sapply(strsplit(model_id, "_"), function(x) paste(x[-1], collapse = "_"))
  )

# Plot RMSE by fold
p_cv_rmse <- ggplot(cv_reg_data, 
                    aes(x = scenario, y = rmse, fill = scenario)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  facet_wrap(~scale, scales = "free_y") +
  scale_fill_manual(values = c(
    "fia_only" = "#d62728",
    "nefin_only" = "#1f77b4",
    "pooled" = "#2ca02c"
  )) +
  labs(
    title = "Spatial Cross-Validation Performance",
    subtitle = "RMSE across 10 spatial folds shows variability by region",
    x = "Scenario",
    y = "RMSE (Mg/ha)",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(fig_dir, "Spatial_CV_RMSE_boxplots.png"),
       p_cv_rmse, width = 10, height = 6, dpi = 300)

cat("  ✓ Saved: Spatial_CV_RMSE_boxplots.png\n")

# Plot R² by fold
p_cv_r2 <- ggplot(cv_reg_data,
                  aes(x = scenario, y = r2, fill = scenario)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  facet_wrap(~scale) +
  scale_fill_manual(values = c(
    "fia_only" = "#d62728",
    "nefin_only" = "#1f77b4",
    "pooled" = "#2ca02c"
  )) +
  labs(
    title = "R² Across Spatial Folds",
    subtitle = "Shows consistency of model performance across space",
    x = "Scenario",
    y = "R²",
    fill = "Scenario"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(fig_dir, "Spatial_CV_R2_boxplots.png"),
       p_cv_r2, width = 10, height = 6, dpi = 300)

cat("  ✓ Saved: Spatial_CV_R2_boxplots.png\n\n")

# =============================================================================
# FIGURE 4: SPATIAL RESIDUAL PATTERNS (Maps)
# =============================================================================

cat("Step 5: Creating spatial residual maps...\n")

# Get base map
states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(postal %in% c("VT", "NH", "ME", "MA", "NY", "CT", "RI"))

# Create maps for each model
for (model_id in unique(all_predictions$model_id)) {
  
  model_data <- all_predictions %>%
    filter(model_id == !!model_id) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Plot residuals
  p_residuals <- ggplot() +
    geom_sf(data = states, fill = "gray95", color = "gray70") +
    geom_sf(data = model_data, 
            aes(color = residual), size = 0.8, alpha = 0.6) +
    scale_color_gradient2(
      low = "#d62728", mid = "white", high = "#1f77b4",
      midpoint = 0,
      name = "Residual\n(Mg/ha)",
      limits = c(-100, 100)
    ) +
    labs(
      title = paste("Spatial Residual Pattern:", model_id),
      subtitle = "Positive = Over-prediction, Negative = Under-prediction"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "right"
    )
  
  ggsave(file.path(fig_dir, paste0("Residual_map_", model_id, ".png")),
         p_residuals, width = 10, height = 8, dpi = 300)
}

cat("  ✓ Saved residual maps for all models\n\n")

# =============================================================================
# FIGURE 5: CUMULATIVE DISTRIBUTION (Probability Maps)
# =============================================================================

if (PHASE4_CONFIG$classification$enabled) {
  
  cat("Step 6: Creating cumulative probability distribution plots...\n")
  
  for (scale_name in unique(cv_summary$scale)) {
    
    scale_models <- all_predictions %>%
      filter(grepl(paste0("^", scale_name, "_"), model_id))
    
    # CDF plot
    # Create color mapping
    color_mapping <- c("#d62728", "#1f77b4", "#2ca02c")
    names(color_mapping) <- c(
      paste0(scale_name, "_fia_only"),
      paste0(scale_name, "_nefin_only"),
      paste0(scale_name, "_pooled")
    )
    
    p_cdf <- ggplot(scale_models,
                    aes(x = predicted_prob, color = model_id)) +
      stat_ecdf(geom = "step", linewidth = 1.2) +
      scale_color_manual(
        values = color_mapping,
        labels = c("FIA Only", "NEFIN Only", "Pooled")
      ) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
      labs(
        title = paste("Cumulative Distribution of Predicted Probabilities:",
                      ifelse(scale_name == "fine", "Fine Scale", "Coarse Scale")),
        subtitle = "Shows how model prioritizes high-probability cells",
        x = "Predicted Probability (High Biomass)",
        y = "Cumulative Proportion of Study Area",
        color = "Scenario"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
      )
    
    ggsave(file.path(fig_dir, paste0("CDF_probabilities_", scale_name, ".png")),
           p_cdf, width = 10, height = 7, dpi = 300)
    
    cat("  ✓ Saved: CDF_probabilities_", scale_name, ".png\n", sep = "")
  }
  
  cat("\n")
}

# =============================================================================
# FIGURE 6: SCALE COMPARISON SUMMARY
# =============================================================================

cat("Step 7: Creating scale comparison summary...\n")

# Prepare data for comparison
comparison_data <- cv_summary %>%
  mutate(
    scale_label = ifelse(scale == "Fine Scale (10m)", "10m (Sentinel-2)", 
                         "250m (MODIS)")
  )

# RMSE comparison
p_scale_rmse <- ggplot(comparison_data,
                       aes(x = scale_label, y = rmse_mean, fill = scenario)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = rmse_mean - rmse_sd, 
                    ymax = rmse_mean + rmse_sd),
                position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c(
    "FIA Only" = "#d62728",
    "NEFIN Only" = "#1f77b4",
    "FIA + NEFIN" = "#2ca02c"
  )) +
  labs(
    title = "Scale-Dependent Prediction Accuracy",
    subtitle = "Does coordinate precision matter more at fine scales?",
    x = "Spatial Resolution",
    y = "RMSE (Mg/ha) ± SD",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(fig_dir, "Scale_comparison_RMSE.png"),
       p_scale_rmse, width = 10, height = 6, dpi = 300)

cat("  ✓ Saved: Scale_comparison_RMSE.png\n")

# AUC comparison (if available)
if (PHASE4_CONFIG$classification$enabled && "auc_mean" %in% names(comparison_data)) {
  
  p_scale_auc <- ggplot(comparison_data,
                        aes(x = scale_label, y = auc_mean, fill = scenario)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = auc_mean - auc_sd,
                      ymax = auc_mean + auc_sd),
                  position = position_dodge(0.9), width = 0.2) +
    scale_fill_manual(values = c(
      "FIA Only" = "#d62728",
      "NEFIN Only" = "#1f77b4",
      "FIA + NEFIN" = "#2ca02c"
    )) +
    geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "gray50") +
    annotate("text", x = 0.5, y = 0.72, label = "Weak (0.7)", 
             hjust = 0, size = 3) +
    annotate("text", x = 0.5, y = 0.92, label = "Strong (0.9)", 
             hjust = 0, size = 3) +
    ylim(0.5, 1.0) +
    labs(
      title = "Classification Performance (AUC) by Scale",
      subtitle = "Can we identify high-biomass areas despite fuzzing?",
      x = "Spatial Resolution",
      y = "AUC (ROC) ± SD",
      fill = "Scenario"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  ggsave(file.path(fig_dir, "Scale_comparison_AUC.png"),
         p_scale_auc, width = 10, height = 6, dpi = 300)
  
  cat("  ✓ Saved: Scale_comparison_AUC.png\n")
}

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("  SPATIAL VALIDATION PLOTS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Created figures:\n")
cat("  • ROC curves (by scale)\n")
cat("  • Precision-Recall curves (by scale)\n")
cat("  • Spatial CV boxplots (RMSE & R²)\n")
cat("  • Spatial residual maps\n")
cat("  • Cumulative probability distributions\n")
cat("  • Scale comparison summaries\n\n")

cat("Location:", fig_dir, "\n\n")

cat("Key interpretations:\n")
cat("  • AUC > 0.9: Excellent discrimination\n")
cat("  • AUC 0.7-0.9: Good discrimination\n")
cat("  • AUC < 0.7: Weak discrimination\n")
cat("  • Spatial CV boxplots show regional variability\n")
cat("  • Residual maps reveal spatial patterns in errors\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")
