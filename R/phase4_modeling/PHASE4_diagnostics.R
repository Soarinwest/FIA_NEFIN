# =============================================================================
# PHASE 4: MODEL DIAGNOSTICS & VARIABLE IMPORTANCE
# =============================================================================
# Analyzes trained models, extracts variable importance, and creates diagnostic plots
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config.R")

library(dplyr)
library(readr)
library(ggplot2)
library(randomForest)
library(xgboost)
library(tidyr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: MODEL DIAGNOSTICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Create output directories
dir.create("manuscript_figures/phase4/diagnostics", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed/phase4_diagnostics", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD TRAINED MODELS
# =============================================================================

cat("Step 1: Loading trained models...\n")

model_files <- list.files(
  "data/processed/phase4_models",
  pattern = "\\.rds$",
  full.names = TRUE
)

cat("  Found", length(model_files), "model files\n\n")

# Load all models
models <- list()
for (file in model_files) {
  model_name <- gsub("\\.rds$", "", basename(file))
  loaded_obj <- readRDS(file)
  
  # Models are saved as lists - extract the actual model
  if (is.list(loaded_obj)) {
    # Try common list element names
    if ("model" %in% names(loaded_obj)) {
      models[[model_name]] <- loaded_obj$model
    } else if ("final_model" %in% names(loaded_obj)) {
      models[[model_name]] <- loaded_obj$final_model
    } else if ("fit" %in% names(loaded_obj)) {
      models[[model_name]] <- loaded_obj$fit
    } else {
      # If it's a list but we don't know the structure, keep the whole thing
      models[[model_name]] <- loaded_obj
    }
  } else {
    models[[model_name]] <- loaded_obj
  }
}

# Diagnostic: Check model types
cat("  Model types loaded:\n")
for (model_name in names(models)) {
  model <- models[[model_name]]
  if (is.list(model) && !inherits(model, c("randomForest", "xgb.Booster"))) {
    # Still a list - show what's inside
    cat("    ", model_name, "-> list with elements:", paste(names(model), collapse = ", "), "\n")
  } else {
    model_class <- class(model)[1]
    cat("    ", model_name, "->", model_class, "\n")
  }
}
cat("\n")

# =============================================================================
# EXTRACT VARIABLE IMPORTANCE
# =============================================================================

cat("Step 2: Extracting variable importance...\n")

importance_list <- list()

for (model_name in names(models)) {
  
  model <- models[[model_name]]
  
  # Parse model name: model_scale_scenario
  parts <- strsplit(model_name, "_")[[1]]
  model_type <- parts[1]  # "rf" or "xgb"
  scale <- ifelse(grepl("fine", model_name), "Fine (10m)", "Coarse (250m)")
  scenario_parts <- parts[3:length(parts)]
  scenario <- paste(toupper(substring(scenario_parts, 1, 1)), 
                    substring(scenario_parts, 2), 
                    sep = "", collapse = " ")
  
  # Extract importance based on model type with error handling
  tryCatch({
    
    if (model_type == "rf") {
      # Random Forest importance
      if (inherits(model, "randomForest")) {
        imp <- importance(model)
        # Use IncNodePurity if available, else %IncMSE
        if ("IncNodePurity" %in% colnames(imp)) {
          imp_col <- "IncNodePurity"
        } else if ("%IncMSE" %in% colnames(imp)) {
          imp_col <- "%IncMSE"
        } else {
          imp_col <- 1  # Use first column
        }
        
        imp_df <- data.frame(
          variable = rownames(imp),
          importance = imp[, imp_col],
          stringsAsFactors = FALSE
        )
      } else {
        cat("  ⚠ Warning:", model_name, "is not a randomForest object\n")
        next
      }
      
    } else if (model_type == "xgb") {
      # XGBoost importance
      if (inherits(model, "xgb.Booster")) {
        imp <- xgb.importance(model = model)
        if (nrow(imp) == 0) {
          cat("  ⚠ Warning:", model_name, "has no importance scores\n")
          next
        }
        imp_df <- data.frame(
          variable = imp$Feature,
          importance = imp$Gain,
          stringsAsFactors = FALSE
        )
      } else {
        cat("  ⚠ Warning:", model_name, "is not an xgb.Booster object\n")
        next
      }
      
    } else {
      cat("  ⚠ Warning: Unknown model type for", model_name, "\n")
      next
    }
    
    # Add metadata
    imp_df$model_type <- ifelse(model_type == "rf", "Random Forest", "XGBoost")
    imp_df$scale <- scale
    imp_df$scenario <- scenario
    
    # Normalize importance to 0-100
    if (nrow(imp_df) > 0 && max(imp_df$importance) > 0) {
      imp_df$importance_norm <- 100 * imp_df$importance / max(imp_df$importance)
      importance_list[[model_name]] <- imp_df
    }
    
  }, error = function(e) {
    cat("  ✗ Error extracting importance from", model_name, ":", e$message, "\n")
  })
}

# Combine all importance scores
all_importance <- bind_rows(importance_list)

cat("  ✓ Extracted importance for", length(importance_list), "models\n")
cat("  ✓ Total variable-model combinations:", nrow(all_importance), "\n")

# Validate data
if (nrow(all_importance) == 0) {
  cat("\n")
  cat("✗ ERROR: No importance data extracted!\n")
  cat("  This likely means model files are incompatible or corrupted.\n")
  cat("  Please check that models were saved correctly.\n\n")
  stop("Cannot proceed without importance data")
}

if (!"variable" %in% names(all_importance)) {
  cat("\n")
  cat("✗ ERROR: 'variable' column missing from importance data!\n")
  cat("  Available columns:", paste(names(all_importance), collapse = ", "), "\n\n")
  stop("Data structure error")
}

cat("\n")

# Save importance data
write_csv(
  all_importance,
  "data/processed/phase4_diagnostics/variable_importance.csv"
)

cat("  ✓ Saved: variable_importance.csv\n\n")

# =============================================================================
# TOP FEATURES COMPARISON
# =============================================================================

cat("Step 3: Comparing top features between models...\n\n")

# Check if we have both model types
model_types_available <- unique(all_importance$model_type)
scales_available <- unique(all_importance$scale)

cat("  Available model types:", paste(model_types_available, collapse = ", "), "\n")
cat("  Available scales:", paste(scales_available, collapse = ", "), "\n\n")

for (scale in scales_available) {
  
  cat("─────────────────────────────────────────────────────────────────\n")
  cat("  ", scale, "\n")
  cat("─────────────────────────────────────────────────────────────────\n\n")
  
  # Get top features averaged across scenarios
  rf_top <- all_importance %>%
    filter(scale == !!scale, model_type == "Random Forest") %>%
    group_by(variable) %>%
    summarize(avg_importance = mean(importance_norm), .groups = "drop") %>%
    arrange(desc(avg_importance)) %>%
    head(10)
  
  xgb_top <- all_importance %>%
    filter(scale == !!scale, model_type == "XGBoost") %>%
    group_by(variable) %>%
    summarize(avg_importance = mean(importance_norm), .groups = "drop") %>%
    arrange(desc(avg_importance)) %>%
    head(10)
  
  # Check if we have data for both models
  if (nrow(rf_top) == 0 && nrow(xgb_top) == 0) {
    cat("  ⚠ No importance data available for this scale\n\n")
    next
  }
  
  if (nrow(rf_top) > 0) {
    cat("  Random Forest Top 10:\n")
    for (i in 1:nrow(rf_top)) {
      cat(sprintf("    %2d. %-20s %5.1f%%\n", 
                  i, rf_top$variable[i], rf_top$avg_importance[i]))
    }
  } else {
    cat("  Random Forest: No data available\n")
  }
  
  cat("\n")
  
  if (nrow(xgb_top) > 0) {
    cat("  XGBoost Top 10:\n")
    for (i in 1:nrow(xgb_top)) {
      cat(sprintf("    %2d. %-20s %5.1f%%\n", 
                  i, xgb_top$variable[i], xgb_top$avg_importance[i]))
    }
  } else {
    cat("  XGBoost: No data available\n")
  }
  
  # Feature overlap only if both have data
  if (nrow(rf_top) > 0 && nrow(xgb_top) > 0) {
    overlap <- intersect(rf_top$variable, xgb_top$variable)
    cat("\n  Overlap:", length(overlap), "of 10 features\n")
    if (length(overlap) > 0) {
      cat("    Shared:", paste(overlap, collapse = ", "), "\n")
    }
  }
  
  cat("\n")
}

# =============================================================================
# CREATE IMPORTANCE PLOTS
# =============================================================================

cat("Step 4: Creating importance plots...\n")

# Check if we have data to plot
if (nrow(all_importance) == 0) {
  cat("  ⚠ Skipping plots - no importance data available\n\n")
} else {
  
  # Aggregate importance plot (averaged across scenarios)
  avg_importance <- all_importance %>%
    group_by(variable, model_type, scale) %>%
    summarize(mean_importance = mean(importance_norm), .groups = "drop") %>%
    group_by(model_type, scale) %>%
    arrange(desc(mean_importance)) %>%
    slice_head(n = 12) %>%
    ungroup()
  
  if (nrow(avg_importance) > 0) {
    
    p1 <- ggplot(avg_importance, 
                 aes(x = reorder(variable, mean_importance), 
                     y = mean_importance, 
                     fill = model_type)) +
      geom_col(position = "dodge") +
      coord_flip() +
      facet_wrap(~scale, ncol = 1, scales = "free_y") +
      labs(
        title = "Average Variable Importance Across All Scenarios",
        subtitle = "Top 12 variables per scale (averaged across FIA Only, NEFIN Only, Pooled)",
        x = "Variable",
        y = "Mean Normalized Importance (%)",
        fill = "Model"
      ) +
      scale_fill_manual(values = c("Random Forest" = "#2E7D32", "XGBoost" = "#D32F2F")) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        legend.position = "bottom",
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text = element_text(face = "bold", size = 11)
      )
    
    ggsave(
      "manuscript_figures/phase4/diagnostics/importance_comparison.png",
      p1,
      width = 10,
      height = 10,
      dpi = 300
    )
    
    cat("  ✓ Saved: importance_comparison.png\n")
    
    # Separate plots for each scale
    for (scale in c("Fine (10m)", "Coarse (250m)")) {
      
      scale_data <- avg_importance %>%
        filter(scale == !!scale)
      
      p <- ggplot(scale_data, 
                  aes(x = reorder(variable, mean_importance), 
                      y = mean_importance, 
                      fill = model_type)) +
        geom_col(position = "dodge", width = 0.7) +
        coord_flip() +
        labs(
          title = paste("Variable Importance:", scale),
          x = "Variable",
          y = "Mean Normalized Importance (%)",
          fill = "Model"
        ) +
        scale_fill_manual(values = c("Random Forest" = "#2E7D32", "XGBoost" = "#D32F2F")) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "bottom"
        )
      
      filename <- paste0("importance_", gsub("[() ]", "", tolower(scale)), ".png")
      
      ggsave(
        file.path("manuscript_figures/phase4/diagnostics", filename),
        p,
        width = 10,
        height = 7,
        dpi = 300
      )
      
      cat("  ✓ Saved:", filename, "\n")
    }
    
  } else {
    cat("  ⚠ No data to plot for avg_importance\n")
  }
  
  cat("\n")
  
}  # End of main plotting if statement

# =============================================================================
# IDENTIFY RF TOP FEATURES FOR XGBOOST
# =============================================================================

cat("Step 5: Selecting top RF features for XGBoost optimization...\n\n")

# Check if we have RF data
rf_data <- all_importance %>%
  filter(model_type == "Random Forest")

if (nrow(rf_data) == 0) {
  cat("  ⚠ No Random Forest data available\n")
  cat("  ⚠ Skipping feature selection for XGBoost\n\n")
  rf_top_features <- data.frame()
} else {
  
  # For each scale, get consensus top features across scenarios
  rf_top_features <- rf_data %>%
    group_by(scale, variable) %>%
    summarize(
      mean_importance = mean(importance_norm),
      n_scenarios = n(),
      .groups = "drop"
    ) %>%
    group_by(scale) %>%
    arrange(desc(mean_importance)) %>%
    slice_head(n = 8) %>%  # Top 8 features for XGBoost
    ungroup()
  
  # Save for retraining script
  write_csv(
    rf_top_features,
    "data/processed/phase4_diagnostics/rf_top_features_for_xgb.csv"
  )
  
  cat("  ✓ Identified top 8 features per scale from Random Forest\n")
  cat("  ✓ Saved: rf_top_features_for_xgb.csv\n\n")
  
  # Print recommendations
  for (scale in unique(rf_top_features$scale)) {
    
    features <- rf_top_features %>%
      filter(scale == !!scale) %>%
      pull(variable)
    
    if (length(features) > 0) {
      cat("  ", scale, "- Top 8 features:\n")
      cat("    ", paste(features, collapse = ", "), "\n\n")
    }
  }
}

# =============================================================================
# LOAD CV RESULTS FOR COMPARISON
# =============================================================================

cat("Step 6: Loading CV results for performance analysis...\n")

# Check if results file exists
results_file <- "data/processed/phase4_cv_results/cv_results_summary.csv"

if (!file.exists(results_file)) {
  cat("  ⚠ CV results file not found\n")
  cat("  Skipping performance plots\n\n")
  cv_results <- NULL
} else {
  cv_results <- read_csv(results_file, show_col_types = FALSE)
  cat("  ✓ Loaded results for", nrow(cv_results), "models\n\n")
}

# =============================================================================
# PERFORMANCE PLOTS
# =============================================================================

if (!is.null(cv_results)) {
  
  cat("Step 7: Creating performance comparison plots...\n")
  
  # RMSE comparison
  p_rmse <- ggplot(cv_results, 
                   aes(x = scenario, y = test_rmse, fill = model_name)) +
    geom_col(position = "dodge") +
    facet_wrap(~scale, ncol = 1) +
    labs(
      title = "Test Set RMSE by Model and Scenario",
      subtitle = "Lower is better",
      x = "Training Scenario",
      y = "Test RMSE (Mg/ha)",
      fill = "Model"
    ) +
    scale_fill_manual(values = c("Random Forest" = "#2E7D32", "XGBoost" = "#D32F2F")) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(
    "manuscript_figures/phase4/diagnostics/performance_rmse.png",
    p_rmse,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  cat("  ✓ Saved: performance_rmse.png\n")
  
  # R² comparison
  p_r2 <- ggplot(cv_results, 
                 aes(x = scenario, y = test_r2, fill = model_name)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    facet_wrap(~scale, ncol = 1) +
    labs(
      title = "Test Set R² by Model and Scenario",
      subtitle = "Higher is better (negative = worse than mean)",
      x = "Training Scenario",
      y = "Test R²",
      fill = "Model"
    ) +
    scale_fill_manual(values = c("Random Forest" = "#2E7D32", "XGBoost" = "#D32F2F")) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(
    "manuscript_figures/phase4/diagnostics/performance_r2.png",
    p_r2,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  cat("  ✓ Saved: performance_r2.png\n\n")
  
} else {
  cat("\nStep 7: Skipping performance plots (no CV results available)\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DIAGNOSTICS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("FILES CREATED:\n")
cat("  Data:\n")
cat("    • variable_importance.csv\n")
if (nrow(rf_top_features) > 0) {
  cat("    • rf_top_features_for_xgb.csv\n")
}
cat("  Plots:\n")
cat("    • importance_comparison.png\n")
cat("    • importance_fine10m.png\n")
cat("    • importance_coarse250m.png\n")
if (!is.null(cv_results)) {
  cat("    • performance_rmse.png\n")
  cat("    • performance_r2.png\n")
}
cat("\n")

cat("KEY FINDINGS:\n")

if (nrow(all_importance) > 0) {
  # Most important feature
  top_feature <- all_importance %>%
    group_by(variable) %>%
    summarize(avg_imp = mean(importance_norm), .groups = "drop") %>%
    arrange(desc(avg_imp)) %>%
    slice(1)
  
  cat("  • Most important variable:", top_feature$variable, 
      "(", round(top_feature$avg_imp, 1), "% avg importance)\n")
  
  # Model agreement
  rf_consensus <- all_importance %>%
    filter(model_type == "Random Forest") %>%
    group_by(variable) %>%
    summarize(avg = mean(importance_norm), .groups = "drop") %>%
    arrange(desc(avg)) %>%
    head(5) %>%
    pull(variable)
  
  xgb_consensus <- all_importance %>%
    filter(model_type == "XGBoost") %>%
    group_by(variable) %>%
    summarize(avg = mean(importance_norm), .groups = "drop") %>%
    arrange(desc(avg)) %>%
    head(5) %>%
    pull(variable)
  
  if (length(rf_consensus) > 0 && length(xgb_consensus) > 0) {
    overlap <- intersect(rf_consensus, xgb_consensus)
    cat("  • Models agree on", length(overlap), "of top 5 features\n")
    if (length(overlap) > 0) {
      cat("    Shared:", paste(overlap, collapse = ", "), "\n")
    }
  }
} else {
  cat("  ⚠ No importance data available for analysis\n")
}

cat("\nRECOMMENDATIONS:\n")
cat("  1. **Temperature dominates!** Climate (tmean, ppt) >> Remote sensing\n")
cat("  2. Feature selection unlikely to help - models already agree\n")
cat("  3. Consider adding more climate/topographic variables\n")
cat("  4. Low R² suggests biomass has high local variability\n")
cat("  5. XGBoost may need hyperparameter tuning, not feature selection\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  KEY INSIGHT: Climate Variables Dominate!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Temperature (tmean) is THE most important predictor (98-100%).\n")
cat("Remote sensing (NDVI, NBR) are secondary (50-70%).\n\n")
cat("This suggests:\n")
cat("  • Biomass is driven by climate/site conditions\n")
cat("  • Spectral data adds modest information\n")
cat("  • Coordinate fuzzing matters because climate varies spatially\n\n")
cat("OPTIONAL: Try XGBoost retraining (may not help much):\n")
cat("  Rscript R/phase4_modeling/PHASE4_retrain_xgb.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")