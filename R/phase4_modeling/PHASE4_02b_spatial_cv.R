# =============================================================================
# PHASE 4 - Spatial Cross-Validation with MULTIPLE MODELS (RF + XGBoost)
# =============================================================================
# Performs spatial 10-fold cross-validation comparing Random Forest and XGBoost
# Calculates both regression and classification metrics
# 
# Models: 2 algorithms × 2 scales × 3 scenarios = 12 models total
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(dplyr)
library(readr)
library(randomForest)
library(xgboost)       # XGBoost support
library(blockCV)       # Spatial cross-validation
library(sf)            # Spatial data
library(pROC)          # ROC curves and AUC
library(PRROC)         # Precision-Recall curves
library(caret)         # Confusion matrix

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: SPATIAL CROSS-VALIDATION (MULTI-MODEL)\n")
cat("  Comparing Random Forest vs XGBoost\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Print configuration
print_phase4_config()

# Determine which models to run
models_to_run <- c()
if (PHASE4_CONFIG$models$rf$enabled) models_to_run <- c(models_to_run, "rf")
if (PHASE4_CONFIG$models$xgb$enabled) models_to_run <- c(models_to_run, "xgb")

if (length(models_to_run) == 0) {
  stop("No models enabled! Enable RF or XGBoost in config.")
}

cat("Models to compare:", length(models_to_run), "\n")
for (model_type in models_to_run) {
  model_name <- if (model_type == "rf") "Random Forest" else "XGBoost"
  cat("  •", model_name, "\n")
}
cat("\n")

# =============================================================================
# LOAD PREPARED DATA
# =============================================================================

cat("Step 1: Loading prepared datasets...\n")

# Load the full dataset - combine train and test OR load from augmented
if (file.exists("data/processed/phase4_modeling/train_data.csv") &&
    file.exists("data/processed/phase4_modeling/test_data.csv")) {
  
  cat("  Loading from train/test files...\n")
  train_data_prep <- read_csv("data/processed/phase4_modeling/train_data.csv",
                              show_col_types = FALSE)
  test_data_prep <- read_csv("data/processed/phase4_modeling/test_data.csv",
                             show_col_types = FALSE)
  full_data <- bind_rows(train_data_prep, test_data_prep)
  
  # Check if coordinates are present
  has_coords <- any(tolower(names(full_data)) %in% c("lon", "longitude", "x")) &&
    any(tolower(names(full_data)) %in% c("lat", "latitude", "y"))
  
  if (!has_coords) {
    cat("  Coordinates missing from train/test - loading from augmented data...\n")
    
    # Load augmented data to get coordinates
    augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                          show_col_types = FALSE) %>%
      select(CN, lon, lat) %>%
      filter(CN %in% full_data$CN)
    
    # Join coordinates back to full data
    full_data <- full_data %>%
      left_join(augmented, by = "CN")
  }
  
} else if (file.exists("data/processed/augmented_with_covariates.csv")) {
  
  cat("  Loading from augmented_with_covariates.csv...\n")
  full_data <- read_csv("data/processed/augmented_with_covariates.csv",
                        show_col_types = FALSE) %>%
    filter(!is.na(biomass), biomass > 0)
  
} else {
  stop("Cannot find data files! Run PHASE4_01_prep_data.R first.")
}

cat("  ✓ Total plots:", nrow(full_data), "\n")
cat("    FIA:", sum(full_data$dataset == "FIA"), "\n")
cat("    NEFIN:", sum(full_data$dataset == "NEFIN"), "\n\n")

# Get available covariates
all_covs <- names(full_data)[!names(full_data) %in% 
                               c("CN", "dataset", "biomass", "lon", "lat")]
cat("  Available covariates:", length(all_covs), "\n")
cat("  ", paste(head(all_covs, 10), collapse = ", "), "...\n\n")

# =============================================================================
# CREATE CLASSIFICATION LABELS
# =============================================================================

cat("Step 2: Creating classification labels...\n")

threshold <- PHASE4_CONFIG$classification$threshold
full_data$biomass_class <- factor(
  ifelse(full_data$biomass >= threshold, "High", "Low"),
  levels = c("Low", "High")
)

cat("  Threshold:", threshold, "Mg/ha\n")
cat("  High biomass:", sum(full_data$biomass_class == "High"), "plots\n")
cat("  Low biomass:", sum(full_data$biomass_class == "Low"), "plots\n")
cat("  Proportion high:", 
    sprintf("%.1f", 100 * mean(full_data$biomass_class == "High")), "%\n\n")

# =============================================================================
# CREATE SPATIAL BLOCKS FOR CV
# =============================================================================

cat("Step 3: Creating spatial blocks for cross-validation...\n")

# Detect coordinate column names
coord_cols <- names(full_data)
lon_col <- coord_cols[tolower(coord_cols) %in% c("lon", "longitude", "x")]
lat_col <- coord_cols[tolower(coord_cols) %in% c("lat", "latitude", "y")]

if (length(lon_col) == 0 || length(lat_col) == 0) {
  cat("\n  Available columns:", paste(head(coord_cols, 20), collapse = ", "), "\n")
  stop("Cannot find coordinate columns! Looking for: lon/longitude/x and lat/latitude/y")
}

lon_col <- lon_col[1]
lat_col <- lat_col[1]

cat("  Using coordinates:", lon_col, "and", lat_col, "\n")

# Convert to spatial object
full_data_sf <- st_as_sf(full_data, coords = c(lon_col, lat_col), crs = 4326)

# Create spatial blocks
set.seed(PHASE4_CONFIG$cv$seed)

cat("  Creating spatial blocks...\n")
invisible(capture.output({
  spatial_blocks <- cv_spatial(
    x = full_data_sf,
    k = PHASE4_CONFIG$cv$n_folds,
    size = PHASE4_CONFIG$cv$block_size_km * 1000,
    selection = "random",
    iteration = 100
  )
}))

cat("  ✓ Created", PHASE4_CONFIG$cv$n_folds, "spatial folds\n")
cat("  Block size:", PHASE4_CONFIG$cv$block_size_km, "km\n\n")

cat("  Fold sizes (plots per fold):\n\n")
print(table(spatial_blocks$folds_ids))
cat("\n\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Calculate regression metrics
calc_regression_metrics <- function(observed, predicted) {
  residuals <- observed - predicted
  list(
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    bias = mean(residuals),
    r2 = 1 - sum(residuals^2) / sum((observed - mean(observed))^2),
    mape = mean(abs(residuals / observed)) * 100
  )
}

# Calculate classification metrics
calc_classification_metrics <- function(observed_class, predicted_prob, predicted_class) {
  
  # Handle case where test set has only one class
  if (length(unique(observed_class)) == 1) {
    return(list(
      auc = NA,
      auprc = NA,
      sensitivity = NA,
      specificity = NA,
      precision = NA,
      f1 = NA,
      kappa = NA
    ))
  }
  
  # Ensure proper factor levels
  if (!is.factor(predicted_class)) {
    predicted_class <- factor(predicted_class, levels = c("Low", "High"))
  }
  if (!is.factor(observed_class)) {
    observed_class <- factor(observed_class, levels = c("Low", "High"))
  }
  
  # Find optimal threshold if needed
  if (length(unique(predicted_class)) == 1) {
    roc_obj <- roc(observed_class, predicted_prob,
                   levels = c("Low", "High"), direction = "<")
    coords_result <- coords(roc_obj, "best", ret = "threshold",
                            best.method = "youden")
    optimal_threshold <- coords_result$threshold
    predicted_class <- ifelse(predicted_prob > optimal_threshold, 
                              "High", "Low")
    predicted_class <- factor(predicted_class, levels = c("Low", "High"))
  }
  
  # ROC-AUC
  roc_obj <- roc(observed_class, predicted_prob,
                 levels = c("Low", "High"), direction = "<")
  auc_value <- as.numeric(auc(roc_obj))
  
  # Precision-Recall AUC
  fg <- predicted_prob[observed_class == "High"]
  bg <- predicted_prob[observed_class == "Low"]
  pr_obj <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
  auprc_value <- pr_obj$auc.integral
  
  # Confusion matrix metrics
  cm <- confusionMatrix(predicted_class, observed_class, positive = "High")
  
  list(
    auc = auc_value,
    auprc = auprc_value,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    precision = cm$byClass["Precision"],
    f1 = cm$byClass["F1"],
    kappa = cm$overall["Kappa"]
  )
}

# =============================================================================
# MAIN CV LOOP - BY MODEL TYPE
# =============================================================================

# Storage for all results
all_cv_results <- list()

# Loop through each model type
for (model_type in models_to_run) {
  
  model_name <- if (model_type == "rf") "Random Forest" else "XGBoost"
  
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  MODEL:", toupper(model_name), "\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Loop through scales
  for (scale_name in names(PHASE4_CONFIG$spatial_scales)) {
    
    scale_info <- PHASE4_CONFIG$spatial_scales[[scale_name]]
    
    cat("\n")
    cat("═══════════════════════════════════════════════════════════════\n")
    cat("  ", toupper(scale_info$name), "\n")
    cat("═══════════════════════════════════════════════════════════════\n\n")
    
    # Get covariates for this scale
    scale_covs <- get_scale_covariates(scale_name)
    scale_covs <- intersect(scale_covs, all_covs)
    
    cat("  Using", length(scale_covs), "covariates:\n")
    cat("  ", paste(scale_covs, collapse = ", "), "\n\n")
    
    # Loop through scenarios
    for (scenario_name in names(PHASE4_CONFIG$scenarios)) {
      
      scenario_info <- PHASE4_CONFIG$scenarios[[scenario_name]]
      
      cat("\n")
      cat("───────────────────────────────────────────────────────────────\n")
      cat("  ", scenario_info$name, "\n")
      cat("───────────────────────────────────────────────────────────────\n\n")
      
      # Filter data for this scenario
      if (scenario_name == "fia_only") {
        scenario_data <- full_data %>% filter(dataset == "FIA")
      } else if (scenario_name == "nefin_only") {
        scenario_data <- full_data %>% filter(dataset == "NEFIN")
      } else {
        scenario_data <- full_data
      }
      
      cat("  Data: ", nrow(scenario_data), "plots\n\n")
      
      scenario_data <- scenario_data %>%
        filter(if_all(all_of(scale_covs), ~ !is.na(.)))
      
      cat("  Data after NA removal:", nrow(scenario_data), "plots\n\n")
      
      if (nrow(scenario_data) < 50) {
        cat("  ⚠ WARNING: Insufficient data after NA removal\n")
        cat("  Skipping this scenario...\n\n")
        next
      }
      
      # Storage for fold results
      fold_results_regression <- list()
      fold_results_classification <- list()
      fold_predictions <- data.frame()
      
      cat("  Running", PHASE4_CONFIG$cv$n_folds, "-fold spatial CV...\n")
      
      # Loop through folds
      for (fold in 1:PHASE4_CONFIG$cv$n_folds) {
        
        # Get train/test indices for this fold
        # Note: spatial_blocks$folds_ids corresponds to full_data rows
        fold_assignment <- spatial_blocks$folds_ids
        
        # Map fold assignments to scenario_data
        scenario_data$fold_id <- fold_assignment[match(scenario_data$CN, full_data$CN)]
        
        # Split into train/test
        test_data <- scenario_data[scenario_data$fold_id == fold, ]
        train_data <- scenario_data[scenario_data$fold_id != fold, ]
        
        if (nrow(test_data) == 0 || nrow(train_data) < 20) {
          cat("    Fold", fold, "skipped (insufficient data)\n")
          next
        }
        
        # Prepare formula
        formula_reg <- as.formula(paste("biomass ~", 
                                        paste(scale_covs, collapse = " + ")))
        
        # ================================================================
        # TRAIN MODEL (RF or XGBoost)
        # ================================================================
        
        set.seed(PHASE4_CONFIG$cv$seed + fold)
        
        if (model_type == "rf") {
          # ============================================================
          # RANDOM FOREST
          # ============================================================
          
          mtry_val <- floor(sqrt(length(scale_covs)))
          
          # Regression model
          rf_reg <- randomForest(
            formula = formula_reg,
            data = train_data,
            ntree = PHASE4_CONFIG$models$rf$params$ntree,
            mtry = mtry_val,
            nodesize = PHASE4_CONFIG$models$rf$params$nodesize,
            importance = FALSE
          )
          
          cat("    Fold", fold, "covariate ranges:\n")
          for (cov in scale_covs[1:2]) {  # Just check first 2
            cat("      ", cov, "train:", 
                sprintf("%.2f to %.2f", min(train_data[[cov]], na.rm=TRUE), 
                        max(train_data[[cov]], na.rm=TRUE)), "\n")
          }
          
          # Predict
          pred_biomass <- predict(rf_reg, newdata = test_data)
          
          # Classification (if enabled)
          if (PHASE4_CONFIG$classification$enabled) {
            formula_class <- as.formula(paste("biomass_class ~",
                                              paste(scale_covs, collapse = " + ")))
            rf_class <- randomForest(
              formula = formula_class,
              data = train_data,
              ntree = PHASE4_CONFIG$models$rf$params$ntree,
              mtry = mtry_val,
              nodesize = PHASE4_CONFIG$models$rf$params$nodesize
            )
            pred_prob <- predict(rf_class, newdata = test_data, type = "prob")[, "High"]
            pred_class <- predict(rf_class, newdata = test_data, type = "response")
          }
          
        } else if (model_type == "xgb") {
          # ============================================================
          # XGBOOST
          # ============================================================
          
          # Prepare matrices
          train_matrix <- as.matrix(train_data[, scale_covs])
          test_matrix <- as.matrix(test_data[, scale_covs])
          
          # Regression model
          xgb_reg <- xgboost(
            data = train_matrix,
            label = train_data$biomass,
            nrounds = PHASE4_CONFIG$models$xgb$params$nrounds,
            max_depth = PHASE4_CONFIG$models$xgb$params$max_depth,
            eta = PHASE4_CONFIG$models$xgb$params$eta,
            objective = "reg:squarederror",
            eval_metric = "rmse",
            verbose = 0
          )
          
          # Predict
          pred_biomass <- predict(xgb_reg, newdata = test_matrix)
          
          # Classification (if enabled)
          if (PHASE4_CONFIG$classification$enabled) {
            # Convert to binary (0/1)
            train_class_binary <- as.numeric(train_data$biomass_class == "High")
            
            xgb_class <- xgboost(
              data = train_matrix,
              label = train_class_binary,
              nrounds = PHASE4_CONFIG$models$xgb$params$nrounds,
              max_depth = PHASE4_CONFIG$models$xgb$params$max_depth,
              eta = PHASE4_CONFIG$models$xgb$params$eta,
              objective = "binary:logistic",
              eval_metric = "auc",
              verbose = 0
            )
            
            pred_prob <- predict(xgb_class, newdata = test_matrix)
            pred_class <- factor(ifelse(pred_prob > 0.5, "High", "Low"),
                                 levels = c("Low", "High"))
          }
        }
        
        # ================================================================
        # CALCULATE METRICS
        # ================================================================
        
        # Regression metrics
        reg_metrics <- calc_regression_metrics(test_data$biomass, pred_biomass)
        
        # Store as data frame row for proper bind_rows() aggregation
        fold_results_regression[[fold]] <- data.frame(
          fold = fold,
          rmse = reg_metrics$rmse,
          mae = reg_metrics$mae,
          bias = reg_metrics$bias,
          r2 = reg_metrics$r2,
          mape = reg_metrics$mape
        )
        
        # Classification metrics
        if (PHASE4_CONFIG$classification$enabled) {
          class_metrics <- calc_classification_metrics(
            test_data$biomass_class,
            pred_prob,
            pred_class
          )
          fold_results_classification[[fold]] <- c(fold = fold, class_metrics)
        }
        
        # Store predictions
        fold_pred <- test_data %>%
          select(CN, dataset, biomass, all_of(c(lon_col, lat_col))) %>%
          mutate(
            fold = fold,
            predicted_biomass = pred_biomass,
            residual = biomass - predicted_biomass
          )
        
        # Rename coordinates to standard names for consistency
        names(fold_pred)[names(fold_pred) == lon_col] <- "lon"
        names(fold_pred)[names(fold_pred) == lat_col] <- "lat"
        
        if (PHASE4_CONFIG$classification$enabled) {
          fold_pred <- fold_pred %>%
            mutate(
              biomass_class = test_data$biomass_class,
              predicted_prob = pred_prob,
              predicted_class = pred_class
            )
        }
        
        fold_predictions <- bind_rows(fold_predictions, fold_pred)
        
        # Progress indicator
        if (fold %% 2 == 0) {
          cat("    Folds", fold-1, "-", fold, "complete\n")
        }
      }
      
      cat("\n")
      cat("  ✓ 10-fold CV complete\n\n")
      
      # =============================================================
      # AGGREGATE RESULTS
      # =============================================================
      
      # Filter out NULL/empty folds
      fold_results_regression <- fold_results_regression[!sapply(fold_results_regression, is.null)]
      
      if (length(fold_results_regression) == 0) {
        cat("  ⚠ WARNING: No valid folds for", scenario_info$name, "at", scale_info$name, "\n")
        cat("  Skipping this scenario...\n\n")
        next  # Skip to next scenario
      }
      
      cv_reg_df <- bind_rows(fold_results_regression)
      
      # Check if cv_reg_df is valid
      if (nrow(cv_reg_df) == 0 || all(is.na(cv_reg_df$rmse))) {
        cat("  ⚠ WARNING: Invalid metrics for", scenario_info$name, "at", scale_info$name, "\n")
        cat("  Skipping this scenario...\n\n")
        next
      }
      
      # Regression summary
      reg_summary <- cv_reg_df %>%
        summarise(
          across(c(rmse, mae, bias, r2, mape), 
                 list(mean = ~mean(., na.rm = TRUE), 
                      sd = ~sd(., na.rm = TRUE)), 
                 .names = "{.col}_{.fn}")
        )
      
      cat("  REGRESSION RESULTS:\n")
      cat("    RMSE: ", sprintf("%.2f ± %.2f Mg/ha\n", 
                                reg_summary$rmse_mean, reg_summary$rmse_sd))
      cat("    R²:   ", sprintf("%.3f ± %.3f\n", 
                                reg_summary$r2_mean, reg_summary$r2_sd))
      cat("    MAE:  ", sprintf("%.2f ± %.2f Mg/ha\n", 
                                reg_summary$mae_mean, reg_summary$mae_sd))
      
      # Classification summary
      if (PHASE4_CONFIG$classification$enabled && 
          length(fold_results_classification) > 0) {
        cv_class_df <- bind_rows(fold_results_classification)
        class_summary <- cv_class_df %>%
          summarise(
            across(c(auc, auprc, sensitivity, specificity, precision, f1), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}")
          )
        
        cat("\n  CLASSIFICATION RESULTS:\n")
        cat("    AUC:       ", sprintf("%.3f ± %.3f\n", 
                                       class_summary$auc_mean, 
                                       class_summary$auc_sd))
        cat("    Precision: ", sprintf("%.3f ± %.3f\n", 
                                       class_summary$precision_mean, 
                                       class_summary$precision_sd))
      }
      
      # =============================================================
      # SAVE RESULTS
      # =============================================================
      
      model_id <- paste(model_type, scale_name, scenario_name, sep = "_")
      
      # Save fold-level results
      write_csv(cv_reg_df, 
                file.path(PHASE4_CONFIG$output$dir_cv,
                          paste0("cv_regression_", model_id, ".csv")))
      
      if (PHASE4_CONFIG$classification$enabled && 
          length(fold_results_classification) > 0) {
        write_csv(cv_class_df,
                  file.path(PHASE4_CONFIG$output$dir_cv,
                            paste0("cv_classification_", model_id, ".csv")))
      }
      
      # Save predictions
      dir.create(file.path(PHASE4_CONFIG$output$dir_cv, "fold_predictions"),
                 showWarnings = FALSE, recursive = TRUE)
      write_csv(fold_predictions,
                file.path(PHASE4_CONFIG$output$dir_cv, "fold_predictions",
                          paste0("predictions_", model_id, ".csv")))
      
      # Store summary for final table
      summary_row <- data.frame(
        model_type = model_type,
        model = model_id,
        scale = scale_info$name,
        scenario = scenario_info$name,
        n_plots = nrow(scenario_data),
        n_covariates = length(scale_covs),
        rmse_mean = reg_summary$rmse_mean,
        rmse_sd = reg_summary$rmse_sd,
        r2_mean = reg_summary$r2_mean,
        r2_sd = reg_summary$r2_sd
      )
      
      if (PHASE4_CONFIG$classification$enabled && 
          !is.na(class_summary$auc_mean)) {
        summary_row$auc_mean <- class_summary$auc_mean
        summary_row$auc_sd <- class_summary$auc_sd
        summary_row$auprc_mean <- class_summary$auprc_mean
        summary_row$auprc_sd <- class_summary$auprc_sd
      }
      
      all_cv_results[[model_id]] <- summary_row
    }
  }
}

# =============================================================================
# SAVE SUMMARY TABLE
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  SPATIAL CV COMPLETE - SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cv_summary <- bind_rows(all_cv_results)
print(cv_summary)

write_csv(cv_summary, 
          file.path(PHASE4_CONFIG$output$dir_cv, "cv_summary.csv"))

cat("\n✓ Spatial cross-validation complete!\n")
cat("  Results saved to:", PHASE4_CONFIG$output$dir_cv, "\n\n")

# =============================================================================
# MODEL COMPARISON
# =============================================================================

if (length(models_to_run) > 1) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  MODEL COMPARISON: Random Forest vs XGBoost\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Select columns that exist
  select_cols <- c("model_type", "scale", "scenario", "rmse_mean", "r2_mean")
  if ("auc_mean" %in% names(cv_summary)) {
    select_cols <- c(select_cols, "auc_mean")
  }
  
  # Compare by scenario
  comparison <- cv_summary %>%
    select(all_of(select_cols)) %>%
    group_by(scale, scenario) %>%
    arrange(model_type) %>%
    summarise(
      rf_rmse = rmse_mean[model_type == "rf"][1],
      xgb_rmse = rmse_mean[model_type == "xgb"][1],
      rmse_diff = xgb_rmse - rf_rmse,
      rmse_pct_change = 100 * (xgb_rmse - rf_rmse) / rf_rmse,
      rf_r2 = r2_mean[model_type == "rf"][1],
      xgb_r2 = r2_mean[model_type == "xgb"][1],
      .groups = "drop"
    )
  
  print(comparison)
  
  write_csv(comparison,
            file.path(PHASE4_CONFIG$output$dir_cv, "model_comparison.csv"))
  
  # Summary
  cat("\n")
  if (mean(comparison$rmse_diff, na.rm = TRUE) < 0) {
    cat("✓ XGBoost performs BETTER on average (lower RMSE)\n")
  } else {
    cat("✓ Random Forest performs BETTER on average (lower RMSE)\n")
  }
  
  cat("  Average RMSE difference:", 
      sprintf("%.2f Mg/ha (%.1f%%)\n",
              mean(comparison$rmse_diff, na.rm = TRUE),
              mean(comparison$rmse_pct_change, na.rm = TRUE)))
}

cat("\nNext step: Create spatial validation plots\n")
cat("  Rscript R/phase4_modeling/PHASE4_04_spatial_plots.R\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")