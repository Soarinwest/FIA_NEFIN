# =============================================================================
# PHASE 4 - STEP 2b: Spatial Cross-Validation (CORRECTED FOR NEW DATA STRUCTURE)
# =============================================================================
# CORRECTED DESIGN:
# - Loads scenario-specific TRAINING files (train_fia_only.csv, etc.)
# - Loads UNIVERSAL TEST file (test_data.csv - same for all models)
# - Runs spatial CV on training data
# - Evaluates final model on universal test set
# - Compares Random Forest vs XGBoost
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(dplyr)
library(readr)
library(randomForest)
library(xgboost)
library(blockCV)
library(sf)
library(pROC)
library(caret)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: SPATIAL CROSS-VALIDATION (CORRECTED)\n")
cat("  Proper train/test split with universal test set\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Print configuration
print_phase4_config()

# Create output directories
dir.create("data/processed/phase4_models", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed/phase4_cv_results", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# DEFINE SCENARIOS AND FILE PATHS
# =============================================================================

scenarios <- list(
  fia_only = list(
    name = "FIA Only",
    train_file = "train_fia_only.csv",
    description = "Train on FIA (fuzzed coords)"
  ),
  nefin_only = list(
    name = "NEFIN Only", 
    train_file = "train_nefin_only.csv",
    description = "Train on NEFIN (precise coords)"
  ),
  pooled = list(
    name = "Pooled",
    train_file = "train_pooled.csv",
    description = "Train on FIA + NEFIN"
  )
)

# Universal test file (same for all scenarios)
test_file <- "test_data.csv"

# Model types to compare
model_types <- c("rf", "xgb")
model_names <- c("Random Forest", "XGBoost")

# =============================================================================
# LOAD UNIVERSAL TEST SET
# =============================================================================

cat("\nStep 1: Loading universal test set...\n")

test_data <- read_csv(
  file.path("data/processed/phase4_modeling", test_file),
  show_col_types = FALSE
)

cat("  ✓ Test set loaded:", nrow(test_data), "plots\n")
cat("    (Same for ALL models)\n\n")

# =============================================================================
# GET COVARIATE LISTS BY SCALE
# =============================================================================

cat("Step 2: Organizing covariates by scale...\n")

# Get active covariates
active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)

# Separate by scale - CREATE ACTUAL COLUMN NAMES WITH SCALE SUFFIX
fine_covs <- sapply(
  Filter(function(x) x$scale == "fine", active_covs), 
  function(x) paste0(x$name, "_", gsub("m", "", x$resolution), "m")
)

coarse_covs <- sapply(
  Filter(function(x) x$scale == "coarse", active_covs), 
  function(x) paste0(x$name, "_", gsub("m", "", x$resolution), "m")
)

cat("  ✓ Fine scale (10m):", length(fine_covs), "covariates\n")
cat("  ✓ Coarse scale (250m):", length(coarse_covs), "covariates\n\n")

# =============================================================================
# DEFINE SCALES
# =============================================================================

scales <- list(
  fine = list(
    name = "Fine Scale (10m)",
    covariates = fine_covs,
    description = "10m resolution - fuzzing should matter"
  ),
  coarse = list(
    name = "Coarse Scale (250m)",
    covariates = coarse_covs,
    description = "250m resolution - fuzzing should NOT matter"
  )
)

# =============================================================================
# INITIALIZE RESULTS STORAGE
# =============================================================================

all_results <- list()
fold_results <- list()
result_counter <- 1
fold_counter <- 1

# =============================================================================
# MAIN LOOP: MODEL TYPE > SCALE > SCENARIO
# =============================================================================

for (model_idx in seq_along(model_types)) {
  
  model_type <- model_types[model_idx]
  model_name <- model_names[model_idx]
  
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  MODEL:", toupper(model_name), "\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  for (scale_name in names(scales)) {
    
    scale <- scales[[scale_name]]
    
    cat("\n═══════════════════════════════════════════════════════════════════\n")
    cat("  ", toupper(scale$name), "\n")
    cat("═══════════════════════════════════════════════════════════════════\n\n")
    
    cat("  Using", length(scale$covariates), "covariates:\n")
    cat("  ", paste(head(scale$covariates, 10), collapse = ", "), "\n")
    if (length(scale$covariates) > 10) {
      cat("   ... (", length(scale$covariates) - 10, "more)\n")
    }
    cat("\n")
    
    # Check which covariates are available in test data
    available_covs <- intersect(scale$covariates, names(test_data))
    
    if (length(available_covs) == 0) {
      cat("  ⚠ WARNING: No covariates available for this scale\n")
      cat("  Skipping...\n\n")
      next
    }
    
    if (length(available_covs) < length(scale$covariates)) {
      missing <- setdiff(scale$covariates, available_covs)
      cat("  ⚠ Missing covariates:", paste(missing, collapse = ", "), "\n")
      cat("  Using", length(available_covs), "available covariates\n\n")
    }
    
    for (scenario_name in names(scenarios)) {
      
      scenario <- scenarios[[scenario_name]]
      
      cat("\n───────────────────────────────────────────────────────────────\n")
      cat("  ", scenario$name, "\n")
      cat("───────────────────────────────────────────────────────────────\n\n")
      
      # -----------------------------------------------------------------------
      # LOAD TRAINING DATA
      # -----------------------------------------------------------------------
      
      train_file_path <- file.path("data/processed/phase4_modeling", scenario$train_file)
      
      if (!file.exists(train_file_path)) {
        cat("  ⚠ WARNING: Training file not found:", scenario$train_file, "\n")
        cat("  Skipping this scenario...\n\n")
        next
      }
      
      train_data <- read_csv(train_file_path, show_col_types = FALSE)
      
      cat("  Training data:", nrow(train_data), "plots\n")
      cat("  Test data:", nrow(test_data), "plots (universal)\n\n")
      
      # -----------------------------------------------------------------------
      # FILTER TO AVAILABLE COVARIATES + COMPLETE CASES + OUTLIERS
      # -----------------------------------------------------------------------
      
      # Select required columns
      required_cols <- c("CN", "biomass", "lon", "lat", available_covs)
      
      train_subset <- train_data %>%
        select(all_of(intersect(required_cols, names(train_data)))) %>%
        filter(!is.na(biomass), biomass > 0)
      
      test_subset <- test_data %>%
        select(all_of(intersect(required_cols, names(test_data)))) %>%
        filter(!is.na(biomass), biomass > 0)
      
      # Filter extreme covariate outliers (likely nodata values)
      # NDVI/EVI/NBR/NDWI should be between -1 and 1
      spectral_indices <- c("ndvi_s2", "evi_s2", "nbr_s2", "ndwi_s2", 
                            "ndvi_modis", "evi_modis", "nbr_modis", "ndwi_modis")
      
      for (cov in intersect(spectral_indices, available_covs)) {
        n_before <- nrow(train_subset)
        train_subset <- train_subset %>%
          filter(!!sym(cov) >= -1, !!sym(cov) <= 1)
        n_after <- nrow(train_subset)
        
        if (n_before > n_after) {
          cat("    Removed", n_before - n_after, "outliers from", cov, "(train)\n")
        }
        
        test_subset <- test_subset %>%
          filter(!!sym(cov) >= -1, !!sym(cov) <= 1)
      }
      
      # Remove remaining incomplete cases
      train_subset <- train_subset %>% filter(complete.cases(.))
      test_subset <- test_subset %>% filter(complete.cases(.))
      
      cat("  After filtering:\n")
      cat("    Train:", nrow(train_subset), "plots\n")
      cat("    Test:", nrow(test_subset), "plots\n\n")
      
      if (nrow(train_subset) < 50 || nrow(test_subset) < 10) {
        cat("  ⚠ WARNING: Insufficient data after filtering\n")
        cat("  Skipping this scenario...\n\n")
        next
      }
      
      # -----------------------------------------------------------------------
      # CREATE SPATIAL BLOCKS FOR CV (ON TRAINING DATA ONLY)
      # -----------------------------------------------------------------------
      
      cat("  Creating spatial blocks for CV...\n")
      
      # Convert to sf object
      train_sf <- st_as_sf(
        train_subset,
        coords = c("lon", "lat"),
        crs = 4326
      )
      
      # Create spatial blocks (suppress verbose output)
      set.seed(PHASE4_CONFIG$cv$seed)
      
      tryCatch({
        # Capture and suppress ALL output from spatial blocking
        spatial_blocks <- suppressWarnings(
          suppressMessages(
            cv_spatial(
              x = train_sf,
              column = "biomass",
              k = PHASE4_CONFIG$cv$n_folds,
              size = PHASE4_CONFIG$cv$block_size_km * 1000,  # Convert to meters
              selection = "random",
              iteration = 100
            )
          )
        )
        
        cat("  ✓ Created", PHASE4_CONFIG$cv$n_folds, "spatial folds\n")
        
        # Show fold sizes
        fold_sizes <- table(spatial_blocks$folds_ids)
        cat("    Fold sizes: ", paste(as.vector(fold_sizes), collapse = ", "), "\n\n")
        
      }, error = function(e) {
        cat("  ✗ ERROR creating spatial blocks:", e$message, "\n")
        cat("  Skipping this scenario...\n\n")
        return(NULL)
      })
      
      if (is.null(spatial_blocks)) next
      
      # -----------------------------------------------------------------------
      # RUN SPATIAL CROSS-VALIDATION
      # -----------------------------------------------------------------------
      
      cat("  Running", PHASE4_CONFIG$cv$n_folds, "-fold spatial CV...\n")
      
      cv_metrics <- list()
      
      for (fold in 1:PHASE4_CONFIG$cv$n_folds) {
        
        # Get fold assignments
        fold_assignment <- spatial_blocks$folds_ids
        
        # Create train/test for this fold
        fold_train <- train_subset[fold_assignment != fold, ]
        fold_test <- train_subset[fold_assignment == fold, ]
        
        if (nrow(fold_test) < 5 || nrow(fold_train) < 20) {
          cat("    Fold", fold, "skipped (insufficient data)\n")
          next
        }
        
        # Standardize covariates WITHIN THIS FOLD
        scaling_means <- sapply(fold_train[, available_covs], mean, na.rm = TRUE)
        scaling_sds <- sapply(fold_train[, available_covs], sd, na.rm = TRUE)
        
        # Standardize
        for (cov in available_covs) {
          fold_train[[cov]] <- (fold_train[[cov]] - scaling_means[cov]) / scaling_sds[cov]
          fold_test[[cov]] <- (fold_test[[cov]] - scaling_means[cov]) / scaling_sds[cov]
        }
        
        # Standardization check removed to reduce verbosity
        # (Standardization is working correctly if no errors occur)
        
        # Prepare data matrices
        X_train <- as.matrix(fold_train[, available_covs])
        y_train <- fold_train$biomass
        X_test <- as.matrix(fold_test[, available_covs])
        y_test <- fold_test$biomass
        
        # Train model
        if (model_type == "rf") {
          model <- randomForest(
            x = X_train,
            y = y_train,
            ntree = 500,
            mtry = max(floor(length(available_covs) / 3), 1),
            nodesize = 5
          )
        } else if (model_type == "xgb") {
          dtrain <- xgb.DMatrix(data = X_train, label = y_train)
          model <- xgb.train(
            data = dtrain,
            nrounds = 100,
            objective = "reg:squarederror",
            max_depth = 6,
            eta = 0.3,
            verbose = 0
          )
        }
        
        # Predict
        if (model_type == "rf") {
          predictions <- predict(model, X_test)
        } else {
          dtest <- xgb.DMatrix(data = X_test)
          predictions <- predict(model, dtest)
        }
        
        # Calculate metrics
        rmse <- sqrt(mean((predictions - y_test)^2))
        mae <- mean(abs(predictions - y_test))
        ss_res <- sum((y_test - predictions)^2)
        ss_tot <- sum((y_test - mean(y_test))^2)
        r2 <- 1 - (ss_res / ss_tot)
        
        cv_metrics[[fold]] <- data.frame(
          fold = fold,
          rmse = rmse,
          mae = mae,
          r2 = r2,
          n_train = nrow(fold_train),
          n_test = nrow(fold_test)
        )
        
        # Store fold-level results
        fold_results[[fold_counter]] <- data.frame(
          model_type = model_type,
          model_name = model_name,
          scale = scale$name,
          scenario = scenario$name,
          fold = fold,
          n_train = nrow(fold_train),
          n_test = nrow(fold_test),
          rmse = rmse,
          mae = mae,
          r2 = r2
        )
        fold_counter <- fold_counter + 1
        
        # Progress indicator (less verbose - every 5 folds)
        if (fold %% 5 == 0) {
          cat("    ", fold, "folds complete...\n")
        }
      }
      
      # Aggregate CV results
      cv_results <- bind_rows(cv_metrics)
      
      cat("\n  ✓", nrow(cv_results), "-fold CV complete\n\n")
      
      cat("  CROSS-VALIDATION RESULTS:\n")
      cat("    RMSE: ", round(mean(cv_results$rmse), 2), "±", 
          round(sd(cv_results$rmse), 2), "Mg/ha\n")
      cat("    R²:   ", round(mean(cv_results$r2), 3), "±", 
          round(sd(cv_results$r2), 3), "\n")
      cat("    MAE:  ", round(mean(cv_results$mae), 2), "±", 
          round(sd(cv_results$mae), 2), "Mg/ha\n\n")
      
      # -----------------------------------------------------------------------
      # TRAIN FINAL MODEL ON ALL TRAINING DATA
      # -----------------------------------------------------------------------
      
      cat("  Training final model on all training data...\n")
      
      # Standardize using full training set statistics
      scaling_means <- sapply(train_subset[, available_covs], mean, na.rm = TRUE)
      scaling_sds <- sapply(train_subset[, available_covs], sd, na.rm = TRUE)
      
      train_scaled <- train_subset
      test_scaled <- test_subset
      
      for (cov in available_covs) {
        train_scaled[[cov]] <- (train_subset[[cov]] - scaling_means[cov]) / scaling_sds[cov]
        test_scaled[[cov]] <- (test_subset[[cov]] - scaling_means[cov]) / scaling_sds[cov]
      }
      
      X_train_full <- as.matrix(train_scaled[, available_covs])
      y_train_full <- train_scaled$biomass
      X_test_full <- as.matrix(test_scaled[, available_covs])
      y_test_full <- test_scaled$biomass
      
      # Train final model
      if (model_type == "rf") {
        final_model <- randomForest(
          x = X_train_full,
          y = y_train_full,
          ntree = 500,
          mtry = max(floor(length(available_covs) / 3), 1),
          nodesize = 5
        )
      } else if (model_type == "xgb") {
        dtrain_full <- xgb.DMatrix(data = X_train_full, label = y_train_full)
        final_model <- xgb.train(
          data = dtrain_full,
          nrounds = 100,
          objective = "reg:squarederror",
          max_depth = 6,
          eta = 0.3,
          verbose = 0
        )
      }
      
      # Predict on universal test set
      if (model_type == "rf") {
        test_predictions <- predict(final_model, X_test_full)
      } else {
        dtest_full <- xgb.DMatrix(data = X_test_full)
        test_predictions <- predict(final_model, dtest_full)
      }
      
      # Calculate test set metrics
      test_rmse <- sqrt(mean((test_predictions - y_test_full)^2))
      test_mae <- mean(abs(test_predictions - y_test_full))
      test_ss_res <- sum((y_test_full - test_predictions)^2)
      test_ss_tot <- sum((y_test_full - mean(y_test_full))^2)
      test_r2 <- 1 - (test_ss_res / test_ss_tot)
      
      cat("  ✓ Final model trained\n\n")
      
      cat("  HOLDOUT TEST SET RESULTS:\n")
      cat("    RMSE: ", round(test_rmse, 2), "Mg/ha\n")
      cat("    R²:   ", round(test_r2, 3), "\n")
      cat("    MAE:  ", round(test_mae, 2), "Mg/ha\n\n")
      
      # -----------------------------------------------------------------------
      # SAVE MODEL
      # -----------------------------------------------------------------------
      
      model_filename <- paste0(
        model_type, "_",
        gsub(" ", "_", tolower(scale$name)), "_",
        gsub(" ", "_", tolower(scenario$name)), 
        ".rds"
      )
      
      saveRDS(
        list(
          model = final_model,
          scaling_means = scaling_means,
          scaling_sds = scaling_sds,
          covariates = available_covs,
          metadata = list(
            model_type = model_type,
            scale = scale$name,
            scenario = scenario$name,
            n_train = nrow(train_subset),
            n_test = nrow(test_subset),
            cv_rmse_mean = mean(cv_results$rmse),
            cv_r2_mean = mean(cv_results$r2),
            test_rmse = test_rmse,
            test_r2 = test_r2
          )
        ),
        file.path("data/processed/phase4_models", model_filename)
      )
      
      cat("  ✓ Model saved:", model_filename, "\n")
      
      # -----------------------------------------------------------------------
      # STORE RESULTS
      # -----------------------------------------------------------------------
      
      all_results[[result_counter]] <- data.frame(
        model_type = model_type,
        model_name = model_name,
        model = paste0(model_type, "_", gsub(" ", "_", tolower(scale$name)), "_", 
                       gsub(" ", "_", tolower(scenario$name))),
        scale = scale$name,
        scenario = scenario$name,
        n_train = nrow(train_subset),
        n_test = nrow(test_subset),
        n_covariates = length(available_covs),
        cv_rmse_mean = mean(cv_results$rmse),
        cv_rmse_sd = sd(cv_results$rmse),
        cv_r2_mean = mean(cv_results$r2),
        cv_r2_sd = sd(cv_results$r2),
        cv_mae_mean = mean(cv_results$mae),
        cv_mae_sd = sd(cv_results$mae),
        test_rmse = test_rmse,
        test_r2 = test_r2,
        test_mae = test_mae
      )
      
      result_counter <- result_counter + 1
    }
  }
}

# =============================================================================
# COMBINE AND SAVE RESULTS
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SAVING RESULTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Combine results
results_df <- bind_rows(all_results)
fold_results_df <- bind_rows(fold_results)

# Save results
write_csv(results_df, "data/processed/phase4_cv_results/cv_summary.csv")
write_csv(fold_results_df, "data/processed/phase4_cv_results/fold_results.csv")

cat("  ✓ Results saved to data/processed/phase4_cv_results/\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SPATIAL CV COMPLETE - SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print(results_df %>% 
        select(model_name, scale, scenario, n_train, n_test, 
               cv_rmse_mean, cv_r2_mean, test_rmse, test_r2))

cat("\n")

# =============================================================================
# MODEL COMPARISON
# =============================================================================

if ("rf" %in% results_df$model_type && "xgb" %in% results_df$model_type) {
  
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  MODEL COMPARISON: Random Forest vs XGBoost\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  comparison <- results_df %>%
    select(scale, scenario, model_type, test_rmse, test_r2) %>%
    tidyr::pivot_wider(
      names_from = model_type,
      values_from = c(test_rmse, test_r2)
    ) %>%
    mutate(
      rmse_diff = test_rmse_xgb - test_rmse_rf,
      rmse_winner = ifelse(test_rmse_rf < test_rmse_xgb, "RF", "XGB"),
      r2_diff = test_r2_rf - test_r2_xgb,
      r2_winner = ifelse(test_r2_rf > test_r2_xgb, "RF", "XGB")
    )
  
  print(comparison)
  
  cat("\n")
  
  # Overall winner
  rf_wins <- sum(comparison$rmse_winner == "RF", na.rm = TRUE)
  xgb_wins <- sum(comparison$rmse_winner == "XGB", na.rm = TRUE)
  
  if (rf_wins > xgb_wins) {
    cat("✓ Random Forest performs BETTER overall (lower RMSE in", rf_wins, "of", nrow(comparison), "scenarios)\n")
  } else if (xgb_wins > rf_wins) {
    cat("✓ XGBoost performs BETTER overall (lower RMSE in", xgb_wins, "of", nrow(comparison), "scenarios)\n")
  } else {
    cat("✓ Models perform EQUALLY (tied)\n")
  }
}

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  NEXT STEP: Generate predictions\n")
cat("  Rscript R/phase4_modeling/PHASE4_03_predict_biomass.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")