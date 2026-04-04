# =============================================================================
# VALIDATE: Do retrained models predict ~0 for water/urban?
# =============================================================================
# Run AFTER retraining with the fixed PHASE4_02_spatial_cv.R
# =============================================================================

library(readr)
library(dplyr)
library(randomForest)
library(xgboost)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VALIDATE: Water/Urban Prediction Check\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

model_dir <- "data/processed/phase4_models"
test_file <- "data/processed/phase4_modeling/test_data_with_water_urban.csv"

# Load test data
test_data <- read_csv(test_file, show_col_types = FALSE)

water_urban_test <- test_data %>% filter(biomass == 0)
forest_test <- test_data %>% filter(biomass > 0)

cat("Test data composition:\n")
cat("  Water/urban (biomass=0):", nrow(water_urban_test), "\n")
cat("  Forest (biomass>0):     ", nrow(forest_test), "\n\n")

if (nrow(water_urban_test) == 0) {
  stop("No water/urban points in test data! Rerun add_water_urban_to_training.R")
}

# Check each model
model_files <- list.files(model_dir, pattern = "\\.rds$", full.names = TRUE)

cat("Model predictions on water/urban points:\n")
cat(sprintf("  %-50s  %8s  %8s  %8s  %5s\n", "Model", "Mean", "Median", "Max", "N"))
cat("  ", strrep("─", 85), "\n")

results <- list()

for (mf in model_files) {
  
  model_obj <- readRDS(mf)
  model <- model_obj$model
  covs <- model_obj$covariates
  scaling_means <- model_obj$scaling_means
  scaling_sds <- model_obj$scaling_sds
  
  available <- intersect(covs, names(water_urban_test))
  
  if (length(available) < length(covs) * 0.8) {
    cat(sprintf("  %-50s  SKIP (missing covariates)\n", basename(mf)))
    next
  }
  
  # Prepare + impute NAs with min forest value
  X <- water_urban_test[, available]
  
  for (cov in available) {
    na_idx <- is.na(X[[cov]])
    if (any(na_idx)) {
      forest_vals <- forest_test[[cov]]
      X[[cov]][na_idx] <- min(forest_vals, na.rm = TRUE)
    }
  }
  
  X <- as.matrix(X)
  complete_idx <- complete.cases(X)
  
  if (sum(complete_idx) == 0) {
    cat(sprintf("  %-50s  SKIP (all NA)\n", basename(mf)))
    next
  }
  
  X <- X[complete_idx, , drop = FALSE]
  
  # Standardize
  for (cov in available) {
    if (cov %in% names(scaling_means) && cov %in% names(scaling_sds)) {
      X[, cov] <- (X[, cov] - scaling_means[cov]) / scaling_sds[cov]
    }
  }
  
  # Predict
  if (inherits(model, "randomForest")) {
    preds <- predict(model, X)
  } else if (inherits(model, "xgb.Booster")) {
    dmat <- xgb.DMatrix(data = X)
    preds <- predict(model, dmat)
  } else {
    next
  }
  
  preds <- pmax(preds, 0)  # Clamp negatives
  
  status <- ifelse(mean(preds) < 15, "✓ PASS", "✗ FAIL")
  
  cat(sprintf("  %-50s  %7.1f  %7.1f  %7.1f  %5d  %s\n",
              basename(mf), mean(preds), median(preds), max(preds), 
              length(preds), status))
  
  results[[basename(mf)]] <- data.frame(
    model = basename(mf),
    mean_pred = mean(preds),
    median_pred = median(preds),
    max_pred = max(preds),
    n = length(preds)
  )
}

cat("\n")

# Also check forest predictions aren't degraded
cat("Sanity check — forest predictions should still be reasonable:\n")
cat(sprintf("  %-50s  %8s  %8s  %8s\n", "Model", "Mean", "Median", "R²"))
cat("  ", strrep("─", 80), "\n")

for (mf in model_files) {
  
  model_obj <- readRDS(mf)
  model <- model_obj$model
  covs <- model_obj$covariates
  scaling_means <- model_obj$scaling_means
  scaling_sds <- model_obj$scaling_sds
  
  available <- intersect(covs, names(forest_test))
  if (length(available) < length(covs) * 0.8) next
  
  X <- as.matrix(forest_test[complete.cases(forest_test[, available]), available])
  y <- forest_test$biomass[complete.cases(forest_test[, available])]
  
  if (nrow(X) < 10) next
  
  for (cov in available) {
    if (cov %in% names(scaling_means) && cov %in% names(scaling_sds)) {
      X[, cov] <- (X[, cov] - scaling_means[cov]) / scaling_sds[cov]
    }
  }
  
  if (inherits(model, "randomForest")) {
    preds <- predict(model, X)
  } else if (inherits(model, "xgb.Booster")) {
    dmat <- xgb.DMatrix(data = X)
    preds <- predict(model, dmat)
  } else {
    next
  }
  
  ss_res <- sum((y - preds)^2)
  ss_tot <- sum((y - mean(y))^2)
  r2 <- 1 - ss_res / ss_tot
  
  cat(sprintf("  %-50s  %7.1f  %7.1f  %6.3f\n",
              basename(mf), mean(preds), median(preds), r2))
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  INTERPRETATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("  Water/urban predictions:\n")
cat("    PASS: mean < 15 Mg/ha (models learned water/urban = low biomass)\n")
cat("    FAIL: mean > 50 Mg/ha (fix didn't work, check covariates)\n\n")
cat("  Forest predictions:\n")
cat("    R² should be similar to pre-fix values\n")
cat("    Mean should be 80-200 Mg/ha (typical NE forest range)\n\n")
