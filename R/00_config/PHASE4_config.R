# =============================================================================
# PHASE 4 CONFIGURATION - Predictive Modeling (UPDATED & FIXED)
# =============================================================================
# Central config for covariates, models, and settings
# 
# FEATURES:
# - Two spatial scales (10m Sentinel-2 vs 250m MODIS)
# - Spatial 10-fold cross-validation
# - Both regression and classification metrics
# - AUC/ROC analysis for high-biomass classification
#
# FIXED: Coarse scale now uses elevation/slope/aspect (10m) 
# =============================================================================

PHASE4_CONFIG <- list()

# =============================================================================
# SPATIAL SCALE DEFINITIONS
# =============================================================================

PHASE4_CONFIG$spatial_scales <- list(
  
  fine = list(
    name = "Fine Scale (10m)",
    resolution = "10m",
    description = "Fine-scale Sentinel-2 + high-res topography",
    sensor = "Sentinel-2",
    covariates = c(
      # Sentinel-2 spectral (10m)
      "ndvi_s2", "evi_s2", "nbr_s2", "ndwi_s2",
      "red_s2", "nir_s2", "swir1_s2", "ndvi_sd_s2",
      # Topography (10m)
      "elevation", "slope", "aspect", "tpi",
      # Climate (downsampled)
      "tmean", "ppt"
    ),
    test_hypothesis = "Fuzzing (±1.6km) should matter at 10m resolution"
  ),
  
  coarse = list(
    name = "Coarse Scale (250m)",
    resolution = "250m", 
    description = "Coarse-scale MODIS spectral + topography",
    sensor = "MODIS",
    covariates = c(
      # MODIS spectral (250m)
      "ndvi_modis", "evi_modis", "nbr_modis", "ndwi_modis",
      "red_modis", "nir_modis", "swir1_modis",
      # Topography (10m - sampled at plot locations)
      # NOTE: Using 10m DEM for both scales because topography varies slowly
      # and the fuzzing hypothesis is about SPECTRAL resolution, not DEM
      "elevation", "slope", "aspect",
      # Climate
      "tmean", "ppt",
      # Optional LST (1km)
      "lst_day", "lst_night"
    ),
    test_hypothesis = "Fuzzing should NOT matter at 250m resolution"
  )
)

# =============================================================================
# DATASET SCENARIOS
# =============================================================================

PHASE4_CONFIG$scenarios <- list(
  fia_only = list(
    name = "FIA Only",
    description = "Baseline - fuzzed coordinates (±1.6 km)",
    filter = "dataset == 'FIA'",
    color = "#d62728",
    hypothesis = "Represents accuracy WITH coordinate fuzzing"
  ),
  nefin_only = list(
    name = "NEFIN Only", 
    description = "Precise coordinates - test if precision helps",
    filter = "dataset == 'NEFIN'",
    color = "#1f77b4",
    hypothesis = "Tests benefit of precise coordinates vs compositional bias"
  ),
  pooled = list(
    name = "FIA + NEFIN",
    description = "Combined - more data vs heterogeneity tradeoff",
    filter = "TRUE",
    color = "#2ca02c",
    hypothesis = "Tests if pooling benefits > compositional costs"
  )
)

# Total models: 2 scales × 3 scenarios = 6 models

# =============================================================================
# CROSS-VALIDATION SETTINGS
# =============================================================================

PHASE4_CONFIG$cv <- list(
  
  # Use spatial cross-validation (critical for geospatial data!)
  method = "spatial_block",      # Spatial blocking to avoid autocorrelation
  
  n_folds = 10,                  # 10-fold CV as requested
  
  # Spatial blocking parameters
  buffer_km = 10,                # 10 km buffer between train/test
  selection = "random",          # Random assignment of blocks to folds
  
  # Block size
  block_size_km = 25,            # 25 km × 25 km blocks = 625 km²
  
  # Alternative: Use k-means clustering of coordinates
  use_kmeans = FALSE,            # Set TRUE for irregular spatial blocks
  
  seed = 42,                     # Reproducibility
  
  # Evaluation
  save_predictions = TRUE,       # Save predictions for each fold
  spatial_plots = TRUE           # Create spatial validation plots
)

# =============================================================================
# CLASSIFICATION SETTINGS
# =============================================================================

# For AUC/ROC analysis, convert continuous biomass to binary classes
PHASE4_CONFIG$classification <- list(
  
  enabled = FALSE,                # Enable classification analysis
  
  # Define "high biomass" threshold
  threshold = 150,               # Mg/ha (adjust based on your data!)
  
  # Alternative: Use quantiles instead of fixed threshold
  use_quantile = FALSE,          # If TRUE, uses quantile_threshold
  quantile_threshold = 0.75,     # Top 25% = "High biomass"
  
  # Classification metrics to calculate
  metrics = c(
    "AUC",                       # Area Under ROC Curve
    "AUPRC",                     # Area Under Precision-Recall Curve
    "sensitivity",               # True Positive Rate
    "specificity",               # True Negative Rate
    "precision",                 # Positive Predictive Value
    "F1",                        # Harmonic mean of precision & recall
    "kappa"                      # Cohen's Kappa
  ),
  
  # Threshold selection for binary predictions
  threshold_method = "youden",   # "youden", "f1", or "fixed"
  
  # Create classification-specific plots
  create_roc = TRUE,
  create_pr = TRUE,              # Precision-Recall curve
  create_probability_maps = TRUE
)

# =============================================================================
# EVALUATION METRICS
# =============================================================================

PHASE4_CONFIG$metrics <- list(
  
  # Regression metrics (continuous biomass)
  regression = c(
    "RMSE",                      # Root Mean Square Error
    "MAE",                       # Mean Absolute Error  
    "R2",                        # R-squared
    "bias",                      # Mean bias
    "CCC",                       # Concordance Correlation Coefficient
    "MAPE"                       # Mean Absolute Percentage Error
  ),
  
  # Classification metrics (if enabled)
  classification = c(
    "AUC", "AUPRC", "sensitivity", "specificity", 
    "precision", "recall", "F1", "kappa"
  ),
  
  # Spatial metrics
  spatial = c(
    "moran_residuals",           # Moran's I on residuals
    "variogram_range",           # Range of residual variogram
    "spatial_rmse_by_distance"   # How error changes with distance
  )
)

# =============================================================================
# MODEL SETTINGS
# =============================================================================

PHASE4_CONFIG$models <- list(
  
  # Random Forest (primary model)
  rf = list(
    name = "Random Forest",
    enabled = TRUE,
    params = list(
      ntree = 500,               # Number of trees
      mtry = NULL,               # sqrt(n_covariates)
      nodesize = 5,              # Min terminal node size
      importance = TRUE,         # Variable importance
      classwt = NULL             # Class weights (for imbalanced data)
    )
  ),
  
  # XGBoost (gradient boosting)
  xgb = list(
    name = "XGBoost",
    enabled = TRUE,              # ← Enabled for comparison!
    params = list(
      nrounds = 100,             # Number of boosting rounds
      max_depth = 6,             # Maximum tree depth
      eta = 0.3,                 # Learning rate (shrinkage)
      gamma = 0,                 # Minimum loss reduction
      subsample = 1,             # Subsample ratio of training instances
      colsample_bytree = 1,      # Subsample ratio of columns
      min_child_weight = 1,      # Minimum sum of instance weight
      objective = "reg:squarederror",  # Loss function
      eval_metric = "rmse"       # Evaluation metric
    )
  )
)

# =============================================================================
# OUTPUT SETTINGS
# =============================================================================

PHASE4_CONFIG$output <- list(
  
  dir_models = "data/processed/phase4_models",
  dir_predictions = "data/processed/phase4_predictions",
  dir_cv = "data/processed/phase4_cv_results",
  dir_figures = "manuscript_figures/phase4",
  
  # Save detailed CV results for each fold
  save_fold_predictions = TRUE,
  save_fold_models = FALSE,      # Can get large!
  
  # Figure settings
  fig_width = 12,
  fig_height = 8,
  fig_dpi = 300
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Get covariates for a specific scale
get_scale_covariates <- function(scale_name = "fine") {
  if (!scale_name %in% names(PHASE4_CONFIG$spatial_scales)) {
    stop("Invalid scale. Choose 'fine' or 'coarse'")
  }
  
  scale <- PHASE4_CONFIG$spatial_scales[[scale_name]]
  return(scale$covariates)
}

# Get active covariates that exist for a scale
get_active_scale_covariates <- function(scale_name, available_covs) {
  scale_covs <- get_scale_covariates(scale_name)
  return(intersect(scale_covs, available_covs))
}

# Calculate high-biomass threshold
get_biomass_threshold <- function(biomass_values) {
  if (PHASE4_CONFIG$classification$use_quantile) {
    threshold <- quantile(biomass_values, 
                          PHASE4_CONFIG$classification$quantile_threshold,
                          na.rm = TRUE)
  } else {
    threshold <- PHASE4_CONFIG$classification$threshold
  }
  return(threshold)
}

# Create binary classification labels
create_classification_labels <- function(biomass_values, threshold = NULL) {
  if (is.null(threshold)) {
    threshold <- get_biomass_threshold(biomass_values)
  }
  
  labels <- ifelse(biomass_values > threshold, "High", "Low")
  labels <- factor(labels, levels = c("Low", "High"))
  
  return(list(
    labels = labels,
    threshold = threshold
  ))
}

# Print configuration summary
print_phase4_config <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  PHASE 4 CONFIGURATION - SPATIAL SCALE COMPARISON\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  cat("SPATIAL SCALES:\n")
  for (scale_name in names(PHASE4_CONFIG$spatial_scales)) {
    scale <- PHASE4_CONFIG$spatial_scales[[scale_name]]
    cat("  •", scale$name, "(", scale$resolution, ")\n")
    cat("    Covariates:", length(scale$covariates), "\n")
    cat("    Hypothesis:", scale$test_hypothesis, "\n")
  }
  cat("\n")
  
  cat("SCENARIOS (per scale):\n")
  for (scenario_name in names(PHASE4_CONFIG$scenarios)) {
    scenario <- PHASE4_CONFIG$scenarios[[scenario_name]]
    cat("  •", scenario$name, "\n")
    cat("    ", scenario$hypothesis, "\n")
  }
  cat("\n")
  
  cat("TOTAL MODELS:", 
      length(PHASE4_CONFIG$spatial_scales) * length(PHASE4_CONFIG$scenarios), 
      "\n")
  cat("  ", length(PHASE4_CONFIG$spatial_scales), "scales ×", 
      length(PHASE4_CONFIG$scenarios), "scenarios\n\n")
  
  cat("CROSS-VALIDATION:\n")
  cat("  Method:", PHASE4_CONFIG$cv$method, "\n")
  cat("  Folds:", PHASE4_CONFIG$cv$n_folds, "\n")
  cat("  Block size:", PHASE4_CONFIG$cv$block_size_km, "km\n")
  cat("  Buffer:", PHASE4_CONFIG$cv$buffer_km, "km\n\n")
  
  if (PHASE4_CONFIG$classification$enabled) {
    cat("CLASSIFICATION:\n")
    cat("  Enabled: YES\n")
    cat("  Threshold:", PHASE4_CONFIG$classification$threshold, "Mg/ha\n")
    cat("  Metrics:", paste(PHASE4_CONFIG$classification$metrics, 
                            collapse = ", "), "\n\n")
  }
  
  cat("OUTPUT DIRECTORIES:\n")
  cat("  Models:", PHASE4_CONFIG$output$dir_models, "\n")
  cat("  CV Results:", PHASE4_CONFIG$output$dir_cv, "\n")
  cat("  Figures:", PHASE4_CONFIG$output$dir_figures, "\n\n")
  
  cat("═══════════════════════════════════════════════════════════════\n\n")
}

cat("✓ Phase 4 configuration loaded (SPATIAL SCALE VERSION - FIXED)\n")
cat("  Run print_phase4_config() to see summary\n\n")

PHASE4_CONFIG$prediction <- list(
  
  # Which models to use for prediction
  models = list(
    # Use the best performing model from CV
    use_best = TRUE,           # If TRUE, automatically picks best RMSE model
    
    # Or specify models manually
    specific_models = c(
      "rf_fine_fia_only",      # Random Forest, fine scale, FIA only
      "rf_fine_nefin_only",    # Random Forest, fine scale, NEFIN only
      "rf_fine_pooled",        # Random Forest, fine scale, pooled
      "rf_coarse_fia_only",    # Random Forest, coarse scale, FIA only
      "rf_coarse_nefin_only",  # Random Forest, coarse scale, NEFIN only
      "rf_coarse_pooled"       # Random Forest, coarse scale, pooled
    )
  ),
  
  # Prediction extent
  extent = list(
    # Options: "chittenden_county", "vermont", "full_region", "custom"
    type = "chittenden_county",
    
    # County-level prediction
    county = list(
      state = "Vermont",
      county = "Chittenden",
      buffer_km = 5              # Add buffer around county (to avoid edge effects)
    ),
    
    # State-level prediction
    state = list(
      states = c("Vermont"),     # Can add: "New Hampshire", "Maine", etc.
      buffer_km = 0
    ),
    
    # Full region prediction
    full_region = list(
      states = c("Vermont", "New Hampshire", "Maine", 
                 "Massachusetts", "Connecticut", "Rhode Island", "New York"),
      buffer_km = 0
    ),
    
    # Custom extent (bounding box)
    custom = list(
      xmin = -73.5,
      xmax = -71.5,
      ymin = 43.5,
      ymax = 45.0,
      crs = 4326                 # WGS84
    )
  ),
  
  # Output settings
  output = list(
    dir = "data/predictions/phase4",
    
    # Raster resolution (meters)
    resolution = list(
      fine = 30,                 # 30m for fine scale (Sentinel-2 based)
      coarse = 250               # 250m for coarse scale (MODIS based)
    ),
    
    # Output format
    format = "GTiff",            # GeoTIFF
    compress = "LZW",            # Compression
    datatype = "FLT4S",          # Float32
    
    # Create these outputs
    create_difference_maps = TRUE,  # NEFIN - FIA differences
    create_uncertainty_maps = FALSE, # Standard error (requires ensemble)
    create_probability_maps = FALSE  # High biomass probability (if classification enabled)
  ),
  
  # Memory management
  processing = list(
    # Process in chunks to avoid memory issues
    chunk_size = 1000,           # Rows to process at once
    n_cores = 4,                 # Parallel processing cores
    
    # Mask out non-forest areas
    use_forest_mask = TRUE,      # Only predict for forested areas
    forest_mask_path = NULL,     # Path to forest mask raster (optional)
    min_forest_prob = 0.5        # Minimum forest probability threshold
  )
)

# Helper function to get prediction extent
get_prediction_extent <- function() {
  extent_type <- PHASE4_CONFIG$prediction$extent$type
  
  if (extent_type == "chittenden_county") {
    return(PHASE4_CONFIG$prediction$extent$county)
  } else if (extent_type == "vermont") {
    return(PHASE4_CONFIG$prediction$extent$state)
  } else if (extent_type == "full_region") {
    return(PHASE4_CONFIG$prediction$extent$full_region)
  } else if (extent_type == "custom") {
    return(PHASE4_CONFIG$prediction$extent$custom)
  } else {
    stop("Invalid extent type")
  }
}

# Helper function to get best model from CV results
get_best_model <- function(cv_summary_path = "data/processed/phase4_cv_results/cv_summary.csv") {
  if (!file.exists(cv_summary_path)) {
    stop("CV summary not found. Run spatial CV first.")
  }
  
  cv_summary <- read.csv(cv_summary_path)
  
  # Find model with lowest RMSE
  best_idx <- which.min(cv_summary$rmse_mean)
  best_model <- cv_summary$model[best_idx]
  
  cat("Best model:", best_model, "\n")
  cat("  RMSE:", round(cv_summary$rmse_mean[best_idx], 2), "Mg/ha\n")
  cat("  R²:", round(cv_summary$r2_mean[best_idx], 3), "\n")
  
  return(list(
    model_name = best_model,
    model_type = cv_summary$model_type[best_idx],
    scale = cv_summary$scale[best_idx],
    scenario = cv_summary$scenario[best_idx],
    rmse = cv_summary$rmse_mean[best_idx],
    r2 = cv_summary$r2_mean[best_idx]
  ))
}

cat("✓ Prediction configuration added\n")
cat("  Default extent:", PHASE4_CONFIG$prediction$extent$type, "\n")
cat("  Output directory:", PHASE4_CONFIG$prediction$output$dir, "\n\n")