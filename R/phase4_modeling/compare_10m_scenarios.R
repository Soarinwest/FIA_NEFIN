# =============================================================================
# COMPARE DIFFERENT SCENARIOS AT 10M RESOLUTION
# =============================================================================
# Compare FIA only, NEFIN only, and Pooled models at fine scale
# Shows how coordinate fuzzing affects predictions
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")
source("R/00_config/PHASE4_config_covariates_PREPROCESSED.R")

Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")

library(terra)
library(dplyr)
library(randomForest)
library(sf)

# =============================================================================
# SETTINGS
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  COMPARE 10M PREDICTIONS: FIA vs NEFIN vs POOLED\n")
cat("  Testing coordinate fuzzing effects at fine scale\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Models to compare (all at 10m resolution)
MODELS <- c(
  "rf_fine_scale_(10m)_fia_only",
  "rf_fine_scale_(10m)_nefin_only",
  "rf_fine_scale_(10m)_pooled"
)

MODEL_DIR <- "data/processed/phase4_models"
OUTPUT_DIR <- "data/predictions/phase4/scenario_comparison"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# DEFINE EXTENT (Same as main predictions)
# =============================================================================

cat("Defining prediction extent...\n")

extent_type <- PHASE4_CONFIG$prediction$extent$type

if (extent_type == "chittenden_county") {
  if (require(tigris, quietly = TRUE)) {
    county_boundary <- counties(state = "VT", cb = TRUE) %>%
      filter(NAME == "Chittenden") %>%
      st_transform(4326)
  } else {
    bbox <- c(xmin = -73.3, xmax = -72.9, ymin = 44.35, ymax = 44.65)
    county_boundary <- st_as_sfc(st_bbox(bbox, crs = 4326))
  }
  prediction_extent <- county_boundary
}

extent_vect <- vect(prediction_extent)

cat("  ✓ Extent defined\n\n")

# =============================================================================
# LOAD FINE SCALE COVARIATES (ONCE)
# =============================================================================

cat("Loading 10m covariates...\n")

# Get fine scale covariates
active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)
fine_covs <- Filter(function(x) x$scale == "fine", active_covs)

# Storage
covariate_rasters <- list()
target_crs <- NULL
extent_transformed <- NULL

for (cov_key in names(fine_covs)) {
  cov_info <- fine_covs[[cov_key]]
  
  if (!file.exists(cov_info$path)) next
  
  r <- rast(cov_info$path)
  
  if (is.null(target_crs)) {
    target_crs <- crs(r)
    extent_transformed <- project(extent_vect, target_crs)
  }
  
  r_cropped <- crop(r, extent_transformed)
  
  # Store with resolution suffix
  covariate_name <- paste0(cov_info$name, "_10m")
  covariate_rasters[[covariate_name]] <- r_cropped
}

cat("  ✓ Loaded", length(covariate_rasters), "covariates\n\n")

# =============================================================================
# PREDICT WITH EACH MODEL
# =============================================================================

predictions <- list()

for (model_name in MODELS) {
  
  scenario <- gsub("rf_fine_scale_\\(10m\\)_", "", model_name)
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("  Scenario:", toupper(scenario), "\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Load model
  model_path <- file.path(MODEL_DIR, paste0(model_name, ".rds"))
  
  if (!file.exists(model_path)) {
    cat("  ✗ Model not found:", model_path, "\n")
    cat("  Skipping...\n")
    next
  }
  
  cat("  Loading model...\n")
  model_obj <- readRDS(model_path)
  
  model <- model_obj$model
  scaling_means <- model_obj$scaling_means
  scaling_sds <- model_obj$scaling_sds
  model_covs <- model_obj$covariates
  
  # Match covariates
  available_covs <- intersect(model_covs, names(covariate_rasters))
  
  cat("  Using", length(available_covs), "of", length(model_covs), "covariates\n")
  
  # Stack
  cat("  Stacking covariates...\n")
  cov_stack <- rast(covariate_rasters[available_covs])
  names(cov_stack) <- available_covs
  
  # Standardize
  cat("  Standardizing...\n")
  for (cov in available_covs) {
    cov_stack[[cov]] <- (cov_stack[[cov]] - scaling_means[cov]) / scaling_sds[cov]
  }
  
  # Predict
  cat("  Predicting...\n")
  biomass_pred <- predict(cov_stack, model, type = "response", na.rm = TRUE)
  biomass_pred <- ifel(biomass_pred < 0, 0, biomass_pred)
  names(biomass_pred) <- "biomass"
  
  cat("  ✓ Prediction complete\n")
  cat("    Mean:", round(global(biomass_pred, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Min:", round(global(biomass_pred, "min", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Max:", round(global(biomass_pred, "max", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  
  # Save
  output_file <- file.path(OUTPUT_DIR, paste0("biomass_10m_", scenario, ".tif"))
  writeRaster(biomass_pred, output_file, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  cat("  ✓ Saved:", basename(output_file), "\n")
  
  predictions[[scenario]] <- biomass_pred
}

# Mask non-forest pixels (biomass = 0) to NA for forest-only comparison
# The saved TIFs retain 0s — this only affects the difference maps and stats
cat("\n  Masking non-forest pixels (biomass = 0 → NA) for comparison...\n")
for (name in names(predictions)) {
  n_zero <- global(predictions[[name]] == 0 & !is.na(predictions[[name]]), "sum", na.rm = TRUE)[[1]]
  predictions[[name]] <- ifel(predictions[[name]] <= 0, NA, predictions[[name]])
  cat("    ", name, ":", n_zero, "pixels masked\n")
}
cat("\n")

# =============================================================================
# CREATE DIFFERENCE MAPS
# =============================================================================

if (length(predictions) >= 2) {
  
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  CREATING DIFFERENCE MAPS\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  # FIA vs NEFIN
  if ("fia_only" %in% names(predictions) && "nefin_only" %in% names(predictions)) {
    
    cat("  FIA vs NEFIN difference...\n")
    diff_fia_nefin <- predictions$fia_only - predictions$nefin_only
    names(diff_fia_nefin) <- "difference"
    
    writeRaster(
      diff_fia_nefin,
      file.path(OUTPUT_DIR, "biomass_10m_difference_fia_vs_nefin.tif"),
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "TILED=YES")
    )
    
    cat("    Mean difference:", round(global(diff_fia_nefin, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
    cat("    SD:", round(global(diff_fia_nefin, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  }
  
  # Pooled vs FIA
  if ("pooled" %in% names(predictions) && "fia_only" %in% names(predictions)) {
    
    cat("\n  Pooled vs FIA difference...\n")
    diff_pooled_fia <- predictions$pooled - predictions$fia_only
    names(diff_pooled_fia) <- "difference"
    
    writeRaster(
      diff_pooled_fia,
      file.path(OUTPUT_DIR, "biomass_10m_difference_pooled_vs_fia.tif"),
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "TILED=YES")
    )
    
    cat("    Mean difference:", round(global(diff_pooled_fia, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
    cat("    SD:", round(global(diff_pooled_fia, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  }
  
  # Pooled vs NEFIN
  if ("pooled" %in% names(predictions) && "nefin_only" %in% names(predictions)) {
    
    cat("\n  Pooled vs NEFIN difference...\n")
    diff_pooled_nefin <- predictions$pooled - predictions$nefin_only
    names(diff_pooled_nefin) <- "difference"
    
    writeRaster(
      diff_pooled_nefin,
      file.path(OUTPUT_DIR, "biomass_10m_difference_pooled_vs_nefin.tif"),
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE", "TILED=YES")
    )
    
    cat("    Mean difference:", round(global(diff_pooled_nefin, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
    cat("    SD:", round(global(diff_pooled_nefin, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  }
  
  cat("\n  ✓ Difference maps created\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SCENARIO COMPARISON COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Predictions created for:\n")
for (scenario in names(predictions)) {
  cat("  •", scenario, "\n")
}

cat("\nOutput directory:", OUTPUT_DIR, "\n")

cat("\nNext step: Visualize with:\n")
cat("  Rscript R/phase4_modeling/visualize_scenario_comparison.R\n\n")
