# =============================================================================
# PHASE 4: DUAL-SCALE BIOMASS PREDICTION - COVARIATE NAME FIX
# =============================================================================
# Fixes covariate name mismatch by adding resolution suffix
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")
source("R/00_config/PHASE4_config_covariates_PREPROCESSED.R")

# Fix PostgreSQL PROJ interference
Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")
cat("✓ Cleared PostgreSQL PROJ paths\n\n")

library(terra)
library(dplyr)
library(readr)
library(randomForest)
library(xgboost)
library(sf)

# =============================================================================
# MODEL SELECTION - EDIT THESE!
# =============================================================================

FINE_MODEL <- "rf_fine_scale_(10m)_pooled"      
COARSE_MODEL <- "rf_coarse_scale_(250m)_pooled"

MODEL_DIR <- "data/processed/phase4_models"

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: DUAL-SCALE BIOMASS PREDICTION\n")
cat("  Comparing Fine (10m) vs Coarse (250m) Resolution\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Model Selection:\n")
cat("  Fine model:", FINE_MODEL, "\n")
cat("  Coarse model:", COARSE_MODEL, "\n\n")

# Check models exist
fine_model_path <- file.path(MODEL_DIR, paste0(FINE_MODEL, ".rds"))
coarse_model_path <- file.path(MODEL_DIR, paste0(COARSE_MODEL, ".rds"))

if (!file.exists(fine_model_path)) {
  cat("✗ Fine model not found:", fine_model_path, "\n\n")
  cat("Available models:\n")
  models <- list.files(MODEL_DIR, pattern = "\\.rds$", full.names = FALSE)
  if (length(models) == 0) {
    cat("  (none)\n\n")
  } else {
    for (m in models) cat("  •", m, "\n")
  }
  stop("Required models not found")
}

if (!file.exists(coarse_model_path)) {
  cat("✗ Coarse model not found:", coarse_model_path, "\n\n")
  stop("Required models not found")
}

cat("✓ Both models found\n\n")

# Create output directory
dir.create(PHASE4_CONFIG$prediction$output$dir, 
           showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# STEP 1: DEFINE PREDICTION EXTENT
# =============================================================================

cat("Step 1: Defining prediction extent...\n")

extent_type <- PHASE4_CONFIG$prediction$extent$type
cat("  Extent type:", extent_type, "\n")

if (extent_type == "chittenden_county") {
  
  cat("  County: Chittenden, Vermont\n")
  
  if (require(tigris, quietly = TRUE)) {
    county_boundary <- counties(state = "VT", cb = TRUE) %>%
      filter(NAME == "Chittenden") %>%
      st_transform(4326)
    
    buffer_km <- PHASE4_CONFIG$prediction$extent$county$buffer_km
    if (buffer_km > 0) {
      county_boundary <- county_boundary %>%
        st_transform(5070) %>%
        st_buffer(buffer_km * 1000) %>%
        st_transform(4326)
      cat("  Buffer:", buffer_km, "km\n")
    }
  } else {
    cat("  Using manual bounding box\n")
    bbox <- c(xmin = -73.3, xmax = -72.9, ymin = 44.35, ymax = 44.65)
    county_boundary <- st_as_sfc(st_bbox(bbox, crs = 4326))
  }
  
  prediction_extent <- county_boundary
  
} else if (extent_type == "vermont") {
  
  cat("  State: Vermont\n")
  
  if (require(tigris, quietly = TRUE)) {
    state_boundary <- states(cb = TRUE) %>%
      filter(NAME == "Vermont") %>%
      st_transform(4326)
    prediction_extent <- state_boundary
  } else {
    bbox <- c(xmin = -73.5, xmax = -71.5, ymin = 42.7, ymax = 45.0)
    prediction_extent <- st_as_sfc(st_bbox(bbox, crs = 4326))
  }
  
} else if (extent_type == "full_region") {
  
  cat("  Full northeastern region\n")
  bbox <- c(xmin = -74.5, xmax = -66.5, ymin = 41.0, ymax = 47.5)
  prediction_extent <- st_as_sfc(st_bbox(bbox, crs = 4326))
  
} else if (extent_type == "custom") {
  
  cat("  Custom bounding box\n")
  custom <- PHASE4_CONFIG$prediction$extent$custom
  bbox <- c(xmin = custom$xmin, xmax = custom$xmax, 
            ymin = custom$ymin, ymax = custom$ymax)
  prediction_extent <- st_as_sfc(st_bbox(bbox, crs = custom$crs))
}

cat("  ✓ Extent defined\n\n")

# =============================================================================
# HELPER FUNCTION: Load covariates for a specific scale
# =============================================================================

load_scale_covariates <- function(scale, extent_vect) {
  
  cat("\nLoading", scale, "scale covariates...\n")
  
  # Get covariates for this scale
  active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)
  scale_covs <- Filter(function(x) x$scale == scale, active_covs)
  
  cat("  Found", length(scale_covs), scale, "scale covariates\n")
  
  # Determine resolution suffix
  if (scale == "fine") {
    resolution_suffix <- "_10m"
  } else {
    resolution_suffix <- "_250m"
  }
  
  # Storage
  covariate_rasters <- list()
  target_crs <- NULL
  extent_transformed <- NULL
  template_extent <- NULL
  template_res <- NULL
  template_crs <- NULL
  
  for (cov_key in names(scale_covs)) {
    cov_info <- scale_covs[[cov_key]]
    
    if (is.null(cov_info$path) || !is.character(cov_info$path)) {
      next
    }
    
    if (!file.exists(cov_info$path)) {
      cat("    ⚠ File not found:", cov_info$display_name, "\n")
      next
    }
    
    cat("  Loading", cov_info$display_name, "...")
    
    tryCatch({
      r <- rast(cov_info$path)
      
      # Use first raster's CRS as target for this scale
      if (is.null(target_crs)) {
        target_crs <- crs(r)
        extent_transformed <- project(extent_vect, target_crs)
        cat(" [template]")
      }
      
      # Crop to extent
      r_cropped <- crop(r, extent_transformed)
      
      # Ensure exact extent match for stacking (within this scale)
      if (is.null(template_extent)) {
        template_extent <- ext(r_cropped)
        template_res <- res(r_cropped)
        template_crs <- crs(r_cropped)
      } else {
        # Resample if needed to match template
        if (!identical(ext(r_cropped), template_extent)) {
          template_r <- rast(extent = template_extent, 
                             resolution = template_res,
                             crs = template_crs)
          r_cropped <- resample(r_cropped, template_r, method = "bilinear")
          cat(" [aligned]")
        }
      }
      
      # CRITICAL FIX: Store with name INCLUDING resolution suffix
      # This matches what the model expects (e.g., "canopy_height_10m")
      covariate_name_with_suffix <- paste0(cov_info$name, resolution_suffix)
      covariate_rasters[[covariate_name_with_suffix]] <- r_cropped
      
      cat(" ✓\n")
      
    }, error = function(e) {
      cat(" ✗ Error:", e$message, "\n")
    })
  }
  
  cat("  ✓ Loaded", length(covariate_rasters), scale, "scale covariates\n\n")
  
  return(covariate_rasters)
}

# =============================================================================
# STEP 2: PREPARE EXTENT
# =============================================================================

cat("Step 2: Preparing extent...\n")

# Convert to terra format
extent_vect <- vect(prediction_extent)

cat("  Extent bounding box (WGS84):\n")
cat("    xmin:", round(ext(extent_vect)[1], 2), "xmax:", round(ext(extent_vect)[2], 2), "\n")
cat("    ymin:", round(ext(extent_vect)[3], 2), "ymax:", round(ext(extent_vect)[4], 2), "\n")

cat("  ✓ Extent ready\n")

# =============================================================================
# STEP 3: LOAD MODELS AND PREDICT
# =============================================================================

cat("\nStep 3: Loading models and predicting...\n")

predictions <- list()

# Define models to run
models_to_run <- list(
  fine = list(
    name = "Fine Scale (10m)",
    model_file = FINE_MODEL,
    scale = "fine"
  ),
  coarse = list(
    name = "Coarse Scale (250m)",
    model_file = COARSE_MODEL,
    scale = "coarse"
  )
)

for (scale_name in c("fine", "coarse")) {
  
  model_info <- models_to_run[[scale_name]]
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat(" ", model_info$name, "\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  # Load covariates for this scale
  scale_covariates <- load_scale_covariates(model_info$scale, extent_vect)
  
  if (length(scale_covariates) == 0) {
    cat("  ✗ No covariates loaded - skipping\n\n")
    next
  }
  
  # Load model
  cat("  Loading model:", model_info$model_file, "\n")
  
  model_path <- file.path(MODEL_DIR, paste0(model_info$model_file, ".rds"))
  model_obj <- readRDS(model_path)
  
  model <- model_obj$model
  scaling_means <- model_obj$scaling_means
  scaling_sds <- model_obj$scaling_sds
  model_covs <- model_obj$covariates
  
  cat("  ✓ Model loaded\n")
  cat("    Model requires:", length(model_covs), "covariates\n")
  
  # Check available covariates
  available_covs <- intersect(model_covs, names(scale_covariates))
  
  cat("    Loaded rasters have:", length(scale_covariates), "covariates\n")
  cat("    Matching covariates:", length(available_covs), "\n")
  
  if (length(available_covs) < length(model_covs)) {
    missing <- setdiff(model_covs, names(scale_covariates))
    cat("  ⚠ Missing", length(missing), "covariates:", 
        paste(head(missing, 3), collapse = ", "), 
        if(length(missing) > 3) "..." else "", "\n")
  }
  
  cat("  Using", length(available_covs), "covariates\n\n")
  
  if (length(available_covs) == 0) {
    cat("  ✗ No matching covariates - cannot predict\n\n")
    next
  }
  
  # Stack covariates
  cat("  Stacking covariates...\n")
  
  # Ensure all extents match
  ref_ext <- ext(scale_covariates[[available_covs[1]]])
  for (cov in available_covs[-1]) {
    if (!identical(ext(scale_covariates[[cov]]), ref_ext)) {
      scale_covariates[[cov]] <- resample(
        scale_covariates[[cov]], 
        scale_covariates[[available_covs[1]]], 
        method = "bilinear"
      )
    }
  }
  
  cov_stack <- rast(scale_covariates[available_covs])
  names(cov_stack) <- available_covs
  
  cat("  ✓ Stacked successfully\n")
  cat("    Resolution:", round(res(cov_stack)[1]), "m\n")
  cat("    Dimensions:", paste(dim(cov_stack)[1:2], collapse = " x "), "pixels\n\n")
  
  # Standardize
  cat("  Standardizing covariates...\n")
  for (cov in available_covs) {
    if (cov %in% names(scaling_means) && cov %in% names(scaling_sds)) {
      cov_stack[[cov]] <- (cov_stack[[cov]] - scaling_means[cov]) / scaling_sds[cov]
    }
  }
  cat("  ✓ Standardized\n\n")
  
  # Predict
  cat("  Predicting biomass...\n")
  
  if ("randomForest" %in% class(model)) {
    biomass_pred <- predict(cov_stack, model, type = "response", na.rm = TRUE)
  } else {
    biomass_pred <- predict(cov_stack, model, na.rm = TRUE)
  }
  
  biomass_pred <- ifel(biomass_pred < 0, 0, biomass_pred)
  
  # Post-prediction water/urban mask (safety net)
  # NDVI < 0.15 reliably identifies non-vegetated surfaces
  ndvi_name <- grep("ndvi", available_covs, value = TRUE)[1]
  if (!is.null(ndvi_name) && ndvi_name %in% names(cov_stack)) {
    cat("  Applying water/urban mask (", ndvi_name, " < 0.15)...\n")
    ndvi_layer <- cov_stack[[ndvi_name]]
    if (ndvi_name %in% names(scaling_means) && ndvi_name %in% names(scaling_sds)) {
      ndvi_raw <- ndvi_layer * scaling_sds[ndvi_name] + scaling_means[ndvi_name]
    } else {
      ndvi_raw <- ndvi_layer
    }
    n_masked <- global(ndvi_raw < 0.15 & !is.na(ndvi_raw), "sum", na.rm = TRUE)[[1]]
    n_total <- global(!is.na(biomass_pred), "sum", na.rm = TRUE)[[1]]
    biomass_pred <- ifel(ndvi_raw < 0.15, 0, biomass_pred)
    cat("  ✓ Masked", n_masked, "of", n_total, "pixels",
        "(", round(n_masked / n_total * 100, 1), "%) to 0 Mg/ha\n")
  }
  
  names(biomass_pred) <- "biomass"
  
  cat("  ✓ Prediction complete\n")
  cat("    Mean biomass:", round(global(biomass_pred, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Min:", round(global(biomass_pred, "min", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Max:", round(global(biomass_pred, "max", na.rm = TRUE)[[1]], 2), "Mg/ha\n\n")
  
  # Save
  output_file <- file.path(
    PHASE4_CONFIG$prediction$output$dir,
    paste0("biomass_", scale_name, "_", gsub("[()]", "", model_info$model_file), ".tif")
  )
  
  cat("  Saving:", basename(output_file), "\n")
  writeRaster(biomass_pred, output_file, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  cat("  ✓ Saved\n\n")
  
  predictions[[scale_name]] <- biomass_pred
}

# =============================================================================
# STEP 4: CREATE COMPARISON MAPS
# =============================================================================

if (length(predictions) == 2) {
  
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  CREATING COMPARISON MAPS\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("  Resampling coarse to fine resolution...\n")
  coarse_resampled <- resample(predictions$coarse, predictions$fine, method = "bilinear")
  
  cat("  Calculating difference (Fine - Coarse)...\n")
  difference <- predictions$fine - coarse_resampled
  names(difference) <- "difference"
  
  diff_file <- file.path(
    PHASE4_CONFIG$prediction$output$dir,
    paste0("biomass_difference_", gsub("[()]", "", FINE_MODEL), "_vs_", gsub("[()]", "", COARSE_MODEL), ".tif")
  )
  
  cat("  Saving difference map...\n")
  writeRaster(difference, diff_file, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  abs_diff <- abs(difference)
  names(abs_diff) <- "abs_difference"
  
  abs_diff_file <- file.path(
    PHASE4_CONFIG$prediction$output$dir,
    paste0("biomass_abs_difference_", gsub("[()]", "", FINE_MODEL), "_vs_", gsub("[()]", "", COARSE_MODEL), ".tif")
  )
  
  cat("  Saving absolute difference map...\n")
  writeRaster(abs_diff, abs_diff_file, overwrite = TRUE,
              gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  cat("\n  Difference Statistics (Fine - Coarse):\n")
  cat("    Mean difference:", round(global(difference, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    SD:", round(global(difference, "sd", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Min:", round(global(difference, "min", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Max:", round(global(difference, "max", na.rm = TRUE)[[1]], 2), "Mg/ha\n")
  cat("    Mean absolute difference:", round(global(abs_diff, "mean", na.rm = TRUE)[[1]], 2), "Mg/ha\n\n")
  
  cat("  ✓ Comparison maps created\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PREDICTION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Models used:\n")
cat("  Fine:", FINE_MODEL, "\n")
cat("  Coarse:", COARSE_MODEL, "\n\n")

cat("Output directory:", PHASE4_CONFIG$prediction$output$dir, "\n\n")