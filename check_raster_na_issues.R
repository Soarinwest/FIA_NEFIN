# =============================================================================
# CHECK RASTERS FOR NA/NaN ISSUES
# =============================================================================
# Diagnoses NA/NaN problems that could break predictions
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")
source("R/00_config/PHASE4_config_covariates_PREPROCESSED.R")

library(terra)
library(dplyr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  CHECKING RASTERS FOR NA/NaN ISSUES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Prediction extent (Chittenden County)
bbox <- c(xmin = -73.3, xmax = -72.9, ymin = 44.35, ymax = 44.65)
prediction_extent <- ext(bbox)

cat("Prediction extent (Chittenden County):\n")
cat("  xmin:", bbox["xmin"], "xmax:", bbox["xmax"], "\n")
cat("  ymin:", bbox["ymin"], "ymax:", bbox["ymax"], "\n\n")

# Get active covariates
active_covs <- get_active_covariates()

cat("Checking", length(active_covs), "active covariates...\n\n")

# Results storage
results <- data.frame(
  covariate = character(),
  scale = character(),
  can_open = logical(),
  can_crop = logical(),
  pct_na_full = numeric(),
  pct_na_crop = numeric(),
  has_nan = logical(),
  has_inf = logical(),
  value_range = character(),
  issue = character(),
  stringsAsFactors = FALSE
)

# ===========================================================================
# CHECK EACH RASTER
# ===========================================================================

for (cov_key in names(active_covs)) {
  cov <- active_covs[[cov_key]]
  
  cat("Checking:", cov$display_name, "(", cov$resolution, ")...\n")
  
  result <- list(
    covariate = cov$display_name,
    scale = cov$resolution,
    can_open = FALSE,
    can_crop = FALSE,
    pct_na_full = NA,
    pct_na_crop = NA,
    has_nan = FALSE,
    has_inf = FALSE,
    value_range = "unknown",
    issue = "none"
  )
  
  # Skip if path invalid
  if (is.null(cov$path) || !is.character(cov$path)) {
    result$issue <- "invalid_path"
    results <- rbind(results, as.data.frame(result))
    cat("  ✗ Invalid path\n\n")
    next
  }
  
  if (!file.exists(cov$path)) {
    result$issue <- "file_not_found"
    results <- rbind(results, as.data.frame(result))
    cat("  ✗ File not found\n\n")
    next
  }
  
  # Try to open
  r <- NULL
  tryCatch({
    r <- rast(cov$path)
    result$can_open <- TRUE
    cat("  ✓ Opened\n")
  }, error = function(e) {
    result$issue <- paste0("cannot_open: ", e$message)
    cat("  ✗ Cannot open:", e$message, "\n\n")
  })
  
  if (!result$can_open) {
    results <- rbind(results, as.data.frame(result))
    next
  }
  
  # Try to crop to prediction extent
  r_crop <- NULL
  tryCatch({
    r_crop <- crop(r, prediction_extent)
    result$can_crop <- TRUE
    cat("  ✓ Cropped to extent\n")
  }, error = function(e) {
    result$issue <- paste0("cannot_crop: ", e$message)
    cat("  ✗ Cannot crop:", e$message, "\n\n")
  })
  
  if (!result$can_crop) {
    results <- rbind(results, as.data.frame(result))
    next
  }
  
  # Sample values from full raster (for speed)
  cat("  Sampling values...\n")
  tryCatch({
    # Sample 10,000 cells from full raster
    n_cells <- ncell(r)
    sample_size <- min(10000, n_cells)
    set.seed(42)
    sample_idx <- sample(1:n_cells, sample_size)
    sample_vals <- r[sample_idx]
    
    # Check for NAs
    n_na <- sum(is.na(sample_vals))
    result$pct_na_full <- round(100 * n_na / sample_size, 2)
    
    # Check for NaN (different from NA)
    if (any(!is.na(sample_vals))) {
      result$has_nan <- any(is.nan(sample_vals[!is.na(sample_vals)]))
    }
    
    # Check for Inf
    if (any(!is.na(sample_vals))) {
      result$has_inf <- any(is.infinite(sample_vals[!is.na(sample_vals)]))
    }
    
    # Value range (excluding NA/NaN/Inf)
    valid_vals <- sample_vals[is.finite(sample_vals)]
    if (length(valid_vals) > 0) {
      result$value_range <- paste(round(range(valid_vals), 3), collapse = " to ")
    }
    
    cat("    Full raster NAs:", result$pct_na_full, "%\n")
    
  }, error = function(e) {
    cat("    ⚠ Could not sample full raster:", e$message, "\n")
  })
  
  # Check cropped area specifically
  cat("  Checking cropped area...\n")
  tryCatch({
    # Get all values from cropped area
    crop_vals <- values(r_crop)
    
    n_na_crop <- sum(is.na(crop_vals))
    n_total_crop <- length(crop_vals)
    result$pct_na_crop <- round(100 * n_na_crop / n_total_crop, 2)
    
    cat("    Cropped area NAs:", result$pct_na_crop, "%\n")
    
    # Check if cropped area is ALL NA
    if (result$pct_na_crop == 100) {
      result$issue <- "crop_all_na"
      cat("    ✗ WARNING: Cropped area is 100% NA!\n")
    } else if (result$pct_na_crop > 90) {
      result$issue <- "crop_mostly_na"
      cat("    ⚠ WARNING: Cropped area is >90% NA\n")
    }
    
  }, error = function(e) {
    cat("    ⚠ Could not check cropped area:", e$message, "\n")
  })
  
  # Check for NaN/Inf issues
  if (result$has_nan) {
    result$issue <- "has_nan"
    cat("    ⚠ WARNING: Contains NaN values!\n")
  }
  
  if (result$has_inf) {
    result$issue <- "has_inf"
    cat("    ⚠ WARNING: Contains Inf values!\n")
  }
  
  results <- rbind(results, as.data.frame(result))
  cat("\n")
}

# ===========================================================================
# SUMMARY REPORT
# ===========================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY REPORT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Total covariates checked:", nrow(results), "\n\n")

# Count issues
n_cannot_open <- sum(!results$can_open)
n_cannot_crop <- sum(results$can_open & !results$can_crop)
n_crop_all_na <- sum(results$issue == "crop_all_na", na.rm = TRUE)
n_crop_mostly_na <- sum(results$issue == "crop_mostly_na", na.rm = TRUE)
n_has_nan <- sum(results$has_nan, na.rm = TRUE)
n_has_inf <- sum(results$has_inf, na.rm = TRUE)

cat("Issues found:\n")
cat("  Cannot open:", n_cannot_open, "\n")
cat("  Cannot crop:", n_cannot_crop, "\n")
cat("  Crop 100% NA:", n_crop_all_na, "\n")
cat("  Crop >90% NA:", n_crop_mostly_na, "\n")
cat("  Has NaN:", n_has_nan, "\n")
cat("  Has Inf:", n_has_inf, "\n\n")

# Show problem rasters
problems <- results[results$issue != "none", ]

if (nrow(problems) > 0) {
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PROBLEM RASTERS\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  for (i in 1:nrow(problems)) {
    cat(sprintf("%d. %s (%s)\n", i, problems$covariate[i], problems$scale[i]))
    cat("   Issue:", problems$issue[i], "\n")
    if (!is.na(problems$pct_na_crop[i])) {
      cat("   NAs in crop area:", problems$pct_na_crop[i], "%\n")
    }
    cat("\n")
  }
  
  cat("RECOMMENDATIONS:\n")
  cat("  1. Cannot open → File is corrupted, delete and re-preprocess\n")
  cat("  2. Crop 100% NA → Check if extent overlaps raster coverage\n")
  cat("  3. Has NaN/Inf → May need to clean raster values\n\n")
  
} else {
  cat("✓ All rasters look good!\n\n")
  cat("NAs in rasters are normal and predictions should handle them fine.\n")
  cat("Your NDWI error is likely file corruption, not NA issues.\n\n")
}

# Save detailed results
write.csv(results, "raster_na_check_results.csv", row.names = FALSE)
cat("Detailed results saved to: raster_na_check_results.csv\n\n")

# ===========================================================================
# SPECIFIC CHECK: NDWI FILE
# ===========================================================================

ndwi_result <- results[grep("NDWI.*Sentinel-2", results$covariate), ]

if (nrow(ndwi_result) > 0) {
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  SPECIFIC CHECK: NDWI (The Problematic File)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("Covariate:", ndwi_result$covariate[1], "\n")
  cat("Can open?", ifelse(ndwi_result$can_open[1], "YES", "NO"), "\n")
  cat("Issue:", ndwi_result$issue[1], "\n\n")
  
  if (!ndwi_result$can_open[1]) {
    cat("VERDICT: NDWI file is CORRUPTED (cannot even open)\n")
    cat("ACTION: Delete and re-preprocess\n\n")
    cat("Command:\n")
    cat("  file.remove('D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed/S2_NDWI_10m_2020_2024.tif')\n")
    cat("  # Then: Rscript R/phase4_modeling/PHASE4_00_preprocess_rasters.R\n\n")
  } else {
    cat("VERDICT: NDWI file opens fine\n")
    cat("The prediction error might be from something else.\n\n")
  }
}
