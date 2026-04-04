# =============================================================================
# PHASE 4: PREPROCESS COVARIATE RASTERS (TEMPLATE-ALIGNED)
# =============================================================================
# Aligns all covariates to user-provided template grids in EPSG:5070
# Clips to study region AOI shapefile
# Verifies every output to catch corruption
#
# KEY FEATURES:
# - Uses YOUR template grids (guarantees exact grid match)
# - EPSG:5070 (NAD83 Albers Equal Area - meters)
# - Clips to AOI shapefile (PHASE4_CONFIG$paths$aoi — set EXTERNAL_DATA_ROOT in PHASE4_config.R)
# - Verifies every output immediately after writing
# - Deletes corrupt files and stops
# - Never uses file.copy() - always projects through template
# - OVERWRITE setting to skip existing files
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================
#
# NOTE: S2 bands B8 (NIR, 842nm) and B11 (SWIR1, 1610nm) are present in the
# raw fine_10m/ directory but were deliberately excluded from preprocessing.
# B8 is redundant with the computed spectral indices (NDVI, EVI, NBR, NDWI).
# B11 was excluded to limit multicollinearity. Neither band appears in any
# of the 6 final models. Raw files are retained on disk for reference only.
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

# Fix PostgreSQL PROJ interference (must be before loading terra)
Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")

library(terra)
library(sf)
library(dplyr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# USER-PROVIDED TEMPLATE GRIDS (MUST BE IN EPSG:5070)
# These define the exact grid (CRS, resolution, extent, origin) for outputs
source("R/00_config/PHASE4_config.R")   # defines EXTERNAL_DATA_ROOT and PHASE4_CONFIG$paths
TEMPLATE_FINE   <- file.path(PHASE4_CONFIG$paths$cov_fine_raw,   "S2_NDVI_10m_2020_2024.tif")    # 10m
TEMPLATE_COARSE <- file.path(PHASE4_CONFIG$paths$cov_coarse_raw, "MODIS_NDVI_250m_2020_2024_NE.tif") # 250m

# AOI shapefile for clipping
AOI_SHAPEFILE <- PHASE4_CONFIG$paths$aoi

# Output directories
OUTPUT_DIR_FINE   <- PHASE4_CONFIG$paths$cov_fine
OUTPUT_DIR_COARSE <- PHASE4_CONFIG$paths$cov_coarse

# GDAL write options
GDAL_OPTIONS <- c(
  "TILED=YES",
  "COMPRESS=DEFLATE",
  "BLOCKXSIZE=512",
  "BLOCKYSIZE=512",
  "BIGTIFF=YES"
)

# Resampling methods
RESAMPLE_CONTINUOUS <- "bilinear"
RESAMPLE_CATEGORICAL <- "near"

# Verification tolerances
RESOLUTION_TOLERANCE <- 1.0    # 1 meter
EXTENT_TOLERANCE <- 100.0      # 100 meters

# OVERWRITE EXISTING FILES?
# Set to TRUE to reprocess all files (even if they exist)
# Set to FALSE to skip files that already exist (faster for reruns)
OVERWRITE <- FALSE  # Change to TRUE to force reprocessing

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: TEMPLATE-ALIGNED RASTER PREPROCESSING\n")
cat("  EPSG:5070 (NAD83 Albers) with AOI Clipping\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("✓ Cleared PostgreSQL PROJ paths (prevents CRS read errors)\n\n")

cat("Configuration:\n")
cat("  Fine template:", TEMPLATE_FINE, "\n")
cat("  Coarse template:", TEMPLATE_COARSE, "\n")
cat("  AOI shapefile:", AOI_SHAPEFILE, "\n")
cat("  Output (fine):", OUTPUT_DIR_FINE, "\n")
cat("  Output (coarse):", OUTPUT_DIR_COARSE, "\n")
cat("  Overwrite:", if (OVERWRITE) "YES (will reprocess all)" else "NO (will skip existing)", "\n\n")

# Create output directories
dir.create(OUTPUT_DIR_FINE, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR_COARSE, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOGGING SETUP
# =============================================================================

processing_log <- data.frame(
  covariate = character(),
  scale = character(),
  source_path = character(),
  output_path = character(),
  method = character(),
  status = character(),
  error_msg = character(),
  stringsAsFactors = FALSE
)

log_entry <- function(cov, scale, src, out, method, status, error = "") {
  processing_log <<- rbind(processing_log, data.frame(
    covariate = cov,
    scale = scale,
    source_path = src,
    output_path = out,
    method = method,
    status = status,
    error_msg = error,
    stringsAsFactors = FALSE
  ))
}

# =============================================================================
# LOAD TEMPLATE GRIDS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  LOADING TEMPLATE GRIDS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load fine scale template
if (!file.exists(TEMPLATE_FINE)) {
  stop(sprintf("FATAL: Fine template not found: %s\n  Please create template grid first!", TEMPLATE_FINE))
}

cat("Loading fine scale (10m) template...\n")
template_fine <- rast(TEMPLATE_FINE)

cat("  ✓ Loaded\n")
crs_proj_fine <- crs(template_fine, proj = TRUE)
cat("    CRS:", substr(crs_proj_fine, 1, 80), "...\n")
cat("    Resolution:", paste(res(template_fine), collapse = " x "), "meters\n")
cat("    Extent:", paste(sprintf("%.1f", as.vector(ext(template_fine))), collapse = ", "), "\n")
cat("    Dimensions:", paste(dim(template_fine)[1:2], collapse = " x "), "\n\n")

# Load coarse scale template
if (!file.exists(TEMPLATE_COARSE)) {
  stop(sprintf("FATAL: Coarse template not found: %s\n  Please create template grid first!", TEMPLATE_COARSE))
}

cat("Loading coarse scale (250m) template...\n")
template_coarse <- rast(TEMPLATE_COARSE)

cat("  ✓ Loaded\n")
crs_proj_coarse <- crs(template_coarse, proj = TRUE)
cat("    CRS:", substr(crs_proj_coarse, 1, 80), "...\n")
cat("    Resolution:", paste(res(template_coarse), collapse = " x "), "meters\n")
cat("    Extent:", paste(sprintf("%.1f", as.vector(ext(template_coarse))), collapse = ", "), "\n")
cat("    Dimensions:", paste(dim(template_coarse)[1:2], collapse = " x "), "\n\n")

# Verify both templates are in EPSG:5070 (NAD83 Conus Albers)
verify_is_5070 <- function(r, template_name) {
  crs_desc <- crs(r, describe = TRUE)
  crs_proj <- crs(r, proj = TRUE)
  
  # Extract info safely (handle NA values)
  crs_name <- if (!is.null(crs_desc$name) && !is.na(crs_desc$name)) crs_desc$name else ""
  crs_code <- if (!is.null(crs_desc$code) && !is.na(crs_desc$code)) crs_desc$code else ""
  
  # Check multiple ways EPSG:5070 can be identified
  is_5070 <- grepl("5070", crs_code, ignore.case = TRUE) ||
    grepl("5070", crs_proj, ignore.case = TRUE) ||
    grepl("Contiguous.*USA.*Albers", crs_name, ignore.case = TRUE) ||
    (grepl("NAD.*1983", crs_name, ignore.case = TRUE) && grepl("Albers", crs_name, ignore.case = TRUE)) ||
    grepl("Conus.*Albers", crs_name, ignore.case = TRUE)
  
  if (!is_5070) {
    cat("\n")
    cat("CRS Verification Details for", template_name, ":\n")
    cat("  PROJ String (full):\n")
    cat("   ", crs_proj, "\n\n")
    
    stop("FATAL: ", template_name, " is not in EPSG:5070 (NAD83 Conus Albers)!\n",
         "  CRS does not match expected patterns for EPSG:5070.\n",
         "  If this template IS in EPSG:5070, the CRS string above will help debug.")
  }
  
  return(TRUE)
}

verify_is_5070(template_fine, "Fine template")
verify_is_5070(template_coarse, "Coarse template")

cat("✓ Both templates verified to be EPSG:5070 (NAD83 Conus Albers)\n\n")

# =============================================================================
# LOAD AOI SHAPEFILE
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  LOADING AOI SHAPEFILE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

if (!file.exists(AOI_SHAPEFILE)) {
  stop(sprintf("FATAL: AOI shapefile not found: %s", AOI_SHAPEFILE))
}

cat("Loading:", AOI_SHAPEFILE, "\n")
aoi <- st_read(AOI_SHAPEFILE, quiet = TRUE)

cat("  Original CRS:", st_crs(aoi)$input, "\n")
cat("  Features:", nrow(aoi), "\n")

# Transform AOI to EPSG:5070
cat("  Transforming to EPSG:5070...\n")
aoi_proj <- st_transform(aoi, 5070)

aoi_bbox <- st_bbox(aoi_proj)

cat("  ✓ AOI ready\n")
cat("    Extent:\n")
cat("      xmin:", sprintf("%.0f", aoi_bbox["xmin"]), "meters\n")
cat("      xmax:", sprintf("%.0f", aoi_bbox["xmax"]), "meters\n")
cat("      ymin:", sprintf("%.0f", aoi_bbox["ymin"]), "meters\n")
cat("      ymax:", sprintf("%.0f", aoi_bbox["ymax"]), "meters\n\n")

# Convert to terra vector
aoi_vect <- vect(aoi_proj)

# =============================================================================
# VERIFICATION FUNCTION
# =============================================================================

verify_output <- function(output_path, template, covariate_name) {
  # Test 1: Can open?
  r <- NULL
  tryCatch({
    r <- rast(output_path)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Cannot open:", e$message)))
  })
  
  if (is.null(r)) {
    return(list(success = FALSE, message = "Failed to open"))
  }
  
  # Test 2: CRS matches? (compare EPSG codes or PROJ strings, not full WKT)
  r_crs_desc <- crs(r, describe = TRUE)
  template_crs_desc <- crs(template, describe = TRUE)
  
  # Compare EPSG codes if available
  if (!is.null(r_crs_desc$code) && !is.na(r_crs_desc$code) && 
      !is.null(template_crs_desc$code) && !is.na(template_crs_desc$code)) {
    if (r_crs_desc$code != template_crs_desc$code) {
      return(list(success = FALSE, 
                  message = sprintf("CRS code mismatch: got %s, expected %s",
                                    r_crs_desc$code, template_crs_desc$code)))
    }
  } else {
    # Fall back to comparing PROJ strings
    r_proj <- substr(crs(r, proj = TRUE), 1, 100)
    template_proj <- substr(crs(template, proj = TRUE), 1, 100)
    
    # Check both contain "5070" or "aea" (Albers Equal Area)
    r_has_5070 <- grepl("5070", r_proj) || grepl("aea", r_proj)
    template_has_5070 <- grepl("5070", template_proj) || grepl("aea", template_proj)
    
    if (!r_has_5070 || !template_has_5070) {
      return(list(success = FALSE, 
                  message = "CRS projection mismatch (neither is EPSG:5070)"))
    }
  }
  
  # Test 3: Resolution matches?
  res_diff <- abs(res(r) - res(template))
  if (any(res_diff > RESOLUTION_TOLERANCE)) {
    return(list(success = FALSE, 
                message = sprintf("Resolution: got %.2f, expected %.2f", 
                                  res(r)[1], res(template)[1])))
  }
  
  # Test 4: Extent is within template bounds?
  ext_r <- ext(r)
  ext_t <- ext(template)
  
  # Check output is within template
  if (ext_r[1] < ext_t[1] - EXTENT_TOLERANCE || ext_r[2] > ext_t[2] + EXTENT_TOLERANCE ||
      ext_r[3] < ext_t[3] - EXTENT_TOLERANCE || ext_r[4] > ext_t[4] + EXTENT_TOLERANCE) {
    return(list(success = FALSE, 
                message = sprintf("Extent outside template bounds")))
  }
  
  # Test 5: Can read values?
  tryCatch({
    vals <- values(r, row = 1, nrows = 10)
    if (all(is.na(vals))) {
      return(list(success = FALSE, message = "All values NA"))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Cannot read values:", e$message)))
  })
  
  # Test 6: File size reasonable?
  if (file.size(output_path) < 1000) {
    return(list(success = FALSE, message = "File too small"))
  }
  
  return(list(success = TRUE, message = "OK"))
}

# =============================================================================
# PROCESSING FUNCTION
# =============================================================================

process_covariate <- function(cov_key, cov, template, output_dir) {
  
  cat("\n──────────────────────────────────────────────────────────────────\n")
  cat("Processing:", cov$display_name, "(", cov$resolution, ")\n")
  cat("──────────────────────────────────────────────────────────────────\n")
  
  output_path <- file.path(output_dir, basename(cov$path))
  
  cat("  Source:", cov$path, "\n")
  cat("  Output:", output_path, "\n")
  
  # Check if output exists and OVERWRITE setting
  if (file.exists(output_path)) {
    if (!OVERWRITE) {
      cat("  ✓ Output exists, skipping (OVERWRITE=FALSE)\n")
      log_entry(cov$display_name, cov$resolution, cov$path, output_path,
                "skipped", "skipped_exists", "")
      return(TRUE)
    } else {
      cat("  ⊘ Output exists, will overwrite (OVERWRITE=TRUE)\n")
    }
  }
  
  # Check source exists
  if (!file.exists(cov$path)) {
    cat("  ✗ Source not found\n")
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              "none", "failed_source_missing", "Source file not found")
    return(FALSE)
  }
  
  # Load source
  cat("  Loading source...\n")
  r_source <- NULL
  tryCatch({
    r_source <- rast(cov$path)
    cat("  ✓ Loaded\n")
  }, error = function(e) {
    cat("  ✗ Load failed:", e$message, "\n")
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              "none", "failed_load", e$message)
    return(FALSE)
  })
  
  if (is.null(r_source)) return(FALSE)
  
  # Determine resampling method
  method <- if (cov$type == "categorical") RESAMPLE_CATEGORICAL else RESAMPLE_CONTINUOUS
  cat("  Method:", method, "\n")
  
  # CRITICAL: Project to template
  cat("  Projecting to template...\n")
  r_proj <- NULL
  tryCatch({
    r_proj <- project(r_source, template, method = method, align = TRUE)
    cat("  ✓ Projected\n")
  }, error = function(e) {
    cat("  ✗ Projection failed:", e$message, "\n")
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              method, "failed_project", e$message)
    return(FALSE)
  })
  
  if (is.null(r_proj)) return(FALSE)
  
  # Crop/mask to AOI
  cat("  Cropping to AOI...\n")
  r_clipped <- NULL
  tryCatch({
    r_clipped <- crop(r_proj, aoi_vect)
    r_clipped <- mask(r_clipped, aoi_vect)
    cat("  ✓ Clipped\n")
  }, error = function(e) {
    cat("  ✗ Clipping failed:", e$message, "\n")
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              method, "failed_clip", e$message)
    return(FALSE)
  })
  
  if (is.null(r_clipped)) return(FALSE)
  
  # Write
  cat("  Writing...\n")
  tryCatch({
    writeRaster(r_clipped, output_path, 
                overwrite = TRUE,
                gdal = GDAL_OPTIONS,
                datatype = "FLT4S")
    cat("  ✓ Written\n")
  }, error = function(e) {
    cat("  ✗ Write failed:", e$message, "\n")
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              method, "failed_write", e$message)
    return(FALSE)
  })
  
  # CRITICAL: Verify
  cat("  Verifying...\n")
  verify_result <- verify_output(output_path, template, cov$display_name)
  
  if (!verify_result$success) {
    cat("  ✗ VERIFICATION FAILED:", verify_result$message, "\n")
    cat("  → Deleting corrupt output\n")
    
    if (file.exists(output_path)) file.remove(output_path)
    
    log_entry(cov$display_name, cov$resolution, cov$path, output_path,
              method, "failed_verification", verify_result$message)
    
    # Don't stop - continue processing other files
    return(FALSE)
  }
  
  cat("  ✓ VERIFIED\n")
  
  log_entry(cov$display_name, cov$resolution, cov$path, output_path,
            method, "success", "")
  
  # Cleanup after each file to free memory and disk space
  rm(r_source, r_proj, r_clipped)
  tmpFiles(remove = TRUE)
  gc(verbose = FALSE)
  
  return(TRUE)
}

# =============================================================================
# MAIN PROCESSING LOOP
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PROCESSING COVARIATES\n")
cat("═══════════════════════════════════════════════════════════════════\n")

active_covs <- Filter(function(x) x$active, COVARIATES)

n_total <- length(active_covs)
n_processed <- 0
n_skipped <- 0
n_failed <- 0

for (cov_key in names(active_covs)) {
  cov <- active_covs[[cov_key]]
  
  template <- if (cov$scale == "fine") template_fine else template_coarse
  output_dir <- if (cov$scale == "fine") OUTPUT_DIR_FINE else OUTPUT_DIR_COARSE
  
  success <- process_covariate(cov_key, cov, template, output_dir)
  
  if (success) {
    status <- processing_log$status[nrow(processing_log)]
    if (status == "skipped_exists") {
      n_skipped <- n_skipped + 1
    } else if (status == "success") {
      n_processed <- n_processed + 1
    }
  } else {
    n_failed <- n_failed + 1
  }
}

# =============================================================================
# SAVE LOG & SUMMARY
# =============================================================================

log_path <- "preprocessing_log.csv"
write.csv(processing_log, log_path, row.names = FALSE)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Summary:\n")
cat("  Total:", n_total, "\n")
cat("  Processed:", n_processed, "\n")
cat("  Skipped:", n_skipped, "\n")
cat("  Failed:", n_failed, "\n\n")

cat("Log:", log_path, "\n\n")

if (n_failed > 0) {
  cat("⚠ Some failures detected - see log\n")
  failed <- processing_log[grepl("^failed", processing_log$status), ]
  cat("\nFailed covariates:\n")
  for (i in 1:nrow(failed)) {
    cat(sprintf("  • %s: %s\n", failed$covariate[i], failed$error_msg[i]))
  }
  cat("\n")
}

# =============================================================================
# UPDATE CONFIG
# =============================================================================

config_content <- '# =============================================================================
# PREPROCESSED RASTERS CONFIG (EPSG:5070, Template-Aligned)
# =============================================================================
# Auto-generated by PHASE4_00_preprocess_rasters.R
#
# All rasters aligned to template grids in EPSG:5070 and clipped to AOI
# =============================================================================

preprocessed_count <- 0
missing_count <- 0

for (cov_name in names(COVARIATES)) {
  cov <- COVARIATES[[cov_name]]
  
  new_path <- file.path(
    if (cov$scale == "fine") PHASE4_CONFIG$paths$cov_fine else PHASE4_CONFIG$paths$cov_coarse,
    basename(cov$path)
  )
  
  if (file.exists(new_path)) {
    COVARIATES[[cov_name]]$path <- new_path
    COVARIATES[[cov_name]]$preprocessed <- TRUE
    preprocessed_count <- preprocessed_count + 1
  } else {
    COVARIATES[[cov_name]]$preprocessed <- FALSE
    missing_count <- missing_count + 1
  }
}

if (preprocessed_count > 0) {
  cat("✓ Using", preprocessed_count, "preprocessed rasters (EPSG:5070, template-aligned)\\n")
}

if (missing_count > 0) {
  cat("ℹ", missing_count, "rasters using originals\\n")
}
'

writeLines(config_content, "R/00_config/PHASE4_config_covariates_PREPROCESSED.R")

cat("✓ Config updated: R/00_config/PHASE4_config_covariates_PREPROCESSED.R\n\n")

if (n_processed + n_skipped == n_total) {
  cat("✓✓ All covariates ready!\n\n")
} else {
  cat("Next: Review failures and rerun if needed\n\n")
}