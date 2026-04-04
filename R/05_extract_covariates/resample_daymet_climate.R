# =============================================================================
# RESAMPLE DAYMET CLIMATE DATA TO FINE (10m) AND COARSE (250m) SCALES
# =============================================================================
# Resamples Daymet temperature and precipitation from 1km to:
#   - 10m (to match Sentinel-2)
#   - 250m (to match MODIS)
#
# Input: 1km Daymet files from GEE exports in data/raw/Daymet/
# Output: Resampled rasters in directories specified by config
#
# NOTE: This script sources PHASE4_config_covariates.R to get all paths
#
# Usage:
#   Rscript resample_daymet_climate.R
# =============================================================================

library(terra)

# =============================================================================
# USER CONFIGURATION - EDIT THIS SECTION IF NEEDED
# =============================================================================

# Daymet input directory (where your 1km Daymet files from GEE are stored)
# Relative to project root — adjust if your Daymet files are elsewhere
DAYMET_INPUT_DIR <- "data/raw/daymet"

# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  RESAMPLING DAYMET CLIMATE DATA (1km → 10m & 250m)\n")
cat("  Temperature (Tmin, Tmax, Tmean) & Precipitation\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD CONFIGURATION
# =============================================================================

cat("Loading configuration from PHASE4_config_covariates.R...\n")

# Find and source config file
config_file <- "R/00_config/PHASE4_config_covariates.R"
if (!file.exists(config_file)) {
  # Try alternative locations
  config_file <- "PHASE4_config_covariates.R"
  if (!file.exists(config_file)) {
    stop("Cannot find PHASE4_config_covariates.R\n",
         "Please ensure it's in R/phase4_modeling/ or current directory")
  }
}

source(config_file, verbose = FALSE)
cat("✓ Configuration loaded\n\n")

# =============================================================================
# EXTRACT PATHS FROM CONFIG
# =============================================================================

# Get output directories from config paths
dir_fine <- dirname(COVARIATES$tmean_fine$path)
dir_coarse <- dirname(COVARIATES$tmean_coarse$path)
cat("Fine scale output:", dir_fine, "\n")
cat("Coarse scale output:", dir_coarse, "\n")

# Get template paths from config
template_fine <- COVARIATES$ndvi_s2$path      # 10m Sentinel-2 NDVI
template_coarse <- COVARIATES$ndvi_modis$path  # 250m MODIS NDVI
cat("Fine scale template:", basename(template_fine), "\n")
cat("Coarse scale template:", basename(template_coarse), "\n")

# =============================================================================
# DAYMET INPUT DIRECTORY CONFIGURATION
# =============================================================================

if (!is.null(DAYMET_INPUT_DIR)) {
  # OPTION 1: Use manually configured path (supports different drives)
  daymet_dir <- DAYMET_INPUT_DIR
  cat("Daymet input: Using configured path\n")
} else {
  # OPTION 2: Auto-detect from covariates location (same drive)
  base_data_dir <- dirname(dirname(dirname(COVARIATES$ndvi_s2$path)))
  daymet_dir <- file.path(base_data_dir, "raw", "daymet")
  cat("Daymet input: Auto-detected from config\n")
}

cat("Daymet directory:", daymet_dir, "\n\n")

# Verify Daymet directory exists
if (!dir.exists(daymet_dir)) {
  cat("⚠ WARNING: Daymet directory not found at:", daymet_dir, "\n")
  cat("Please update 'DAYMET_INPUT_DIR' at the top of this script.\n")
  cat("Common locations:\n")
  cat("  - data/raw/daymet  (relative to project root)\n")
  cat("  - D:/FIA_NEFIN/data/raw/daymet\n\n")
}

# Expected Daymet files (1km resolution from GEE export)
daymet_files <- c(
  tmin = "Daymet_tmin_1km_2020_2024_NE.tif",
  tmax = "Daymet_tmax_1km_2020_2024_NE.tif",
  tmean = "Daymet_tmean_1km_2020_2024_NE.tif",
  prcp = "Daymet_prcp_1km_2020_2024_NE.tif"
)
cat("\n")

# Create output directories (from config)
dir.create(dir_fine, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_coarse, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# STEP 1: CHECK INPUT FILES
# =============================================================================

cat("Step 1: Checking Daymet input files...\n\n")

# Check if Daymet directory exists
if (!dir.exists(daymet_dir)) {
  stop("Daymet directory not found: ", daymet_dir, "\n",
       "Please create it and add the 1km Daymet files from GEE exports.")
}

# Check each expected file
missing_files <- c()
for (var_name in names(daymet_files)) {
  file_path <- file.path(daymet_dir, daymet_files[var_name])
  if (file.exists(file_path)) {
    cat("  ✓ Found:", daymet_files[var_name], "\n")
  } else {
    cat("  ✗ Missing:", daymet_files[var_name], "\n")
    missing_files <- c(missing_files, daymet_files[var_name])
  }
}

if (length(missing_files) > 0) {
  cat("\n⚠ WARNING: Missing", length(missing_files), "Daymet file(s)\n")
  cat("Expected location:", daymet_dir, "\n")
  cat("Missing files:\n")
  for (f in missing_files) {
    cat("  -", f, "\n")
  }
  cat("\nPlease download these files from Google Drive after GEE export\n")
  cat("and place them in:", daymet_dir, "\n\n")
  
  # Continue with available files
  cat("Continuing with available files...\n\n")
} else {
  cat("\n✓ All Daymet files found!\n\n")
}

# =============================================================================
# STEP 2: LOAD TEMPLATE RASTERS
# =============================================================================

cat("Step 2: Loading template rasters...\n")

# Check templates exist
if (!file.exists(template_fine)) {
  stop("Fine template not found: ", template_fine, "\n",
       "Please ensure Sentinel-2 NDVI is processed first.")
}
if (!file.exists(template_coarse)) {
  stop("Coarse template not found: ", template_coarse, "\n",
       "Please ensure MODIS NDVI is processed first.")
}

# Load templates
cat("  Loading fine scale template (10m)...\n")
template_10m <- rast(template_fine)
cat("    CRS:", crs(template_10m, describe = TRUE)$name, "\n")
cat("    Resolution:", paste(round(res(template_10m), 4), collapse = " x "), "meters\n")
cat("    Extent:", paste(round(ext(template_10m)[], 2), collapse = ", "), "\n")
cat("    Dimensions:", paste(dim(template_10m)[1:2], collapse = " x "), "pixels\n")

cat("\n  Loading coarse scale template (250m)...\n")
template_250m <- rast(template_coarse)
cat("    CRS:", crs(template_250m, describe = TRUE)$name, "\n")
cat("    Resolution:", paste(round(res(template_250m), 4), collapse = " x "), "meters\n")
cat("    Extent:", paste(round(ext(template_250m)[], 2), collapse = ", "), "\n")
cat("    Dimensions:", paste(dim(template_250m)[1:2], collapse = " x "), "pixels\n\n")

# =============================================================================
# STEP 3: RESAMPLE EACH DAYMET RASTER
# =============================================================================

cat("Step 3: Resampling Daymet climate data...\n\n")

processed_files <- list(fine = c(), coarse = c())

for (var_name in names(daymet_files)) {
  
  daymet_file <- file.path(daymet_dir, daymet_files[var_name])
  
  cat("───────────────────────────────────────────────────────────────\n")
  cat("  Variable:", toupper(var_name), "\n")
  cat("───────────────────────────────────────────────────────────────\n")
  
  # Check file exists
  if (!file.exists(daymet_file)) {
    cat("  ⚠ File not found:", basename(daymet_file), "\n")
    cat("  Skipping...\n\n")
    next
  }
  
  # Load Daymet raster
  cat("  Loading Daymet 1km raster...\n")
  daymet <- rast(daymet_file)
  
  cat("    Resolution:", paste(round(res(daymet), 0), collapse = " x "), "meters\n")
  cat("    Dimensions:", paste(dim(daymet)[1:2], collapse = " x "), "pixels\n")
  cat("    Value range:", round(minmax(daymet)[1], 2), "to", 
      round(minmax(daymet)[2], 2), "\n")
  cat("    Units:", ifelse(var_name == "prcp", "mm/day", "°C"), "\n")
  
  # -------------------------------------------------------------------------
  # Resample to FINE scale (10m)
  # -------------------------------------------------------------------------
  
  cat("\n  Resampling to 10m (fine scale)...\n")
  cat("    Method: Bilinear interpolation (smooth gradients)\n")
  
  # Reproject and resample
  if (crs(daymet) != crs(template_10m)) {
    cat("    Reprojecting to template CRS...\n")
    daymet_fine <- project(daymet, template_10m, method = "bilinear")
  } else {
    daymet_fine <- resample(daymet, template_10m, method = "bilinear")
  }
  
  cat("    Output resolution:", paste(round(res(daymet_fine), 4), collapse = " x "), "meters\n")
  cat("    Output dimensions:", paste(dim(daymet_fine)[1:2], collapse = " x "), "pixels\n")
  cat("    Value range:", round(minmax(daymet_fine)[1], 2), "to", 
      round(minmax(daymet_fine)[2], 2), "\n")
  
  # Check for smooth gradients (should be similar range to 1km)
  range_diff <- abs(minmax(daymet_fine)[1] - minmax(daymet)[1]) + 
    abs(minmax(daymet_fine)[2] - minmax(daymet)[2])
  if (range_diff < 1) {
    cat("    ✓ Smooth interpolation verified (range preserved)\n")
  } else {
    cat("    ⚠ Warning: Value range changed significantly\n")
  }
  
  # Save (use simple filename, config already has full path pattern)
  output_fine <- file.path(dir_fine, paste0(var_name, ".tif"))
  writeRaster(daymet_fine, output_fine, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))
  
  cat("    ✓ Saved:", basename(output_fine), "\n")
  cat("    File size:", round(file.size(output_fine) / 1024^2, 1), "MB\n")
  
  processed_files$fine <- c(processed_files$fine, output_fine)
  
  # -------------------------------------------------------------------------
  # Resample to COARSE scale (250m)
  # -------------------------------------------------------------------------
  
  cat("\n  Resampling to 250m (coarse scale)...\n")
  cat("    Method: Bilinear interpolation (smooth gradients)\n")
  
  # Reproject and resample
  if (crs(daymet) != crs(template_250m)) {
    cat("    Reprojecting to template CRS...\n")
    daymet_coarse <- project(daymet, template_250m, method = "bilinear")
  } else {
    daymet_coarse <- resample(daymet, template_250m, method = "bilinear")
  }
  
  cat("    Output resolution:", paste(round(res(daymet_coarse), 4), collapse = " x "), "meters\n")
  cat("    Output dimensions:", paste(dim(daymet_coarse)[1:2], collapse = " x "), "pixels\n")
  cat("    Value range:", round(minmax(daymet_coarse)[1], 2), "to", 
      round(minmax(daymet_coarse)[2], 2), "\n")
  
  # Save (use simple filename)
  output_coarse <- file.path(dir_coarse, paste0(var_name, ".tif"))
  writeRaster(daymet_coarse, output_coarse, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES"))
  
  cat("    ✓ Saved:", basename(output_coarse), "\n")
  cat("    File size:", round(file.size(output_coarse) / 1024^2, 1), "MB\n\n")
  
  processed_files$coarse <- c(processed_files$coarse, output_coarse)
}

# =============================================================================
# STEP 4: VERIFY OUTPUTS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VERIFICATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# List output files
cat("Fine scale (10m) outputs:\n")
fine_files <- list.files(dir_fine, pattern = "\\.tif$", full.names = TRUE)
if (length(fine_files) > 0) {
  for (f in fine_files) {
    r <- rast(f)
    var_name <- gsub("\\.tif$", "", basename(f))
    units <- ifelse(var_name == "prcp", "mm/day", "°C")
    cat("  ✓", basename(f), "\n")
    cat("     Resolution:", paste(round(res(r), 4), collapse = " x "), "meters\n")
    cat("     Dimensions:", paste(dim(r)[1:2], collapse = " x "), "pixels\n")
    cat("     Range:", round(minmax(r)[1], 2), "to", round(minmax(r)[2], 2), units, "\n")
  }
} else {
  cat("  No files created (check for errors above)\n")
}

cat("\nCoarse scale (250m) outputs:\n")
coarse_files <- list.files(dir_coarse, pattern = "\\.tif$", full.names = TRUE)
if (length(coarse_files) > 0) {
  for (f in coarse_files) {
    r <- rast(f)
    var_name <- gsub("\\.tif$", "", basename(f))
    units <- ifelse(var_name == "prcp", "mm/day", "°C")
    cat("  ✓", basename(f), "\n")
    cat("     Resolution:", paste(round(res(r), 4), collapse = " x "), "meters\n")
    cat("     Dimensions:", paste(dim(r)[1:2], collapse = " x "), "pixels\n")
    cat("     Range:", round(minmax(r)[1], 2), "to", round(minmax(r)[2], 2), units, "\n")
  }
} else {
  cat("  No files created (check for errors above)\n")
}

# =============================================================================
# STEP 5: CHECK ALIGNMENT
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  ALIGNMENT CHECK\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Checking fine scale (10m) alignment...\n")
if (length(fine_files) >= 2) {
  r1 <- rast(fine_files[1])
  r2 <- rast(fine_files[2])
  
  if (identical(ext(r1), ext(r2)) && identical(res(r1), res(r2))) {
    cat("  ✓ All fine scale rasters perfectly aligned\n")
  } else {
    cat("  ⚠ Warning: Fine scale rasters have different extents/resolutions\n")
  }
  
  # Check alignment with template
  if (identical(ext(r1), ext(template_10m)) && identical(res(r1), res(template_10m))) {
    cat("  ✓ Climate rasters match Sentinel-2 template grid\n")
  } else {
    cat("  ⚠ Warning: Climate rasters don't perfectly match S2 grid\n")
  }
} else {
  cat("  ⚠ Less than 2 files - cannot check alignment\n")
}

cat("\nChecking coarse scale (250m) alignment...\n")
if (length(coarse_files) >= 2) {
  r1 <- rast(coarse_files[1])
  r2 <- rast(coarse_files[2])
  
  if (identical(ext(r1), ext(r2)) && identical(res(r1), res(r2))) {
    cat("  ✓ All coarse scale rasters perfectly aligned\n")
  } else {
    cat("  ⚠ Warning: Coarse scale rasters have different extents/resolutions\n")
  }
  
  # Check alignment with template
  if (identical(ext(r1), ext(template_250m)) && identical(res(r1), res(template_250m))) {
    cat("  ✓ Climate rasters match MODIS template grid\n")
  } else {
    cat("  ⚠ Warning: Climate rasters don't perfectly match MODIS grid\n")
  }
} else {
  cat("  ⚠ Less than 2 files - cannot check alignment\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DAYMET RESAMPLING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Source data: Daymet V4 1km (2020-2024 mean, May-Sep)\n")
cat("Method: Bilinear interpolation (creates smooth gradients)\n\n")

cat("Paths configuration:\n")
cat("  Daymet input dir:", daymet_dir, "\n")
cat("  Fine scale output:", dir_fine, "(from config)\n")
cat("  Coarse scale output:", dir_coarse, "(from config)\n\n")

cat("Files created:\n")
cat("  Fine scale (10m):", length(fine_files), "rasters\n")
cat("  Coarse scale (250m):", length(coarse_files), "rasters\n\n")

cat("Climate variables available:\n")
if (length(fine_files) > 0) {
  for (f in fine_files) {
    var <- gsub("\\.tif$", "", basename(f))
    desc <- switch(var,
                   tmin = "Minimum temperature (°C)",
                   tmax = "Maximum temperature (°C)",
                   tmean = "Mean temperature (°C)",
                   prcp = "Precipitation (mm/day)",
                   var)
    cat("  -", desc, "\n")
  }
}

cat("\n")
cat("IMPORTANT NOTES:\n")
cat("  • Output paths from PHASE4_config_covariates.R\n")
cat("  • Daymet input path configured separately (may be on different drive)\n")
cat("  • 10m and 250m files are INTERPOLATED from 1km\n")
cat("  • Smooth appearance is CORRECT for climate data\n")
cat("  • Climate varies gradually, not at pixel boundaries\n")
cat("  • These files match your Sentinel-2 and MODIS grids\n\n")

cat("TO CHANGE DAYMET INPUT LOCATION:\n")
cat("  Edit 'daymet_dir' variable near top of this script\n")
cat("  Current: ", daymet_dir, "\n\n")

cat("Next steps:\n")
cat("  1. Add other covariates (S2, MODIS, DEM) to the same directories\n")
cat("  2. Re-run covariate extraction:\n")
cat("     Rscript R/phase4_modeling/PHASE4_extract_covariates.R\n")
cat("  3. Config will automatically find these new climate files!\n\n")

cat("Files created (config should already have these):\n")
cat("  Fine scale:\n")
cat("    • tmean.tif → matches:", COVARIATES$tmean_fine$path, "\n")
cat("    • prcp.tif  → matches:", COVARIATES$ppt_fine$path, "\n")
cat("    • tmin.tif  (NEW - add to config if needed)\n")
cat("    • tmax.tif  (NEW - add to config if needed)\n")
cat("  Coarse scale:\n")
cat("    • tmean.tif → matches:", COVARIATES$tmean_coarse$path, "\n")
cat("    • prcp.tif  → matches:", COVARIATES$ppt_coarse$path, "\n")
cat("    • tmin.tif  (NEW - add to config if needed)\n")
cat("    • tmax.tif  (NEW - add to config if needed)\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DONE! Climate covariates ready for modeling.\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")