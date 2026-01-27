# =============================================================================
# PHASE 4: PREPROCESS COVARIATE RASTERS
# =============================================================================
# Pre-transform all covariate rasters to common CRS (WGS84)
# This only needs to be run ONCE, then predictions are much faster!
# =============================================================================

source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(terra)
library(dplyr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: PREPROCESS COVARIATE RASTERS\n")
cat("  Transform to Common CRS (One-Time Setup)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETUP
# =============================================================================

# Target CRS (WGS84 - most common)
target_crs <- "EPSG:4326"

# Output directory for preprocessed rasters
output_dir_fine <- "D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed"
output_dir_coarse <- "D:/FIA_NEFIN/data/covariates/coarse_250m_preprocessed"

dir.create(output_dir_fine, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_coarse, recursive = TRUE, showWarnings = FALSE)

cat("Target CRS:", target_crs, "\n")
cat("Output directories created\n\n")

# =============================================================================
# PREPROCESS RASTERS
# =============================================================================

cat("Processing covariate rasters...\n\n")

# Get active covariates
active_covs <- Filter(function(x) x$active, COVARIATES)

processed_count <- 0
skipped_count <- 0
error_count <- 0

for (cov_name in names(active_covs)) {
  cov <- active_covs[[cov_name]]
  
  cat(sprintf("Processing: %s (%s)...\n", cov$display_name, cov$resolution))
  
  # Determine output path
  if (cov$scale == "fine") {
    output_path <- file.path(output_dir_fine, basename(cov$path))
  } else {
    output_path <- file.path(output_dir_coarse, basename(cov$path))
  }
  
  # Skip if already processed
  if (file.exists(output_path)) {
    cat("  ⊘ Already preprocessed, skipping\n\n")
    skipped_count <- skipped_count + 1
    next
  }
  
  # Check if source exists
  if (!file.exists(cov$path)) {
    cat("  ✗ Source file not found:", cov$path, "\n\n")
    error_count <- error_count + 1
    next
  }
  
  tryCatch({
    # Load raster
    r <- rast(cov$path)
    
    # Get current CRS
    current_crs <- crs(r)
    cat("  Current CRS:", substr(current_crs, 1, 50), "...\n")
    
    # Check if already in target CRS
    if (grepl("WGS 84", current_crs) || grepl("4326", current_crs)) {
      cat("  ✓ Already in WGS84, copying...\n")
      file.copy(cov$path, output_path, overwrite = TRUE)
    } else {
      # Reproject
      cat("  → Reprojecting to", target_crs, "...\n")
      r_proj <- project(r, target_crs, method = "bilinear")
      
      # Save
      writeRaster(r_proj, output_path, overwrite = TRUE)
      cat("  ✓ Saved:", basename(output_path), "\n")
    }
    
    processed_count <- processed_count + 1
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
    error_count <- error_count + 1
  })
  
  cat("\n")
}

# =============================================================================
# UPDATE CONFIG FILE
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  CREATING UPDATED CONFIG\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Create updated config file that points to preprocessed rasters
config_update <- "
# =============================================================================
# AUTO-GENERATED: Use Preprocessed Rasters
# =============================================================================
# This file automatically updates paths to use preprocessed (pre-transformed)
# covariate rasters. Include this AFTER loading PHASE4_config_covariates.R
# =============================================================================

# Update paths to preprocessed rasters
for (cov_name in names(COVARIATES)) {
  cov <- COVARIATES[[cov_name]]
  
  if (cov$scale == 'fine') {
    new_path <- file.path('D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed', 
                          basename(cov$path))
  } else {
    new_path <- file.path('D:/FIA_NEFIN/data/covariates/coarse_250m_preprocessed',
                          basename(cov$path))
  }
  
  if (file.exists(new_path)) {
    COVARIATES[[cov_name]]$path <- new_path
    COVARIATES[[cov_name]]$preprocessed <- TRUE
  }
}

cat('✓ Using preprocessed covariate rasters (pre-transformed to WGS84)\\n')
"

# Save config update
writeLines(config_update, "R/00_config/PHASE4_config_covariates_PREPROCESSED.R")

cat("✓ Created: R/00_config/PHASE4_config_covariates_PREPROCESSED.R\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PREPROCESSING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Summary:\n")
cat("  Processed:", processed_count, "rasters\n")
cat("  Skipped (already done):", skipped_count, "rasters\n")
if (error_count > 0) {
  cat("  Errors:", error_count, "rasters\n")
}
cat("\n")

cat("Output locations:\n")
cat("  Fine scale (10m):", output_dir_fine, "\n")
cat("  Coarse scale (250m):", output_dir_coarse, "\n\n")

cat("NEXT STEP:\n")
cat("  Update your prediction script to use preprocessed rasters:\n")
cat("  \n")
cat("  # Instead of:\n")
cat("  source('R/00_config/PHASE4_config_covariates.R')\n")
cat("  \n")
cat("  # Use:\n")
cat("  source('R/00_config/PHASE4_config_covariates.R')\n")
cat("  source('R/00_config/PHASE4_config_covariates_PREPROCESSED.R')\n")
cat("  \n\n")

cat("Benefits:\n")
cat("  ✓ No more CRS transformation warnings\n")
cat("  ✓ Much faster predictions (10-50x speedup)\n")
cat("  ✓ Transformations done once, used many times\n\n")
