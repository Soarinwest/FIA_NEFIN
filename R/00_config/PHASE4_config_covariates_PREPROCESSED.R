
# =============================================================================
# AUTO-GENERATED: Use Preprocessed Rasters (SAFE VERSION)
# =============================================================================
# This file automatically updates paths to use preprocessed (pre-transformed)
# covariate rasters ONLY if the preprocessed files actually exist.
# If preprocessed files don't exist, original paths are unchanged.
# =============================================================================

# Track how many were updated
preprocessed_count <- 0
original_count <- 0

# Update paths to preprocessed rasters (only if they exist)
for (cov_name in names(COVARIATES)) {
  cov <- COVARIATES[[cov_name]]
  
  if (cov$scale == 'fine') {
    new_path <- file.path('D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed', 
                          basename(cov$path))
  } else {
    new_path <- file.path('D:/FIA_NEFIN/data/covariates/coarse_250m_preprocessed',
                          basename(cov$path))
  }
  
  # Only update if preprocessed file exists
  if (file.exists(new_path)) {
    COVARIATES[[cov_name]]$path <- new_path
    COVARIATES[[cov_name]]$preprocessed <- TRUE
    preprocessed_count <- preprocessed_count + 1
  } else {
    COVARIATES[[cov_name]]$preprocessed <- FALSE
    original_count <- original_count + 1
  }
}

if (preprocessed_count > 0) {
  cat('✓ Using', preprocessed_count, 'preprocessed rasters (pre-transformed to WGS84)\n')
}

if (original_count > 0) {
  cat('ℹ Using', original_count, 'original rasters (preprocessing not run yet)\n')
  cat('  To speed up predictions, run: Rscript R/phase4_modeling/PHASE4_00_preprocess_rasters.R\n')
}

