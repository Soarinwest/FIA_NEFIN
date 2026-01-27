
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

cat('✓ Using preprocessed covariate rasters (pre-transformed to WGS84)\n')

