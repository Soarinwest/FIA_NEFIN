# Extract NDVI/PRISM covariates for baseline dataset
source("R/00_config/config.R")

cat("\nPHASE D - STEP 3: EXTRACT BASELINE COVARIATES\n\n")

# TODO: Extract environmental covariates at lat_for_extraction/lon_for_extraction
# - NDVI (MODIS or Sentinel-2)
# - PRISM (temperature, precipitation)
# Output: data/processed/baseline_covariates.csv
