# Extract NDVI/PRISM covariates for augmented dataset
source("R/00_config/config.R")

cat("\nPHASE D - STEP 4: EXTRACT AUGMENTED COVARIATES\n\n")

# TODO: Same as baseline but for augmented dataset
# Note: Uses lat_for_extraction/lon_for_extraction which are mixed (FIA fuzzed, NEFIN true)
# Output: data/processed/augmented_covariates.csv
