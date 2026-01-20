# Phase D Part 2: Covariate Extraction

Extracts environmental covariates at plot locations.

## Scripts

1. **01_extract_baseline_covariates.R** - NDVI/PRISM for baseline
2. **02_extract_augmented_covariates.R** - NDVI/PRISM for augmented

## Covariates

- NDVI (MODIS or Sentinel-2)
- PRISM climate (temperature, precipitation)

## Important

Uses lat_for_extraction/lon_for_extraction, which are:
- Fuzzed for FIA plots
- True for NEFIN plots
