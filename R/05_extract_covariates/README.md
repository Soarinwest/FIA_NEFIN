# R/05_extract_covariates

Extracts environmental covariates at plot locations.

## Scripts

1. `01_extract_baseline_covariates.R` -- spectral and climate covariates for baseline plots
2. `02_extract_augmented_covariates.R` -- spectral and climate covariates for augmented plots
3. `resample_daymet_climate.R` -- resamples Daymet V4 climate rasters to target resolution

## Covariates

- Spectral indices (NDVI, EVI, NBR, NDWI) from Sentinel-2 (10 m) and MODIS (250 m)
- Daymet V4 climate: tmean, tmin, tmax, ppt

## Coordinate handling

Extraction uses `lat_for_extraction` / `lon_for_extraction`, which are:
- Fuzzed for FIA plots
- True coordinates for NEFIN plots
