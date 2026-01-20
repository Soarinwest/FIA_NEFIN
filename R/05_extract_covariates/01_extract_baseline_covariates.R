# =============================================================================
# Extract Covariates for Baseline Dataset
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  EXTRACT BASELINE COVARIATES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD BASELINE DATA
# =============================================================================

cat("Loading baseline dataset...\n")
baseline <- read_csv("data/processed/baseline.csv", show_col_types = FALSE)
cat("  Plots:", nrow(baseline), "\n\n")

# Convert to spatial (use lat_for_extraction, lon_for_extraction)
baseline_sf <- st_as_sf(baseline, 
                        coords = c("lon_for_extraction", "lat_for_extraction"),
                        crs = 4326)  # WGS84

# Transform to Albers for raster extraction (match your rasters)
baseline_sf <- st_transform(baseline_sf, crs = 5070)

cat("Converted to spatial (Albers Equal Area)\n\n")

# =============================================================================
# EXTRACT NDVI - MODIS
# =============================================================================

cat("Extracting MODIS NDVI (2020-2024)...\n")

modis_path <- "data/raw/ndvi/modis/MODIS_NDVI_5yr_blocked_2020_2024.tif"

if (file.exists(modis_path)) {
  modis <- rast(modis_path)
  
  # Extract values
  ndvi_modis <- terra::extract(modis, vect(baseline_sf), method = "bilinear")
  
  # Add to baseline (terra returns ID column + values)
  baseline$ndvi_modis <- ndvi_modis[[2]]  # Second column is the value
  
  cat("  ✓ Extracted MODIS NDVI\n")
  cat("  Range:", sprintf("%.3f - %.3f\n", 
                          min(baseline$ndvi_modis, na.rm = TRUE),
                          max(baseline$ndvi_modis, na.rm = TRUE)))
  cat("  Missing:", sum(is.na(baseline$ndvi_modis)), "plots\n\n")
} else {
  cat("  ⚠ MODIS file not found, skipping\n\n")
  baseline$ndvi_modis <- NA
}

# =============================================================================
# EXTRACT NDVI - SENTINEL-2
# =============================================================================

cat("Extracting Sentinel-2 NDVI (10m, 2020-2025)...\n")

s2_path <- "data/raw/ndvi/s2/S2_NDVI_10m_2020_2025.tif"

if (file.exists(s2_path)) {
  s2 <- rast(s2_path)
  
  # Extract values
  ndvi_s2 <- terra::extract(s2, vect(baseline_sf), method = "bilinear")
  
  baseline$ndvi_s2 <- ndvi_s2[[2]]
  
  cat("  ✓ Extracted Sentinel-2 NDVI\n")
  cat("  Range:", sprintf("%.3f - %.3f\n",
                          min(baseline$ndvi_s2, na.rm = TRUE),
                          max(baseline$ndvi_s2, na.rm = TRUE)))
  cat("  Missing:", sum(is.na(baseline$ndvi_s2)), "plots\n\n")
} else {
  cat("  ⚠ Sentinel-2 file not found, skipping\n\n")
  baseline$ndvi_s2 <- NA
}

# =============================================================================
# EXTRACT PRISM TEMPERATURE
# =============================================================================

cat("Extracting PRISM temperature (2020-2024)...\n")

tmean_path <- "data/raw/prism/prism_tmean_ne_2020_2024.tif"

if (file.exists(tmean_path)) {
  tmean <- rast(tmean_path)
  
  # Extract values
  tmean_vals <- terra::extract(tmean, vect(baseline_sf), method = "bilinear")
  
  baseline$tmean <- tmean_vals[[2]]
  
  cat("  ✓ Extracted temperature\n")
  cat("  Range:", sprintf("%.2f - %.2f °C\n",
                          min(baseline$tmean, na.rm = TRUE),
                          max(baseline$tmean, na.rm = TRUE)))
  cat("  Missing:", sum(is.na(baseline$tmean)), "plots\n\n")
} else {
  cat("  ⚠ PRISM tmean file not found, skipping\n\n")
  baseline$tmean <- NA
}

# =============================================================================
# EXTRACT PRISM PRECIPITATION
# =============================================================================

cat("Extracting PRISM precipitation (2020-2024)...\n")

ppt_path <- "data/raw/prism/prism_ppt_ne_2020_2024.tif"

if (file.exists(ppt_path)) {
  ppt <- rast(ppt_path)
  
  # Extract values
  ppt_vals <- terra::extract(ppt, vect(baseline_sf), method = "bilinear")
  
  baseline$ppt <- ppt_vals[[2]]
  
  cat("  ✓ Extracted precipitation\n")
  cat("  Range:", sprintf("%.0f - %.0f mm\n",
                          min(baseline$ppt, na.rm = TRUE),
                          max(baseline$ppt, na.rm = TRUE)))
  cat("  Missing:", sum(is.na(baseline$ppt)), "plots\n\n")
} else {
  cat("  ⚠ PRISM ppt file not found, skipping\n\n")
  baseline$ppt <- NA
}

# =============================================================================
# SAVE ENRICHED DATASET
# =============================================================================

cat("Saving enriched baseline dataset...\n")

output_path <- "data/processed/baseline_with_covariates.csv"
write_csv(baseline, output_path)

cat("  ✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(baseline), "\n")
cat("  Columns:", ncol(baseline), "\n\n")

# Summary
cat("Covariate summary:\n")
cat("  NDVI MODIS:  ", sum(!is.na(baseline$ndvi_modis)), "/ ", nrow(baseline), 
    sprintf(" (%.1f%%)\n", 100 * mean(!is.na(baseline$ndvi_modis))))
cat("  NDVI S2:     ", sum(!is.na(baseline$ndvi_s2)), "/ ", nrow(baseline),
    sprintf(" (%.1f%%)\n", 100 * mean(!is.na(baseline$ndvi_s2))))
cat("  Temperature: ", sum(!is.na(baseline$tmean)), "/ ", nrow(baseline),
    sprintf(" (%.1f%%)\n", 100 * mean(!is.na(baseline$tmean))))
cat("  Precip:      ", sum(!is.na(baseline$ppt)), "/ ", nrow(baseline),
    sprintf(" (%.1f%%)\n", 100 * mean(!is.na(baseline$ppt))))

# Complete cases
complete <- baseline %>%
  filter(!is.na(ndvi_modis), !is.na(ndvi_s2), !is.na(tmean), !is.na(ppt))

cat("\nPlots with all covariates:", nrow(complete), 
    sprintf(" (%.1f%%)\n\n", 100 * nrow(complete) / nrow(baseline)))

cat("✓ Baseline covariates extracted!\n")
cat("Next: Extract augmented covariates\n\n")
