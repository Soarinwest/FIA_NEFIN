# =============================================================================
# Extract NDVI/PRISM covariates for augmented dataset
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  EXTRACT AUGMENTED COVARIATES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD AUGMENTED DATA
# =============================================================================

cat("Loading augmented dataset...\n")
augmented <- read_csv("data/processed/augmented.csv", show_col_types = FALSE)
cat("  Plots:", nrow(augmented), "\n\n")

# Convert to spatial (use lat_for_extraction, lon_for_extraction)
augmented_sf <- st_as_sf(augmented,
												 coords = c("lon_for_extraction", "lat_for_extraction"),
												 crs = 4326)  # WGS84

# Transform to analysis CRS (Albers)
augmented_sf <- st_transform(augmented_sf, crs = CONFIG$crs_analysis)

cat("Converted augmented dataset to spatial (Albers Equal Area)\n\n")

# =============================================================================
# EXTRACT NDVI - MODIS
# =============================================================================

cat("Extracting MODIS NDVI (2020-2024)...\n")

modis_path <- "data/raw/ndvi/modis/MODIS_NDVI_5yr_blocked_2020_2024.tif"

if (file.exists(modis_path)) {
	modis <- rast(modis_path)
  
	ndvi_modis <- terra::extract(modis, vect(augmented_sf), method = "bilinear")
	augmented$ndvi_modis <- ndvi_modis[[2]]
  
	cat("  ✓ Extracted MODIS NDVI\n")
	cat("  Range:", sprintf("%.3f - %.3f\n", 
													min(augmented$ndvi_modis, na.rm = TRUE),
													max(augmented$ndvi_modis, na.rm = TRUE)))
	cat("  Missing:", sum(is.na(augmented$ndvi_modis)), "plots\n\n")
} else {
	cat("  ⚠ MODIS file not found, skipping\n\n")
	augmented$ndvi_modis <- NA
}

# =============================================================================
# EXTRACT NDVI - SENTINEL-2
# =============================================================================

cat("Extracting Sentinel-2 NDVI (10m, 2020-2025)...\n")

s2_path <- "data/raw/ndvi/s2/S2_NDVI_10m_2020_2025.tif"

if (file.exists(s2_path)) {
	s2 <- rast(s2_path)
  
	ndvi_s2 <- terra::extract(s2, vect(augmented_sf), method = "bilinear")
	augmented$ndvi_s2 <- ndvi_s2[[2]]
  
	cat("  ✓ Extracted Sentinel-2 NDVI\n")
	cat("  Range:", sprintf("%.3f - %.3f\n",
													min(augmented$ndvi_s2, na.rm = TRUE),
													max(augmented$ndvi_s2, na.rm = TRUE)))
	cat("  Missing:", sum(is.na(augmented$ndvi_s2)), "plots\n\n")
} else {
	cat("  ⚠ Sentinel-2 file not found, skipping\n\n")
	augmented$ndvi_s2 <- NA
}

# =============================================================================
# EXTRACT PRISM TEMPERATURE
# =============================================================================

cat("Extracting PRISM temperature (2020-2024)...\n")

tmean_path <- "data/raw/prism/prism_tmean_ne_2020_2024.tif"

if (file.exists(tmean_path)) {
	tmean <- rast(tmean_path)
  
	tmean_vals <- terra::extract(tmean, vect(augmented_sf), method = "bilinear")
	augmented$tmean <- tmean_vals[[2]]
  
	cat("  ✓ Extracted temperature\n")
	cat("  Range:", sprintf("%.2f - %.2f °C\n",
													min(augmented$tmean, na.rm = TRUE),
													max(augmented$tmean, na.rm = TRUE)))
	cat("  Missing:", sum(is.na(augmented$tmean)), "plots\n\n")
} else {
	cat("  ⚠ PRISM tmean file not found, skipping\n\n")
	augmented$tmean <- NA
}

# =============================================================================
# EXTRACT PRISM PRECIPITATION
# =============================================================================

cat("Extracting PRISM precipitation (2020-2024)...\n")

ppt_path <- "data/raw/prism/prism_ppt_ne_2020_2024.tif"

if (file.exists(ppt_path)) {
	ppt <- rast(ppt_path)
  
	ppt_vals <- terra::extract(ppt, vect(augmented_sf), method = "bilinear")
	augmented$ppt <- ppt_vals[[2]]
  
	cat("  ✓ Extracted precipitation\n")
	cat("  Range:", sprintf("%.0f - %.0f mm\n",
													min(augmented$ppt, na.rm = TRUE),
													max(augmented$ppt, na.rm = TRUE)))
	cat("  Missing:", sum(is.na(augmented$ppt)), "plots\n\n")
} else {
	cat("  ⚠ PRISM ppt file not found, skipping\n\n")
	augmented$ppt <- NA
}

# =============================================================================
# SAVE ENRICHED DATASET
# =============================================================================

cat("Saving enriched augmented dataset...\n")

output_path <- "data/processed/augmented_with_covariates.csv"
write_csv(augmented, output_path)

cat("  ✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(augmented), "\n")
cat("  Columns:", ncol(augmented), "\n\n")

cat("Covariate summary:\n")
cat("  NDVI MODIS:  ", sum(!is.na(augmented$ndvi_modis)), "/ ", nrow(augmented),
		sprintf(" (%.1f%%)\n", 100 * mean(!is.na(augmented$ndvi_modis))))
cat("  NDVI S2:     ", sum(!is.na(augmented$ndvi_s2)), "/ ", nrow(augmented),
		sprintf(" (%.1f%%)\n", 100 * mean(!is.na(augmented$ndvi_s2))))
cat("  Temperature: ", sum(!is.na(augmented$tmean)), "/ ", nrow(augmented),
		sprintf(" (%.1f%%)\n", 100 * mean(!is.na(augmented$tmean))))
cat("  Precip:      ", sum(!is.na(augmented$ppt)), "/ ", nrow(augmented),
		sprintf(" (%.1f%%)\n", 100 * mean(!is.na(augmented$ppt))))

complete <- augmented %>%
	filter(!is.na(ndvi_modis), !is.na(ndvi_s2), !is.na(tmean), !is.na(ppt))

cat("\nPlots with all covariates:", nrow(complete), 
		sprintf(" (%.1f%%)\n\n", 100 * nrow(complete) / nrow(augmented)))

cat("✓ Augmented covariates extracted!\n")

