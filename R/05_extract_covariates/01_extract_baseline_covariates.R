# =============================================================================
# Extract Covariates for Baseline Dataset (UPDATED - Uses Phase 4 Config)
# =============================================================================
# Updated to:
# 1. Use Phase 4 covariate config for paths
# 2. Use scale-specific column names (e.g., ndvi_modis_250m)
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config_covariates.R")
source("R/utils/file_utils.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  EXTRACT BASELINE COVARIATES (Phase 4 Config + Naming)\n")
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
# EXTRACT COVARIATES USING PHASE 4 CONFIG
# =============================================================================

# Define which covariates to extract for Phase D
# (basic set: NDVI, temperature, precipitation)
covariates_to_extract <- c(
  "ndvi_modis",    # 250m MODIS NDVI
  "ndvi_s2",       # 10m Sentinel-2 NDVI
  "tmean_coarse",  # 250m temperature
  "ppt_coarse"     # 250m precipitation
)

cat("Extracting covariates from Phase 4 config...\n\n")

for (cov_key in covariates_to_extract) {
  
  # Get covariate info from config
  cov <- COVARIATES[[cov_key]]
  
  if (is.null(cov)) {
    cat("  ⚠ Covariate not found in config:", cov_key, "\n")
    next
  }
  
  if (!cov$active) {
    cat("  ⊘ Covariate not active:", cov$display_name, "\n")
    next
  }
  
  cat(sprintf("  Processing: %s (%s)...\n", cov$display_name, cov$resolution))
  
  # Check if file exists
  if (!file.exists(cov$path)) {
    cat(sprintf("    ⚠ File not found: %s\n", cov$path))
    cat(sprintf("    Creating NA column...\n\n"))
    
    # Create column with scale-specific name
    col_name <- paste0(cov$name, "_", gsub("m", "", cov$resolution), "m")
    baseline[[col_name]] <- NA
    next
  }
  
  # Load raster
  r <- rast(cov$path)
  
  # Extract values
  vals <- terra::extract(r, vect(baseline_sf), method = "bilinear")
  
  # Add to baseline with scale-specific name
  col_name <- paste0(cov$name, "_", gsub("m", "", cov$resolution), "m")
  baseline[[col_name]] <- vals[[2]]
  
  cat(sprintf("    ✓ Extracted → %s\n", col_name))
  cat(sprintf("    Range: %.3f - %.3f\n",
              min(baseline[[col_name]], na.rm = TRUE),
              max(baseline[[col_name]], na.rm = TRUE)))
  cat(sprintf("    Missing: %d plots\n\n", sum(is.na(baseline[[col_name]]))))
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

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  COVARIATE SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Get the column names that were created
extracted_cols <- grep("_(10m|250m)$", names(baseline), value = TRUE)

cat("Extracted covariates:\n")
for (col in extracted_cols) {
  n_complete <- sum(!is.na(baseline[[col]]))
  pct_complete <- 100 * n_complete / nrow(baseline)
  cat(sprintf("  %-20s %5d / %5d (%.1f%%)\n", 
              paste0(col, ":"), n_complete, nrow(baseline), pct_complete))
}

# Complete cases
if (length(extracted_cols) > 0) {
  complete <- baseline %>%
    select(all_of(extracted_cols)) %>%
    filter(complete.cases(.))
  
  cat("\nPlots with all covariates:", nrow(complete), 
      sprintf(" (%.1f%%)\n\n", 100 * nrow(complete) / nrow(baseline)))
}

cat("✓ Baseline covariates extracted using Phase 4 config!\n")
cat("Next: Extract augmented covariates\n")
cat("      Rscript R/05_extract_covariates/02_extract_augmented_covariates.R\n\n")