# =============================================================================
# PHASE 4 - EXTRACT COVARIATES (FIXED - CRS HANDLING)
# =============================================================================
# Extracts all ACTIVE covariates defined in config to plot locations
# FIXED: Explicit CRS handling to avoid PROJ conflicts
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(terra)
library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: EXTRACT COVARIATES TO PLOTS (FIXED CRS)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# CHECK COVARIATE AVAILABILITY
# =============================================================================

cat("Step 1: Checking covariate availability...\n\n")

availability <- check_covariate_availability()

# Show which are active
active_covs <- availability %>% filter(active == TRUE)
cat("Active covariates:", nrow(active_covs), "\n")
print(active_covs %>% select(display_name, exists, resolution, type), 
      row.names = FALSE)
cat("\n")

# Check which files are missing
missing <- active_covs %>% filter(exists == FALSE)
if (nrow(missing) > 0) {
  cat("⚠ WARNING: Some active covariates are missing files:\n")
  print(missing %>% select(display_name, path), row.names = FALSE)
  cat("\nThese will be skipped. Set active=FALSE in config or add files.\n\n")
}

# Get available covariates
available_covs <- active_covs %>% 
  filter(exists == TRUE) %>%
  pull(name)

cat("Extracting", length(available_covs), "covariates:\n")
for (cov_name in available_covs) {
  cov <- COVARIATES[[cov_name]]
  cat(sprintf("  • %s (%s)\n", cov$display_name, cov$resolution))
}
cat("\n")

# =============================================================================
# LOAD PLOT DATA
# =============================================================================

cat("Step 2: Loading plot locations...\n")

# Load baseline (FIA) - FIXED FILENAME
baseline <- read_csv("data/processed/baseline.csv", 
                     show_col_types = FALSE)

# Load augmented (FIA + NEFIN) - FIXED FILENAME
augmented <- read_csv("data/processed/augmented.csv",
                      show_col_types = FALSE)

cat("  ✓ Baseline plots:", nrow(baseline), "\n")
cat("  ✓ Augmented plots:", nrow(augmented), "\n\n")

# =============================================================================
# CREATE SPATIAL POINTS WITH EXPLICIT CRS
# =============================================================================

cat("Step 3: Creating spatial points...\n")

# Create data frames with coordinates
baseline_coords <- baseline %>%
  select(CN, lon, lat) %>%
  filter(!is.na(lon), !is.na(lat))

augmented_coords <- augmented %>%
  select(CN, lon, lat) %>%
  filter(!is.na(lon), !is.na(lat))

# Convert to spatial points with EXPLICIT CRS (WGS84 = EPSG:4326)
baseline_pts <- vect(baseline_coords, 
                     geom = c("lon", "lat"), 
                     crs = "+proj=longlat +datum=WGS84 +no_defs")

augmented_pts <- vect(augmented_coords, 
                      geom = c("lon", "lat"), 
                      crs = "+proj=longlat +datum=WGS84 +no_defs")

cat("  ✓ Created spatial points with WGS84 CRS\n\n")

# =============================================================================
# EXTRACT COVARIATES
# =============================================================================

cat("Step 4: Extracting covariates...\n\n")

# Storage for extracted values
baseline_extracted <- data.frame(CN = baseline_coords$CN)
augmented_extracted <- data.frame(CN = augmented_coords$CN)

for (cov_name in available_covs) {
  cov <- COVARIATES[[cov_name]]
  
  cat(sprintf("  Extracting %s...\n", cov$display_name))
  
  # Load raster
  if (!file.exists(cov$path)) {
    cat("    ⚠ File not found, skipping\n")
    next
  }
  
  tryCatch({
    # Load raster
    rast_data <- rast(cov$path)
    
    # Check if raster has valid CRS
    if (is.na(crs(rast_data))) {
      cat("    ⚠ Raster has no CRS, assuming WGS84\n")
      crs(rast_data) <- "+proj=longlat +datum=WGS84 +no_defs"
    }
    
    # Project points to raster CRS
    baseline_pts_proj <- project(baseline_pts, crs(rast_data))
    augmented_pts_proj <- project(augmented_pts, crs(rast_data))
    
    # Extract to baseline
    baseline_vals <- extract(rast_data, baseline_pts_proj)
    baseline_extracted[[cov_name]] <- baseline_vals[, 2]  # First column is ID
    
    # Extract to augmented
    augmented_vals <- extract(rast_data, augmented_pts_proj)
    augmented_extracted[[cov_name]] <- augmented_vals[, 2]
    
    # Summary stats
    n_na_base <- sum(is.na(baseline_extracted[[cov_name]]))
    n_na_aug <- sum(is.na(augmented_extracted[[cov_name]]))
    
    cat(sprintf("    ✓ Extracted (baseline: %d NA, augmented: %d NA)\n", 
                n_na_base, n_na_aug))
    
  }, error = function(e) {
    cat(sprintf("    ✗ Error: %s\n", e$message))
    # Add NA column if extraction failed
    baseline_extracted[[cov_name]] <<- NA
    augmented_extracted[[cov_name]] <<- NA
  })
}

cat("\n")

# =============================================================================
# MERGE WITH PLOT DATA
# =============================================================================

cat("Step 5: Merging with plot data...\n")

# Join extracted covariates with original data
baseline_complete <- baseline %>%
  left_join(baseline_extracted, by = "CN")

augmented_complete <- augmented %>%
  left_join(augmented_extracted, by = "CN", relationship = "many-to-many")

cat("  ✓ Baseline:", nrow(baseline_complete), "plots with", 
    length(available_covs), "covariates\n")
cat("  ✓ Augmented:", nrow(augmented_complete), "plots with", 
    length(available_covs), "covariates\n\n")

# =============================================================================
# QUALITY CHECK
# =============================================================================

cat("Step 6: Quality check...\n\n")

cat("  Missing data summary:\n")
for (cov_name in available_covs) {
  n_missing_base <- sum(is.na(baseline_complete[[cov_name]]))
  n_missing_aug <- sum(is.na(augmented_complete[[cov_name]]))
  pct_missing_base <- 100 * n_missing_base / nrow(baseline_complete)
  pct_missing_aug <- 100 * n_missing_aug / nrow(augmented_complete)
  
  cov <- COVARIATES[[cov_name]]
  cat(sprintf("    %s: %.1f%% (baseline), %.1f%% (augmented)\n",
              cov$display_name, pct_missing_base, pct_missing_aug))
}

cat("\n")

# Check for complete cases
if (length(available_covs) > 0) {
  complete_base <- sum(complete.cases(baseline_complete[, available_covs]))
  complete_aug <- sum(complete.cases(augmented_complete[, available_covs]))
  
  cat(sprintf("  Complete cases (no missing covariates):\n"))
  cat(sprintf("    Baseline: %d of %d (%.1f%%)\n", 
              complete_base, nrow(baseline_complete),
              100 * complete_base / nrow(baseline_complete)))
  cat(sprintf("    Augmented: %d of %d (%.1f%%)\n\n",
              complete_aug, nrow(augmented_complete),
              100 * complete_aug / nrow(augmented_complete)))
}

# =============================================================================
# SAVE UPDATED DATASETS
# =============================================================================

cat("Step 7: Saving updated datasets...\n")

# Backup originals if they exist
if (file.exists("data/processed/baseline_with_covariates.csv")) {
  file.copy("data/processed/baseline_with_covariates.csv",
            "data/processed/baseline_with_covariates_backup.csv",
            overwrite = TRUE)
  cat("  ✓ Backed up existing baseline_with_covariates.csv\n")
}
if (file.exists("data/processed/augmented_with_covariates.csv")) {
  file.copy("data/processed/augmented_with_covariates.csv",
            "data/processed/augmented_with_covariates_backup.csv",
            overwrite = TRUE)
  cat("  ✓ Backed up existing augmented_with_covariates.csv\n")
}

# Save new versions
write_csv(baseline_complete, "data/processed/baseline_with_covariates.csv")
write_csv(augmented_complete, "data/processed/augmented_with_covariates.csv")

cat("  ✓ baseline_with_covariates.csv\n")
cat("  ✓ augmented_with_covariates.csv\n\n")

# Save extraction metadata
extraction_meta <- data.frame(
  covariate = available_covs,
  display_name = sapply(COVARIATES[available_covs], function(x) x$display_name),
  resolution = sapply(COVARIATES[available_covs], function(x) x$resolution),
  type = sapply(COVARIATES[available_covs], function(x) x$type),
  path = sapply(COVARIATES[available_covs], function(x) x$path),
  extraction_date = Sys.time(),
  stringsAsFactors = FALSE
)

write_csv(extraction_meta, "data/processed/covariate_extraction_metadata.csv")
cat("  ✓ Extraction metadata saved\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  COVARIATE EXTRACTION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Extracted covariates:", length(available_covs), "\n")
for (cov_name in available_covs) {
  cov <- COVARIATES[[cov_name]]
  cat(sprintf("  • %s (%s, %s)\n", 
              cov$display_name, cov$resolution, cov$type))
}
cat("\n")

cat("Output files:\n")
cat("  • data/processed/baseline_with_covariates.csv\n")
cat("  • data/processed/augmented_with_covariates.csv\n")
cat("  • data/processed/covariate_extraction_metadata.csv\n\n")

cat("Next step: Prepare data for modeling\n")
cat("  Rscript R/phase4_modeling/PHASE4_01_prep_data.R\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")
