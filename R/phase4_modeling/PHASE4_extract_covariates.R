# =============================================================================
# PHASE 4 - EXTRACT COVARIATES (SCALE-SPECIFIC VERSION)
# =============================================================================
# Extracts all ACTIVE covariates defined in config to plot locations
# NOW SUPPORTS SCALE-SPECIFIC ORGANIZATION (fine_10m/ and coarse_250m/)
# 
# Key features:
# - Extracts from scale-specific directories
# - Handles CRS transformations properly
# - Creates single dataset with all scales combined
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(terra)
library(sf)
library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: EXTRACT COVARIATES (SCALE-SPECIFIC NAMING)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# CHECK ACTIVE COVARIATES
# =============================================================================

cat("Step 1: Checking covariate availability...\n\n")

# Get active covariates
active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)

# Separate by scale
fine_covs <- Filter(function(x) x$scale == "fine", active_covs)
coarse_covs <- Filter(function(x) x$scale == "coarse", active_covs)

cat("Active covariates:", length(active_covs), "\n\n")
cat("By scale:\n")
cat("  FINE (10m):", length(fine_covs), "covariates\n")
for (cov in fine_covs) {
  cat("    ✓", cov$display_name, "\n")
}
cat("  COARSE (250m):", length(coarse_covs), "covariates\n")
for (cov in coarse_covs) {
  cat("    ✓", cov$display_name, "\n")
}
cat("\n")

# =============================================================================
# LOAD PLOT DATA
# =============================================================================

cat("Step 2: Loading plot locations...\n")

baseline <- read_csv("data/processed/baseline.csv", show_col_types = FALSE)
augmented <- read_csv("data/processed/augmented.csv", show_col_types = FALSE)

cat("  ✓ Baseline plots:", nrow(baseline), "\n")
cat("  ✓ Augmented plots:", nrow(augmented), "\n\n")

# =============================================================================
# CREATE SPATIAL POINTS
# =============================================================================

cat("Step 3: Creating spatial points...\n")

# Function to create sf points
create_spatial_points <- function(data, name) {
  data_clean <- data %>%
    filter(!is.na(lon), !is.na(lat)) %>%
    filter(lon >= -180, lon <= 180, lat >= -90, lat <= 90)
  
  if (nrow(data_clean) < nrow(data)) {
    cat("  ⚠ Removed", nrow(data) - nrow(data_clean), "plots with invalid coordinates\n")
  }
  
  data_sf <- st_as_sf(
    data_clean,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  cat("  ✓", name, ":", nrow(data_sf), "plots with valid coordinates\n")
  return(data_sf)
}

baseline_sf <- create_spatial_points(baseline, "Baseline")
augmented_sf <- create_spatial_points(augmented, "Augmented")

cat("  ✓ Created spatial points with WGS84 CRS\n\n")

# =============================================================================
# EXTRACT COVARIATES WITH SCALE-SPECIFIC NAMES
# =============================================================================

cat("Step 4: Extracting covariates with scale-specific naming...\n\n")

# Initialize results data frames
baseline_extracted <- baseline %>% select(CN)
augmented_extracted <- augmented %>% select(CN)

# Function to extract with scale-specific column name
extract_covariate <- function(cov, data_sf, data_name) {
  
  cat("  Extracting", cov$display_name, "(", cov$resolution, ",", cov$scale, "scale)...\n")
  
  # Check if raster file exists
  if (!file.exists(cov$path)) {
    cat("    ⚠ File not found:", cov$path, "\n")
    cat("    Skipping...\n\n")
    return(NULL)
  }
  
  # Load raster
  tryCatch({
    rast <- rast(cov$path)
    
    # Extract values
    extracted <- terra::extract(rast, data_sf, ID = FALSE)
    
    # Create SCALE-SPECIFIC column name
    # Format: covariate_scale (e.g., elevation_10m, elevation_250m)
    col_name <- paste0(cov$name, "_", gsub("m", "", cov$resolution), "m")
    
    # Rename column
    if (ncol(extracted) == 1) {
      colnames(extracted) <- col_name
    } else {
      cat("    ⚠ Multiple bands detected, using first band\n")
      extracted <- extracted[, 1, drop = FALSE]
      colnames(extracted) <- col_name
    }
    
    # Check extraction
    n_na <- sum(is.na(extracted[[col_name]]))
    pct_na <- round(100 * n_na / nrow(extracted), 1)
    val_range <- range(extracted[[col_name]], na.rm = TRUE)
    
    cat("    ✓ Extracted as '", col_name, "'\n", sep = "")
    cat("     ", data_name, ":", n_na, "NA (", pct_na, "%), range: [",
        round(val_range[1], 2), ", ", round(val_range[2], 2), "]\n", sep = "")
    
    return(extracted)
    
  }, error = function(e) {
    cat("    ✗ ERROR:", e$message, "\n\n")
    return(NULL)
  })
}

# Extract FINE scale covariates
cat("FINE SCALE (10m) COVARIATES:\n")
for (cov in fine_covs) {
  baseline_result <- extract_covariate(cov, baseline_sf, "Baseline")
  augmented_result <- extract_covariate(cov, augmented_sf, "Augmented")
  
  if (!is.null(baseline_result)) {
    baseline_extracted <- bind_cols(baseline_extracted, baseline_result)
  }
  if (!is.null(augmented_result)) {
    augmented_extracted <- bind_cols(augmented_extracted, augmented_result)
  }
}

cat("\nCOARSE SCALE (250m) COVARIATES:\n")
for (cov in coarse_covs) {
  baseline_result <- extract_covariate(cov, baseline_sf, "Baseline")
  augmented_result <- extract_covariate(cov, augmented_sf, "Augmented")
  
  if (!is.null(baseline_result)) {
    baseline_extracted <- bind_cols(baseline_extracted, baseline_result)
  }
  if (!is.null(augmented_result)) {
    augmented_extracted <- bind_cols(augmented_extracted, augmented_result)
  }
}

cat("\nExtraction summary:\n")
cat("  ✓ Baseline:", ncol(baseline_extracted) - 1, "covariates extracted\n")
cat("  ✓ Augmented:", ncol(augmented_extracted) - 1, "covariates extracted\n\n")

# =============================================================================
# MERGE WITH PLOT DATA
# =============================================================================

cat("Step 5: Merging with plot data...\n")

# Merge using left_join (one-to-one, should not create duplicates)
baseline_with_covs <- baseline %>%
  left_join(baseline_extracted, by = "CN")

augmented_with_covs <- augmented %>%
  left_join(augmented_extracted, by = "CN")

cat("  ✓ Baseline:", nrow(baseline_with_covs), "plots with", 
    ncol(baseline_with_covs) - ncol(baseline), "covariates\n")
cat("  ✓ Augmented:", nrow(augmented_with_covs), "plots with", 
    ncol(augmented_with_covs) - ncol(augmented), "covariates\n\n")

# Check for unexpected row expansion
if (nrow(baseline_with_covs) != nrow(baseline)) {
  cat("  ⚠ WARNING: Baseline row count changed during merge!\n")
  cat("    Before:", nrow(baseline), "After:", nrow(baseline_with_covs), "\n\n")
}

if (nrow(augmented_with_covs) != nrow(augmented)) {
  cat("  ⚠ WARNING: Augmented row count changed during merge!\n")
  cat("    Before:", nrow(augmented), "After:", nrow(augmented_with_covs), "\n\n")
}

# =============================================================================
# QUALITY CHECK
# =============================================================================

cat("Step 6: Quality check...\n\n")

# Get covariate column names (exclude plot metadata)
metadata_cols <- c("CN", "dataset", "biomass", "lon", "lat", "plot_id", "year", 
                   "state", "county", "elev", "physio")
cov_cols <- setdiff(names(augmented_with_covs), metadata_cols)

cat("  Extracted covariate columns:\n")
for (col in cov_cols) {
  cat("    •", col, "\n")
}
cat("\n")

# Identify fine vs coarse covariates
fine_cov_names <- cov_cols[grepl("_10m$", cov_cols)]
coarse_cov_names <- cov_cols[grepl("_250m$", cov_cols)]

cat("  Complete cases by scale:\n")
cat("    FINE (10m):", length(fine_cov_names), "covariates\n")
cat("    COARSE (250m):", length(coarse_cov_names), "covariates\n\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("Step 7: Saving updated datasets...\n")

# Backup existing files
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

# Save
write_csv(baseline_with_covs, "data/processed/baseline_with_covariates.csv")
write_csv(augmented_with_covs, "data/processed/augmented_with_covariates.csv")

cat("  ✓ baseline_with_covariates.csv\n")
cat("  ✓ augmented_with_covariates.csv\n\n")

# Save metadata
metadata <- data.frame(
  parameter = c("extraction_date", "n_baseline", "n_augmented", 
                "n_covariates", "fine_scale_covariates", "coarse_scale_covariates"),
  value = c(as.character(Sys.time()), 
            nrow(baseline_with_covs), 
            nrow(augmented_with_covs),
            length(cov_cols),
            length(fine_cov_names),
            length(coarse_cov_names))
)

write_csv(metadata, "data/processed/covariate_extraction_metadata.csv")
cat("  ✓ Extraction metadata saved\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  COVARIATE EXTRACTION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Extracted covariates:", length(cov_cols), "\n\n")

cat("FINE scale (10m):", length(fine_cov_names), "covariates\n")
for (col in fine_cov_names) {
  cat("  •", col, "\n")
}
cat("\n")

cat("COARSE scale (250m):", length(coarse_cov_names), "covariates\n")
for (col in coarse_cov_names) {
  cat("  •", col, "\n")
}
cat("\n")

cat("Output files:\n")
cat("  • data/processed/baseline_with_covariates.csv\n")
cat("  • data/processed/augmented_with_covariates.csv\n")
cat("  • data/processed/covariate_extraction_metadata.csv\n\n")

cat("Row counts (should match input):\n")
cat("  Baseline: ", nrow(baseline), " → ", nrow(baseline_with_covs), 
    ifelse(nrow(baseline) == nrow(baseline_with_covs), " ✓", " ⚠"), "\n", sep = "")
cat("  Augmented: ", nrow(augmented), " → ", nrow(augmented_with_covs),
    ifelse(nrow(augmented) == nrow(augmented_with_covs), " ✓", " ⚠"), "\n\n", sep = "")

cat("Next steps:\n")
cat("  1. Prepare data for modeling:\n")
cat("     Rscript R/phase4_modeling/PHASE4_01_prep_data.R\n")
cat("  2. Run spatial cross-validation:\n")
cat("     Rscript R/phase4_modeling/PHASE4_02b_spatial_cv.R\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")