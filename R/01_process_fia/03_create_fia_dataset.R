# =============================================================================
# Create Final Clean FIA Dataset
# =============================================================================
# Standardizes FIA data to common schema for comparison with NEFIN
#
# INPUTS:
#   - data/interim/fia/biomass/fia_plot_biomass.csv
#
# OUTPUTS:
#   - data/processed/fia_complete.csv
#   - data/processed/fia_processing_report.txt
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

# Load configuration and utilities
source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/biomass_utils.R")
source("R/utils/validation_utils.R")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  STEP 3: CREATE FINAL FIA DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Input path
input_path <- file.path(CONFIG$paths$interim_fia, "biomass", "fia_plot_biomass.csv")

# Output path
output_path <- file.path(CONFIG$paths$processed, "fia_complete.csv")

# Ensure output directory exists
ensure_dir(CONFIG$paths$processed)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading FIA plot biomass data...\n\n")

fia_raw <- load_csv_validated(
  input_path,
  required_cols = c("CN", "lat", "lon", "biomass", "MEASYEAR", "STATECD")
)

cat("Input plots:", nrow(fia_raw), "\n")

# =============================================================================
# STANDARDIZE TO COMMON SCHEMA
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Standardizing to Common Schema\n")
cat("───────────────────────────────────────\n\n")

fia_standardized <- fia_raw %>%
  mutate(
    # Dataset identifier
    dataset = "FIA",
    
    # Coordinate source (FIA coordinates are ALWAYS fuzzed)
    coord_source = "fuzzed",
    
    # Coordinates for covariate extraction
    # For FIA, these are the fuzzed coordinates
    lat_for_extraction = lat,
    lon_for_extraction = lon,
    
    # Keep original coordinates for reference
    lat_original = lat,
    lon_original = lon
  ) %>%
  # Select and order columns according to standard schema
  select(
    # Identifiers
    CN,
    STATECD,
    COUNTYCD,
    PLOT,
    
    # Temporal
    MEASYEAR,
    
    # Spatial
    lat,
    lon,
    lat_original,
    lon_original,
    lat_for_extraction,
    lon_for_extraction,
    
    # Biomass
    biomass,
    n_trees,
    
    # Metadata
    dataset,
    coord_source,
    
    # Keep any other columns
    everything()
  )

cat("Standardized columns:", ncol(fia_standardized), "\n")
cat("Rows:", nrow(fia_standardized), "\n")

# =============================================================================
# FINAL VALIDATION
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Final Validation\n")
cat("───────────────────────────────────────\n\n")

# Run comprehensive validation
validation_report <- run_validation_suite(
  fia_standardized,
  dataset_name = "FIA Complete",
  required_cols = CONFIG$standard_columns,
  id_cols = "CN"
)

# Additional FIA-specific checks
cat("\nFIA-specific checks:\n")

# 1. All coordinates should be fuzzed
if (!all(fia_standardized$coord_source == "fuzzed")) {
  stop("ERROR: Not all FIA coordinates marked as fuzzed!")
}
cat("✓ All coordinates marked as fuzzed\n")

# 2. Check year range
year_range <- range(fia_standardized$MEASYEAR)
if (year_range[1] < CONFIG$year_start || year_range[2] > CONFIG$year_end) {
  warning("Some plots outside configured year range")
}
cat("✓ Year range:", year_range[1], "-", year_range[2], "\n")

# 3. Check state coverage
states_present <- unique(fia_standardized$STATECD)
expected_codes <- unlist(CONFIG$state_codes[CONFIG$states])
missing_states <- setdiff(expected_codes, states_present)

if (length(missing_states) > 0) {
  warning("Missing data from some states: ", 
          paste(names(CONFIG$state_codes)[match(missing_states, CONFIG$state_codes)], 
                collapse = ", "))
} else {
  cat("✓ All states represented\n")
}

# =============================================================================
# SAVE OUTPUT
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Saving Final Dataset\n")
cat("───────────────────────────────────────\n\n")

# Save with metadata
metadata <- list(
  dataset = "FIA",
  n_plots = nrow(fia_standardized),
  n_states = length(unique(fia_standardized$STATECD)),
  year_range = paste(range(fia_standardized$MEASYEAR), collapse = "-"),
  coord_source = "fuzzed",
  processing_date = as.character(Sys.time())
)

save_with_metadata(
  fia_standardized,
  output_path,
  metadata = metadata,
  overwrite = TRUE
)

# =============================================================================
# PROCESSING REPORT
# =============================================================================

report_path <- file.path(CONFIG$paths$processed, "fia_processing_report.txt")

sink(report_path)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  FIA DATA PROCESSING REPORT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Processing completed:", as.character(Sys.time()), "\n")
cat("Configuration version:", CONFIG$version, "\n\n")

cat("PROCESSING STEPS:\n\n")
cat("  1. Extract from SQLite → data/interim/fia/extracted/\n")
cat("  2. Compute biomass     → data/interim/fia/biomass/\n")
cat("  3. Standardize schema  → data/processed/fia_complete.csv\n\n")

cat("FINAL DATASET:\n\n")
cat("  Output file:", output_path, "\n")
cat("  File size:  ", get_file_size(output_path), "\n")
cat("  Rows:       ", nrow(fia_standardized), "\n")
cat("  Columns:    ", ncol(fia_standardized), "\n\n")

cat("SPATIAL COVERAGE:\n\n")
cat("  States:", paste(CONFIG$states, collapse = ", "), "\n")
cat("  Latitude range: ", sprintf("%.4f to %.4f", 
                                   min(fia_standardized$lat), 
                                   max(fia_standardized$lat)), "\n")
cat("  Longitude range:", sprintf("%.4f to %.4f", 
                                   min(fia_standardized$lon), 
                                   max(fia_standardized$lon)), "\n\n")

cat("TEMPORAL COVERAGE:\n\n")
by_year <- fia_standardized %>%
  count(MEASYEAR) %>%
  arrange(MEASYEAR)

for (i in 1:nrow(by_year)) {
  cat(sprintf("  %d: %5d plots (%.1f%%)\n",
              by_year$MEASYEAR[i],
              by_year$n[i],
              100 * by_year$n[i] / nrow(fia_standardized)))
}

cat("\n")
cat("BIOMASS SUMMARY:\n\n")
stats <- summarize_biomass(fia_standardized, "biomass")
cat(sprintf("  Mean:   %.2f Mg/ha\n", stats$mean))
cat(sprintf("  Median: %.2f Mg/ha\n", stats$median))
cat(sprintf("  SD:     %.2f Mg/ha\n", stats$sd))
cat(sprintf("  Range:  %.2f - %.2f Mg/ha\n", stats$min, stats$max))
cat(sprintf("  IQR:    %.2f - %.2f Mg/ha\n", stats$q25, stats$q75))

cat("\n")
cat("DATA QUALITY:\n\n")
cat("  Coordinate source:    100% fuzzed (±", CONFIG$fia$fuzz_distance_km, "km)\n")
cat("  Missing coordinates:  ", sum(is.na(fia_standardized$lat)), "\n")
cat("  Missing biomass:      ", sum(is.na(fia_standardized$biomass)), "\n")
cat("  Duplicate CNs:        ", sum(duplicated(fia_standardized$CN)), "\n")

cat("\n")
cat("STANDARD COLUMNS PRESENT:\n\n")
for (col in CONFIG$standard_columns) {
  present <- col %in% names(fia_standardized)
  cat(sprintf("  %s %-25s %s\n", 
              ifelse(present, "✓", "✗"),
              col,
              ifelse(present, "", "MISSING")))
}

cat("\n")
cat("NEXT STEPS:\n\n")
cat("  1. Process NEFIN data:           R/02_process_nefin/\n")
cat("  2. Create comparison datasets:   R/03_create_comparison_datasets/\n")
cat("  3. Assign to hexagons:           R/04_assign_to_hexagons/\n")
cat("  4. Extract covariates:           R/05_extract_covariates/\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")

sink()

cat("✓ Report written:", report_path, "\n")

# =============================================================================
# COMPLETE
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  FIA PROCESSING COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print_biomass_summary(fia_standardized)

cat("\n")
cat("Final output: ", output_path, "\n")
cat("Ready for: R/02_process_nefin/ pipeline\n\n")
