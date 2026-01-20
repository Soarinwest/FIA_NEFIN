# =============================================================================
# Validate FIA and NEFIN Inputs
# =============================================================================
# Ensures both datasets are ready for comparison
#
# INPUTS:
#   - data/processed/fia_complete.csv
#   - data/processed/nefin_complete.csv
#
# OUTPUTS:
#   - data/processed/validation_report.txt
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================
# =============================================================================
# Validate Inputs for Comparison - FIXED
# =============================================================================
# FIX: Handle coord_source as either logical TRUE or string "true"
# =============================================================================

source("R/00_config/config.R")
source("R/utils/validation_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C - STEP 1: VALIDATE INPUTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD DATASETS
# =============================================================================

fia_path <- file.path(CONFIG$paths$processed, "fia_complete.csv")
nefin_path <- file.path(CONFIG$paths$processed, "nefin_complete.csv")

fia <- read_csv(fia_path, show_col_types = FALSE)
nefin <- read_csv(nefin_path, show_col_types = FALSE)

cat("FIA:  ", nrow(fia), "plots\n")
cat("NEFIN:", nrow(nefin), "plots\n\n")

# =============================================================================
# SCHEMA VALIDATION
# =============================================================================

cat("Checking schemas...\n")

required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", 
                   "lat_for_extraction", "lon_for_extraction",
                   "biomass", "dataset", "coord_source")

validate_columns(fia, required_cols, "FIA")
validate_columns(nefin, required_cols, "NEFIN")

# =============================================================================
# COORDINATE SOURCE VALIDATION - FIXED!
# =============================================================================

cat("\nChecking coordinate sources...\n")

# FIA: should be "fuzzed" (character) or FALSE/NULL (logical)
fia_fuzzed <- if (is.character(fia$coord_source)) {
  all(fia$coord_source == "fuzzed", na.rm = TRUE)
} else {
  # If logical, fuzzed might be FALSE or similar
  all(fia$coord_source == "fuzzed" | is.na(fia$coord_source), na.rm = TRUE)
}

if (!fia_fuzzed) {
  cat("  ⚠ FIA coord_source values:", paste(unique(fia$coord_source), collapse=", "), "\n")
  stop("ERROR: Not all FIA coords marked as fuzzed!")
}
cat("✓ FIA: 100% fuzzed\n")

# NEFIN: should be "true" (character) OR TRUE (logical) - FLEXIBLE!
nefin_true <- if (is.logical(nefin$coord_source)) {
  # Logical: should all be TRUE
  all(nefin$coord_source == TRUE, na.rm = TRUE)
} else if (is.character(nefin$coord_source)) {
  # Character: should all be "true"
  all(nefin$coord_source == "true", na.rm = TRUE)
} else {
  FALSE
}

if (!nefin_true) {
  cat("  ⚠ NEFIN coord_source type:", class(nefin$coord_source), "\n")
  cat("  ⚠ NEFIN coord_source values:", paste(unique(nefin$coord_source), collapse=", "), "\n")
  stop("ERROR: Not all NEFIN coords marked as true!")
}
cat("✓ NEFIN: 100% true\n")

# =============================================================================
# DATASET VALIDATION
# =============================================================================

cat("\nChecking dataset labels...\n")

if (!all(fia$dataset == "FIA", na.rm = TRUE)) {
  stop("ERROR: Not all FIA rows marked as FIA!")
}
cat("✓ FIA: All marked as 'FIA'\n")

if (!all(nefin$dataset == "NEFIN", na.rm = TRUE)) {
  stop("ERROR: Not all NEFIN rows marked as NEFIN!")
}
cat("✓ NEFIN: All marked as 'NEFIN'\n")

# =============================================================================
# OVERLAP CHECK
# =============================================================================

cat("\nChecking for overlapping plots...\n")

# Check if any CNs overlap (they might!)
overlap <- intersect(fia$CN, nefin$CN)

if (length(overlap) > 0) {
  cat("  ⚠ Found", length(overlap), "plots in both datasets\n")
  cat("    These will use NEFIN version in augmented dataset\n")
} else {
  cat("  No overlapping plots\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  VALIDATION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Datasets validated:\n")
cat("  FIA:   ", nrow(fia), "plots (fuzzed coords)\n")
cat("  NEFIN: ", nrow(nefin), "plots (true coords)\n")
if (length(overlap) > 0) {
  cat("  Overlap:", length(overlap), "plots\n")
}

cat("\n✓ Ready to create comparison datasets\n\n")