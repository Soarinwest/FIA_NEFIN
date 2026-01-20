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

source("R/00_config/config.R")
source("R/utils/validation_utils.R")

library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C - STEP 1: VALIDATE INPUTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load datasets
fia <- read_csv("data/processed/fia_complete.csv", show_col_types = FALSE)
nefin <- read_csv("data/processed/nefin_complete.csv", show_col_types = FALSE)

cat("FIA:  ", nrow(fia), "plots\n")
cat("NEFIN:", nrow(nefin), "plots\n\n")

# =============================================================================
# CHECK SCHEMAS MATCH
# =============================================================================

cat("Checking schemas...\n")

required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", "biomass",
                  "dataset", "coord_source", "lat_for_extraction", "lon_for_extraction")

validate_columns(fia, required_cols, "FIA")
validate_columns(nefin, required_cols, "NEFIN")

# =============================================================================
# CHECK COORDINATE SOURCES
# =============================================================================

cat("\nChecking coordinate sources...\n")

# FIA should be 100% fuzzed
if (!all(fia$coord_source == "fuzzed")) {
  stop("ERROR: Not all FIA coords marked as fuzzed!")
}
cat("✓ FIA: 100% fuzzed\n")

# NEFIN should be 100% true
if (!all(nefin$coord_source == "true")) {
  stop("ERROR: Not all NEFIN coords marked as true!")
}
cat("✓ NEFIN: 100% true coordinates\n")

# =============================================================================
# CHECK FOR OVERLAPPING PLOTS
# =============================================================================

cat("\nChecking for overlapping plots (by CN)...\n")

overlap_cn <- intersect(fia$CN, nefin$CN)

if (length(overlap_cn) > 0) {
  cat("⚠ Found", length(overlap_cn), "plots in both datasets\n")
  cat("  Will use NEFIN version (true coords) in augmented dataset\n")
} else {
  cat("✓ No overlapping plots - datasets are disjoint\n")
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================

report_path <- "data/processed/validation_report.txt"

sink(report_path)
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DATASET VALIDATION REPORT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Generated:", as.character(Sys.time()), "\n\n")

cat("FIA DATASET:\n")
cat("  Rows:", nrow(fia), "\n")
cat("  Coord source:", unique(fia$coord_source), "\n")
cat("  Dataset tag:", unique(fia$dataset), "\n\n")

cat("NEFIN DATASET:\n")
cat("  Rows:", nrow(nefin), "\n")
cat("  Coord source:", unique(nefin$coord_source), "\n")
cat("  Dataset tag:", unique(nefin$dataset), "\n\n")

cat("OVERLAP:\n")
cat("  Shared CNs:", length(overlap_cn), "\n")
cat("  Percentage of FIA:", sprintf("%.1f%%", 100 * length(overlap_cn) / nrow(fia)), "\n")
cat("  Percentage of NEFIN:", sprintf("%.1f%%", 100 * length(overlap_cn) / nrow(nefin)), "\n\n")

cat("SCHEMA VALIDATION:\n")
cat("  ✓ Both datasets have required columns\n")
cat("  ✓ Coordinate sources correctly tagged\n")
cat("  ✓ Ready for comparison\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
sink()

cat("\n✓ Validation complete\n")
cat("Report:", report_path, "\n")
cat("\nNext: R/03_create_comparison_datasets/02_create_baseline.R\n\n")
