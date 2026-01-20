# =============================================================================
# Create Baseline Dataset (FIA-Only)
# =============================================================================
# Creates FIA-only dataset as comparison baseline
#
# INPUTS:
#   - data/processed/fia_complete.csv
#
# OUTPUTS:
#   - data/processed/baseline.csv
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C - STEP 2: CREATE BASELINE DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load FIA
fia <- read_csv("data/processed/fia_complete.csv", show_col_types = FALSE)

cat("Input: FIA complete -", nrow(fia), "plots\n")

# =============================================================================
# CREATE BASELINE
# =============================================================================

# Baseline is simply FIA data (all fuzzed coordinates)
# No changes needed - it's already in the correct format

baseline <- fia

# Verify all required columns exist
required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", "biomass",
                  "dataset", "coord_source", "lat_for_extraction", "lon_for_extraction")

missing <- setdiff(required_cols, names(baseline))
if (length(missing) > 0) {
  stop("Missing columns: ", paste(missing, collapse = ", "))
}

# Verify coordinate source
if (!all(baseline$coord_source == "fuzzed")) {
  stop("ERROR: Baseline must have 100% fuzzed coordinates")
}

cat("\nBaseline dataset created:\n")
cat("  Rows:", nrow(baseline), "\n")
cat("  Dataset:", unique(baseline$dataset), "\n")
cat("  Coord source:", unique(baseline$coord_source), "\n")

# =============================================================================
# SAVE BASELINE
# =============================================================================

output_path <- "data/processed/baseline.csv"

write_csv(baseline, output_path)

cat("\n✓ Saved:", output_path, "\n")
cat("  Size:", file.size(output_path) / 1024^2, "MB\n")

cat("\nNext: R/03_create_comparison_datasets/03_create_augmented.R\n\n")
