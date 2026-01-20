# =============================================================================
# Create Augmented Dataset (FIA + NEFIN)
# =============================================================================
# Combines FIA (fuzzed) with NEFIN (true coords) for comparison
#
# STRATEGY:
#   1. Check for overlapping plots (by CN)
#   2. For overlaps: use NEFIN version (true coords)
#   3. For FIA-only: keep fuzzed coords
#   4. Add all NEFIN plots
#
# INPUTS:
#   - data/processed/fia_complete.csv
#   - data/processed/nefin_complete.csv
#
# OUTPUTS:
#   - data/processed/augmented.csv
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
cat("  PHASE C - STEP 3: CREATE AUGMENTED DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load datasets
fia <- read_csv("data/processed/fia_complete.csv", show_col_types = FALSE)
nefin <- read_csv("data/processed/nefin_complete.csv", show_col_types = FALSE)

cat("FIA:  ", nrow(fia), "plots\n")
cat("NEFIN:", nrow(nefin), "plots\n\n")

# =============================================================================
# CHECK FOR OVERLAPPING PLOTS
# =============================================================================

cat("Checking for overlapping plots...\n")

overlap_cn <- intersect(fia$CN, nefin$CN)

if (length(overlap_cn) > 0) {
  cat("Found", length(overlap_cn), "overlapping plots\n")
  cat("  Using NEFIN version (true coords) for these plots\n\n")
  
  # Remove overlapping plots from FIA (we'll use NEFIN version instead)
  fia_unique <- fia %>%
    filter(!CN %in% overlap_cn)
  
  cat("After removing overlaps:\n")
  cat("  FIA unique:  ", nrow(fia_unique), "plots\n")
  cat("  NEFIN total: ", nrow(nefin), "plots (includes", length(overlap_cn), "overlaps)\n")
  
} else {
  cat("No overlapping plots - datasets are disjoint\n\n")
  fia_unique <- fia
}

# =============================================================================
# CREATE AUGMENTED DATASET
# =============================================================================

cat("\nCombining datasets...\n")

# Augmented = FIA (non-overlapping, fuzzed) + NEFIN (all, true coords)
augmented <- bind_rows(
  fia_unique,
  nefin
)

cat("\nAugmented dataset created:\n")
cat("  Total plots:", nrow(augmented), "\n")
cat("  FIA plots (fuzzed):  ", sum(augmented$dataset == "FIA"), "\n")
cat("  NEFIN plots (true):  ", sum(augmented$dataset == "NEFIN"), "\n")

# Verify coordinate sources
coord_summary <- augmented %>%
  count(dataset, coord_source)

cat("\nCoordinate source breakdown:\n")
print(coord_summary)

# =============================================================================
# VALIDATION
# =============================================================================

cat("\nValidation checks:\n")

# Check no duplicate CNs
if (any(duplicated(augmented$CN))) {
  stop("ERROR: Duplicate CNs found in augmented dataset!")
}
cat("✓ No duplicate CNs\n")

# Check all required columns
required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", "biomass",
                  "dataset", "coord_source", "lat_for_extraction", "lon_for_extraction")

missing <- setdiff(required_cols, names(augmented))
if (length(missing) > 0) {
  stop("Missing columns: ", paste(missing, collapse = ", "))
}
cat("✓ All required columns present\n")

# Check FIA has fuzzed, NEFIN has true
fia_check <- augmented %>% filter(dataset == "FIA")
nefin_check <- augmented %>% filter(dataset == "NEFIN")

if (!all(fia_check$coord_source == "fuzzed")) {
  stop("ERROR: Some FIA plots don't have fuzzed coords!")
}
if (!all(nefin_check$coord_source == "true")) {
  stop("ERROR: Some NEFIN plots don't have true coords!")
}
cat("✓ Coordinate sources correct\n")

# =============================================================================
# SAVE AUGMENTED DATASET
# =============================================================================

output_path <- "data/processed/augmented.csv"

write_csv(augmented, output_path)

cat("\n✓ Saved:", output_path, "\n")
cat("  Size:", file.size(output_path) / 1024^2, "MB\n")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

summary_path <- "data/processed/augmentation_summary.txt"

sink(summary_path)
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  AUGMENTED DATASET CREATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Created:", as.character(Sys.time()), "\n\n")

cat("INPUT DATASETS:\n")
cat("  FIA:   ", nrow(fia), "plots (all fuzzed)\n")
cat("  NEFIN: ", nrow(nefin), "plots (all true coords)\n\n")

cat("OVERLAP HANDLING:\n")
cat("  Overlapping CNs:     ", length(overlap_cn), "\n")
cat("  FIA plots removed:   ", length(overlap_cn), "\n")
cat("  NEFIN plots kept:    ", length(overlap_cn), "(true coords replace fuzzed)\n\n")

cat("OUTPUT DATASET:\n")
cat("  Total plots:         ", nrow(augmented), "\n")
cat("  FIA plots (fuzzed):  ", sum(augmented$dataset == "FIA"), "\n")
cat("  NEFIN plots (true):  ", sum(augmented$dataset == "NEFIN"), "\n\n")

cat("COORDINATE BREAKDOWN:\n")
cat("  Fuzzed coords (FIA): ", sum(augmented$coord_source == "fuzzed"), "\n")
cat("  True coords (NEFIN): ", sum(augmented$coord_source == "true"), "\n\n")

cat("COMPARISON DESIGN:\n")
cat("  Baseline:  FIA-only (", nrow(fia), "plots, all fuzzed)\n")
cat("  Augmented: FIA + NEFIN (", nrow(augmented), "plots, mixed coords)\n\n")

cat("RESEARCH QUESTION:\n")
cat("  Does augmenting FIA with NEFIN's precise coordinates\n")
cat("  improve forest biomass estimates at different scales?\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
sink()

cat("\n✓ Summary written:", summary_path, "\n")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Datasets created:\n")
cat("  1. baseline.csv   - FIA-only (", nrow(fia), "plots)\n")
cat("  2. augmented.csv  - FIA + NEFIN (", nrow(augmented), "plots)\n\n")

cat("Next: R/04_assign_to_hexagons/ (Phase D)\n\n")
