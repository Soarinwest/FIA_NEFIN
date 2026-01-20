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
# =============================================================================
# Create Augmented Dataset - FIXED v2
# =============================================================================
# FIX: Convert both CN and coord_source to character before combining
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C - STEP 3: CREATE AUGMENTED DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD DATASETS
# =============================================================================

fia <- read_csv(file.path(CONFIG$paths$processed, "fia_complete.csv"), 
                show_col_types = FALSE)
nefin <- read_csv(file.path(CONFIG$paths$processed, "nefin_complete.csv"), 
                  show_col_types = FALSE)

cat("FIA:  ", nrow(fia), "plots\n")
cat("NEFIN:", nrow(nefin), "plots\n\n")

# =============================================================================
# STANDARDIZE COLUMN TYPES - CRITICAL FIX!
# =============================================================================

cat("Standardizing column types...\n")

# Convert CN to character (it's an identifier, not a number)
fia <- fia %>% mutate(CN = as.character(CN))
nefin <- nefin %>% mutate(CN = as.character(CN))

# Convert coord_source to character
# FIA: already "fuzzed" (character)
# NEFIN: TRUE (logical) → "true" (character)
fia <- fia %>% mutate(coord_source = as.character(coord_source))
nefin <- nefin %>% mutate(coord_source = as.character(coord_source))

# Convert dataset to character (should already be, but make sure)
fia <- fia %>% mutate(dataset = as.character(dataset))
nefin <- nefin %>% mutate(dataset = as.character(dataset))

cat("  ✓ All columns standardized to compatible types\n\n")

# =============================================================================
# CHECK FOR OVERLAPS
# =============================================================================

cat("Checking for overlapping plots...\n")

overlap <- intersect(fia$CN, nefin$CN)

if (length(overlap) > 0) {
  cat("  Found", length(overlap), "overlapping plots\n")
  cat("  Strategy: Use NEFIN version (true coords) for overlaps\n\n")
  
  # Remove overlapping plots from FIA (keep NEFIN version)
  fia_unique <- fia %>% filter(!CN %in% overlap)
  
  cat("  FIA plots after removing overlaps:", nrow(fia_unique), "\n")
  cat("  NEFIN plots (all):", nrow(nefin), "\n\n")
} else {
  cat("  No overlapping plots - datasets are disjoint\n\n")
  fia_unique <- fia
}

# =============================================================================
# COMBINE DATASETS
# =============================================================================

cat("Combining datasets...\n")

augmented <- bind_rows(fia_unique, nefin)

cat("  ✓ Combined:", nrow(augmented), "plots\n")
cat("  Expected:", nrow(fia_unique) + nrow(nefin), "\n\n")

# =============================================================================
# VALIDATE AUGMENTED DATASET
# =============================================================================

cat("Validating augmented dataset...\n")

# Check composition
cat("\n  Dataset distribution:\n")
print(table(augmented$dataset))

cat("\n  Coordinate source distribution:\n")
print(table(augmented$coord_source))

# Check for duplicates
n_dup <- sum(duplicated(augmented$CN))
if (n_dup > 0) {
  cat("\n  ⚠ WARNING:", n_dup, "duplicate CNs found\n")
} else {
  cat("\n  ✓ No duplicate CNs\n")
}

# Summary stats
cat("\nBiomass summary:\n")
cat(sprintf("  Overall mean: %.2f Mg/ha\n", mean(augmented$biomass, na.rm = TRUE)))
cat(sprintf("  FIA mean:     %.2f Mg/ha\n", 
            mean(augmented$biomass[augmented$dataset == "FIA"], na.rm = TRUE)))
cat(sprintf("  NEFIN mean:   %.2f Mg/ha\n", 
            mean(augmented$biomass[augmented$dataset == "NEFIN"], na.rm = TRUE)))

# =============================================================================
# SAVE AUGMENTED DATASET
# =============================================================================

cat("\n")
output_path <- file.path(CONFIG$paths$processed, "augmented.csv")
write_csv(augmented, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(augmented), "\n")
cat("  Size:", sprintf("%.2f MB\n", file.size(output_path) / 1024^2))

# =============================================================================
# SUMMARY REPORT
# =============================================================================

summary_path <- file.path(CONFIG$paths$processed, "augmentation_summary.txt")

sink(summary_path)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  AUGMENTED DATASET SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Created:", as.character(Sys.time()), "\n\n")

cat("INPUT DATASETS:\n")
cat("  FIA complete:   ", nrow(fia), "plots\n")
cat("  NEFIN complete: ", nrow(nefin), "plots\n")
if (length(overlap) > 0) {
  cat("  Overlapping:    ", length(overlap), "plots\n")
}
cat("\n")

cat("AUGMENTED DATASET:\n")
cat("  Total plots:", nrow(augmented), "\n")
cat("  FIA plots:  ", sum(augmented$dataset == "FIA"), 
    sprintf(" (%.1f%%)\n", 100 * sum(augmented$dataset == "FIA") / nrow(augmented)))
cat("  NEFIN plots:", sum(augmented$dataset == "NEFIN"), 
    sprintf(" (%.1f%%)\n", 100 * sum(augmented$dataset == "NEFIN") / nrow(augmented)))
cat("\n")

cat("COORDINATE SOURCES:\n")
fuzzed_count <- sum(augmented$coord_source == "fuzzed", na.rm = TRUE)
true_count <- sum(augmented$coord_source == "TRUE", na.rm = TRUE)

cat("  Fuzzed:", fuzzed_count, 
    sprintf(" (%.1f%%)\n", 100 * fuzzed_count / nrow(augmented)))
cat("  True:  ", true_count, 
    sprintf(" (%.1f%%)\n", 100 * true_count / nrow(augmented)))
cat("\n")

cat("BIOMASS STATISTICS:\n")
cat(sprintf("  Overall: Mean = %.2f Mg/ha, Median = %.2f Mg/ha\n",
            mean(augmented$biomass, na.rm = TRUE),
            median(augmented$biomass, na.rm = TRUE)))
cat(sprintf("  FIA:     Mean = %.2f Mg/ha, Median = %.2f Mg/ha\n",
            mean(augmented$biomass[augmented$dataset == "FIA"], na.rm = TRUE),
            median(augmented$biomass[augmented$dataset == "FIA"], na.rm = TRUE)))
cat(sprintf("  NEFIN:   Mean = %.2f Mg/ha, Median = %.2f Mg/ha\n",
            mean(augmented$biomass[augmented$dataset == "NEFIN"], na.rm = TRUE),
            median(augmented$biomass[augmented$dataset == "NEFIN"], na.rm = TRUE)))
cat("\n")

cat("OUTPUT FILE:\n")
cat("  ", output_path, "\n")

cat("\n═══════════════════════════════════════════════════════════════════\n")

sink()

cat("\n✓ Summary written:", summary_path, "\n")

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Comparison datasets created:\n")
cat("  baseline.csv:   ", nrow(fia), "plots (100% fuzzed)\n")
cat("  augmented.csv:  ", nrow(augmented), "plots (mixed precision)\n")
cat("    - FIA:  ", sum(augmented$dataset == "FIA"), "(fuzzed)\n")
cat("    - NEFIN:", sum(augmented$dataset == "NEFIN"), "(true)\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  READY FOR ANALYSIS!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Research Question:\n")
cat("  Does augmenting FIA with NEFIN's precise coordinates\n")
cat("  improve forest biomass estimates at different spatial scales?\n\n")

cat("Next steps:\n")
cat("  1. Assign plots to hexagonal grids (Phase D)\n")
cat("  2. Compare baseline vs augmented estimates\n")
cat("  3. Quantify improvement from coordinate precision\n\n")