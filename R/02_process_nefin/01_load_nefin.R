# =============================================================================
# Load and Clean NEFIN Data
# =============================================================================
# Loads NEFIN raw data and performs initial cleaning
#
# INPUTS:
#   - data/raw/nefin/TREE_PLOT_DATA.csv
#
# OUTPUTS:
#   - data/interim/nefin/cleaned/plots.csv
#   - data/interim/nefin/cleaned/trees.csv
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/validation_utils.R")

library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 1: LOAD NEFIN DATA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Output directory
output_dir <- file.path(CONFIG$paths$interim_nefin, "cleaned")
ensure_dir(output_dir)

# =============================================================================
# LOAD RAW NEFIN DATA
# =============================================================================

cat("Loading NEFIN raw data...\n\n")

# TODO: Adjust path based on your NEFIN file structure
nefin_path <- "data/raw/nefin/TREE_PLOT_DATA.csv"

if (!file.exists(nefin_path)) {
  stop("NEFIN data not found: ", nefin_path)
}

nefin_raw <- read_csv(nefin_path, show_col_types = FALSE)

cat("Loaded:", nrow(nefin_raw), "rows\n")
cat("Columns:", paste(names(nefin_raw), collapse = ", "), "\n")

# =============================================================================
# IDENTIFY PLOT VS TREE DATA
# =============================================================================

# TODO: Separate plot-level from tree-level data
# This depends on your NEFIN file structure

# Example if data is already at plot level:
# plots <- nefin_raw %>% distinct(plot_id, lat, lon, year, ...)

# Example if data needs tree → plot aggregation:
# trees <- nefin_raw
# plots <- trees %>% group_by(plot_id) %>% ...

cat("\nNEFIN data structure identified:\n")
cat("  Plot-level columns: [list them]\n")
cat("  Tree-level columns: [list them]\n")

# =============================================================================
# SAVE CLEANED DATA
# =============================================================================

# Save plots
# write_csv(plots, file.path(output_dir, "plots.csv"))

# Save trees (if separate)
# write_csv(trees, file.path(output_dir, "trees.csv"))

cat("\n✓ Phase B Step 1 complete\n")
cat("Next: R/02_process_nefin/02_compute_biomass.R\n\n")
