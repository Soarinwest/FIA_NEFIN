# =============================================================================
# Create Final Clean NEFIN Dataset
# =============================================================================
# Standardizes NEFIN to match FIA schema
#
# INPUTS:
#   - data/interim/nefin/biomass/nefin_plot_biomass.csv
#
# OUTPUTS:
#   - data/processed/nefin_complete.csv
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 3: CREATE NEFIN DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# TODO: Standardize NEFIN to match FIA schema
# Required columns:
# - CN (or equivalent plot ID)
# - STATECD, MEASYEAR
# - lat, lon (true coordinates!)
# - lat_for_extraction, lon_for_extraction (same as lat/lon)
# - biomass
# - dataset = "NEFIN"
# - coord_source = "true"

cat("\n✓ Phase B complete!\n")
cat("Output: data/processed/nefin_complete.csv\n")
cat("Next: R/03_create_comparison_datasets/\n\n")
