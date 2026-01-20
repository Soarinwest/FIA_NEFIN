# =============================================================================
# Compute NEFIN Plot-Level Biomass
# =============================================================================
# Aggregates tree-level data to plot level (if needed)
#
# INPUTS:
#   - data/interim/nefin/cleaned/*.csv
#
# OUTPUTS:
#   - data/interim/nefin/biomass/nefin_plot_biomass.csv
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

source("R/00_config/config.R")
source("R/utils/biomass_utils.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 2: COMPUTE NEFIN BIOMASS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# TODO: Implement NEFIN biomass calculation
# Similar to FIA but may have different column names/structure

cat("\n✓ Phase B Step 2 complete\n")
cat("Next: R/02_process_nefin/03_create_nefin_dataset.R\n\n")
