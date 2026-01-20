# =============================================================================
# Run Phase B: NEFIN Processing
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B: NEFIN PROCESSING\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

source("R/02_process_nefin/01_load_nefin.R")
source("R/02_process_nefin/02_compute_biomass.R")
source("R/02_process_nefin/03_create_nefin_dataset.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Output: data/processed/nefin_complete.csv\n")
cat("Next:   source('run_scripts/run_phase_C.R')\n\n")
