# =============================================================================
# Run Phase A: FIA Processing
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE A: FIA PROCESSING\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Step 1: Extract from SQLite
cat("STEP 1/3: Extract from SQLite databases...\n")
source("R/01_process_fia/01_extract_from_sqlite.R")

# Step 2: Compute biomass
cat("\nSTEP 2/3: Compute plot-level biomass...\n")
source("R/01_process_fia/02_compute_biomass.R")

# Step 3: Create final dataset
cat("\nSTEP 3/3: Create final FIA dataset...\n")
source("R/01_process_fia/03_create_fia_dataset.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE A COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Output: data/processed/fia_complete.csv\n")
cat("Next:   source('run_scripts/run_phase_B.R')\n\n")
