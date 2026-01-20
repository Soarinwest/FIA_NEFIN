# =============================================================================
# Run Phase C: Create Comparison Datasets
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C: CREATE COMPARISON DATASETS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

source("R/03_create_comparison_datasets/01_validate_inputs.R")
source("R/03_create_comparison_datasets/02_create_baseline.R")
source("R/03_create_comparison_datasets/03_create_augmented.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE C COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Outputs:\n")
cat("  - data/processed/baseline.csv   (FIA-only)\n")
cat("  - data/processed/augmented.csv  (FIA + NEFIN)\n\n")
cat("Next: source('run_scripts/run_phase_D.R')\n\n")
