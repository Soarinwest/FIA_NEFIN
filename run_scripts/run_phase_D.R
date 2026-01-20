# =============================================================================
# Run Phase D: Hex Assignment & Covariate Extraction
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE D: HEX ASSIGNMENT & COVARIATES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

source("R/04_assign_to_hexagons/01_assign_baseline.R")
source("R/04_assign_to_hexagons/02_assign_augmented.R")
source("R/05_extract_covariates/01_extract_baseline_covariates.R")
source("R/05_extract_covariates/02_extract_augmented_covariates.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE D COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
cat("Ready for analysis!\n\n")
