# =============================================================================
# Run Complete Pipeline (All Phases Including Analysis)
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  RUNNING COMPLETE PIPELINE WITH ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Phase A: Process FIA data
cat("▶ PHASE A: FIA Processing\n")
source("run_scripts/run_phase_A.R")

# Phase B: Create comparison datasets  
cat("\n▶ PHASE B: Comparison Datasets\n")
source("run_scripts/run_phase_B.R")

# Phase C: Hexagon assignment
cat("\n▶ PHASE C: Hexagon Assignment\n")
source("run_scripts/run_phase_C.R")

# Phase D: Covariate extraction
cat("\n▶ PHASE D: Covariate Extraction\n")
source("run_scripts/run_phase_D.R")

# Phase E: Analysis & Results (NEW!)
cat("\n▶ PHASE E: Analysis & Results\n")
source("run_scripts/run_analysis.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  ALL PHASES COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("PIPELINE SUMMARY:\n")
cat("  ✓ Phase A: FIA data processed\n")
cat("  ✓ Phase B: Comparison datasets created\n")
cat("  ✓ Phase C: Hexagon assignments complete\n")
cat("  ✓ Phase D: Covariates extracted\n")
cat("  ✓ Phase E: Analysis complete, figures generated\n\n")

cat("OUTPUT LOCATIONS:\n")
cat("  • Data:    data/processed/\n")
cat("  • Figures: manuscript_figures/\n")
cat("  • Results: data/processed/analysis/\n\n")

cat("OPTIONAL NEXT STEP:\n")
cat("  Run Phase 4 modeling for spatial predictions:\n")
cat("    source('R/phase4_modeling/RUN_PHASE4_COMPLETE_UPDATED.R')\n\n")
