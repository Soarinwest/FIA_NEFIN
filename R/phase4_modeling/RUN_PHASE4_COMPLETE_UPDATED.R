# =============================================================================
# PHASE 4: MASTER RUNNER - UPDATED (Spatial Scale Comparison)
# =============================================================================
# Complete pipeline with:
# - 2 spatial scales (10m vs 250m)
# - 3 scenarios per scale (FIA, NEFIN, Pooled)
# - Spatial 10-fold cross-validation
# - Both regression and classification metrics
# - Comprehensive spatial validation plots
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: SPATIAL SCALE COMPARISON PIPELINE\n")
cat("  Testing Scale-Dependent Fuzzing Effects\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Track total time
start_time <- Sys.time()

# =============================================================================
# STEP 1: EXTRACT COVARIATES
# =============================================================================

cat("\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("  STEP 1/4: EXTRACT COVARIATES TO PLOTS\n")
cat("───────────────────────────────────────────────────────────────────\n")

step1_start <- Sys.time()
source("R/phase4_modeling/PHASE4_extract_covariates.R")
step1_time <- difftime(Sys.time(), step1_start, units = "mins")

cat("✓ Step 1 complete in", round(step1_time, 1), "minutes\n\n")

# =============================================================================
# STEP 2: PREPARE DATA
# =============================================================================

cat("\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("  STEP 2/4: PREPARE DATA FOR MODELING\n")
cat("───────────────────────────────────────────────────────────────────\n")

step2_start <- Sys.time()
source("R/phase4_modeling/PHASE4_01_prep_data.R")
step2_time <- difftime(Sys.time(), step2_start, units = "mins")

cat("✓ Step 2 complete in", round(step2_time, 1), "minutes\n\n")

# =============================================================================
# STEP 3: SPATIAL CROSS-VALIDATION (6 MODELS)
# =============================================================================

cat("\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("  STEP 3/4: SPATIAL 10-FOLD CROSS-VALIDATION\n")
cat("  Running 6 models (2 scales × 3 scenarios)\n")
cat("───────────────────────────────────────────────────────────────────\n")

step3_start <- Sys.time()
source("R/phase4_modeling/PHASE4_02b_spatial_cv.R")
step3_time <- difftime(Sys.time(), step3_start, units = "mins")

cat("✓ Step 3 complete in", round(step3_time, 1), "minutes\n\n")

# =============================================================================
# STEP 4: CREATE SPATIAL VALIDATION PLOTS
# =============================================================================

cat("\n")
cat("───────────────────────────────────────────────────────────────────\n")
cat("  STEP 4/4: CREATE SPATIAL VALIDATION PLOTS\n")
cat("  ROC, PR curves, spatial diagnostics\n")
cat("───────────────────────────────────────────────────────────────────\n")

step4_start <- Sys.time()
source("R/phase4_modeling/PHASE4_04_spatial_plots.R")
step4_time <- difftime(Sys.time(), step4_start, units = "mins")

cat("✓ Step 4 complete in", round(step4_time, 1), "minutes\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

total_time <- difftime(Sys.time(), start_time, units = "mins")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4 PIPELINE COMPLETE!\n")
cat("  Spatial Scale Comparison with AUC Validation\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Pipeline timing:\n")
cat(sprintf("  Step 1 (Extract):     %.1f min\n", step1_time))
cat(sprintf("  Step 2 (Prepare):     %.1f min\n", step2_time))
cat(sprintf("  Step 3 (Spatial CV):  %.1f min\n", step3_time))
cat(sprintf("  Step 4 (Plots):       %.1f min\n", step4_time))
cat(sprintf("  ────────────────────────────\n"))
cat(sprintf("  TOTAL TIME:           %.1f min\n\n", total_time))

cat("Models evaluated:\n")
cat("  • Fine Scale (10m):   FIA-only, NEFIN-only, Pooled\n")
cat("  • Coarse Scale (250m): FIA-only, NEFIN-only, Pooled\n")
cat("  • Total: 6 models × 10 folds = 60 model fits\n\n")

cat("Outputs created:\n")
cat("  • CV results: data/processed/phase4_cv_results/\n")
cat("    - cv_summary.csv (all model performance)\n")
cat("    - Fold-level results for each model\n")
cat("    - Plot-level predictions\n\n")
cat("  • Figures: manuscript_figures/phase4/spatial_cv/\n")
cat("    - ROC curves (AUC)\n")
cat("    - Precision-Recall curves\n")
cat("    - Spatial CV boxplots\n")
cat("    - Residual maps\n")
cat("    - Probability distributions\n")
cat("    - Scale comparison plots ⭐ KEY FIGURES!\n\n")

cat("Key findings to check:\n")
cat("  1. Does NEFIN outperform FIA at 10m? (Scale hypothesis)\n")
cat("  2. Are FIA and NEFIN similar at 250m?\n")
cat("  3. What's the AUC for high-biomass classification?\n")
cat("  4. Do residual maps show spatial patterns?\n\n")

cat("Next steps:\n")
cat("  1. Review: manuscript_figures/phase4/spatial_cv/\n")
cat("  2. Check: data/processed/phase4_cv_results/cv_summary.csv\n")
cat("  3. Interpret scale comparison plots (main findings!)\n")
cat("  4. Add covariates? Update config and re-run!\n")
cat("  5. Write Results section with quantitative findings\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("MANUSCRIPT-READY OUTPUTS:\n")
cat("  ✓ Quantified fuzzing effects at two scales\n")
cat("  ✓ AUC/ROC curves for classification\n")
cat("  ✓ Spatial validation with proper CV\n")
cat("  ✓ Publication-quality figures\n")
cat("  ✓ Statistical summaries with uncertainty\n\n")

cat("🎉 PHASE 4 COMPLETE - Ready for manuscript writing!\n\n")
