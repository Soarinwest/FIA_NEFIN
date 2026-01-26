# =============================================================================
# Run Phase E: Analysis & Results Generation
# =============================================================================
# Runs all analysis scripts with updated data
# Generates tables, figures, and statistics for manuscript
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE E: ANALYSIS & RESULTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# CORE ANALYSIS
# =============================================================================

cat("PART 1: CORE ANALYSIS\n")
cat("─────────────────────────────────────────────────────────────────\n\n")

# Step 1: Extract FIA tree data (if needed for analysis)

# Step 2: Aggregate to hexagons
cat("STEP 2/12: Aggregate data to hexagons...\n")
source("R/06_analysis/01_aggregate_to_hexagons.R")
cat("  ✓ Complete\n\n")

# Step 3: Filter empty hexagons
cat("STEP 3/12: Filter empty hexagons...\n")
source("R/06_analysis/01_filter_empty_hexagons.R")
cat("  ✓ Complete\n\n")

# Step 4: Compare datasets
cat("STEP 4/12: Compare baseline vs augmented datasets...\n")
source("R/06_analysis/02_compare_datasets.R")
cat("  ✓ Complete\n\n")

# =============================================================================
# MONTE CARLO UNCERTAINTY ANALYSIS
# =============================================================================

cat("\nPART 2: MONTE CARLO UNCERTAINTY ANALYSIS\n")
cat("─────────────────────────────────────────────────────────────────\n\n")

# Step 5: Generate coordinate jitter
cat("STEP 5/12: Generate Monte Carlo coordinate jitter...\n")
source("R/06_analysis/03_monte_carlo_generate_jitter.R")
cat("  ✓ Complete\n\n")

# Step 6: Extract covariates for jittered coordinates
cat("STEP 6/12: Extract covariates for jittered coordinates...\n")
source("R/06_analysis/04_monte_carlo_extract_covariates.R")
cat("  ✓ Complete\n\n")

# Step 7: Analyze Monte Carlo uncertainty
cat("STEP 7/12: Analyze Monte Carlo uncertainty...\n")
source("R/06_analysis/05_monte_carlo_analyze_uncertainty.R")
cat("  ✓ Complete\n\n")

# =============================================================================
# COMBINED ANALYSIS & STATISTICS
# =============================================================================

cat("\nPART 3: COMBINED ANALYSIS & STATISTICS\n")
cat("─────────────────────────────────────────────────────────────────\n\n")

# Step 8: Combined analysis
cat("STEP 8/12: Run combined analysis...\n")
source("R/06_analysis/06_combined_analysis.R")
cat("  ✓ Complete\n\n")

# Step 9: Summary statistics
cat("STEP 9/12: Generate summary statistics...\n")
source("R/06_analysis/07_summary_statistics.R")
cat("  ✓ Complete\n\n")

# =============================================================================
# SUPPLEMENTARY ANALYSES
# =============================================================================

cat("\nPART 4: SUPPLEMENTARY ANALYSES\n")
cat("─────────────────────────────────────────────────────────────────\n\n")

# Step 10: Within-hexagon variance
cat("STEP 10/12: Analyze within-hexagon variance...\n")
source("R/06_analysis/08_within_hex_variance.R")
cat("  ✓ Complete\n\n")

# Step 11: Large tree analysis
cat("STEP 11/12: Analyze large tree effects...\n")
source("R/06_analysis/09_large_tree_analysis.R")
cat("  ✓ Complete\n\n")

# Step 12: Hexagon scale impact
cat("STEP 12/12: Analyze hexagon scale impact...\n")
source("R/06_analysis/10_hexagon_scale_impact.R")
cat("  ✓ Complete\n\n")

# Optional: Max tree per plot analysis
if (file.exists("R/06_analysis/11_max_tree_per_plot.R")) {
  cat("OPTIONAL: Max tree per plot analysis...\n")
  source("R/06_analysis/11_max_tree_per_plot.R")
  cat("  ✓ Complete\n\n")
}

# =============================================================================
# VISUALIZATION
# =============================================================================

cat("\nPART 5: VISUALIZATION\n")
cat("─────────────────────────────────────────────────────────────────\n\n")
if (file.exists("R/08_visualization/CREATE_HEXAGON_GEOJSONS.R")) {
  cat("Generating all manuscript figures...\n")
  source("R/08_visualization/CREATE_HEXAGON_GEOJSONS.R")
  cat("  ✓ Complete\n\n")
}

if (file.exists("R/08_visualization/GENERATE_ALL_FIGURES.R")) {
  cat("Generating all manuscript figures...\n")
  source("R/08_visualization/GENERATE_ALL_FIGURES.R")
  cat("  ✓ Complete\n\n")
}

if (file.exists("R/08_visualization/CREATE_SPATIAL_CONTEXT_MAPS.R")) {
  cat("Creating spatial context maps...\n")
  source("R/08_visualization/CREATE_SPATIAL_CONTEXT_MAPS.R")
  cat("  ✓ Complete\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE E COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("OUTPUTS GENERATED:\n")
cat("  Core Analysis:\n")
cat("    • Hexagon aggregated data\n")
cat("    • Dataset comparison statistics\n")
cat("    • Monte Carlo uncertainty estimates\n")
cat("  \n")
cat("  Supplementary:\n")
cat("    • Within-hexagon variance analysis\n")
cat("    • Large tree effect analysis\n")
cat("    • Scale sensitivity analysis\n")
cat("  \n")
cat("  Figures:\n")
cat("    • All manuscript figures in manuscript_figures/\n")
cat("    • Spatial context maps\n")
cat("  \n")

cat("NEXT STEPS:\n")
cat("  1. Review results in data/processed/analysis/\n")
cat("  2. Check figures in manuscript_figures/\n")
cat("  3. Review summary statistics\n")
cat("  4. Run Phase 4 modeling (optional):\n")
cat("     source('run_scripts/run_phase4.R')\n\n")
