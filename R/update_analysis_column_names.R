# =============================================================================
# BATCH UPDATE: Fix Column Names in Analysis Scripts
# =============================================================================
# Updates all analysis scripts to use scale-specific covariate column names
# =============================================================================

library(stringr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  UPDATING ANALYSIS SCRIPTS FOR NEW COLUMN NAMES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Define replacements (old name -> new name)
replacements <- list(
  # MODIS covariates (250m)
  c("ndvi_modis(?!_)", "ndvi_modis_250m"),
  c("evi_modis(?!_)", "evi_modis_250m"),
  c("nbr_modis(?!_)", "nbr_modis_250m"),
  c("ndwi_modis(?!_)", "ndwi_modis_250m"),
  c("red_modis(?!_)", "red_modis_250m"),
  c("nir_modis(?!_)", "nir_modis_250m"),
  c("blue_modis(?!_)", "blue_modis_250m"),
  c("green_modis(?!_)", "green_modis_250m"),
  c("swir1_modis(?!_)", "swir1_modis_250m"),
  
  # Sentinel-2 covariates (10m)
  c("ndvi_s2(?!_)", "ndvi_s2_10m"),
  c("nbr_s2(?!_)", "nbr_s2_10m"),
  c("ndwi_s2(?!_)", "ndwi_s2_10m"),
  c("red_s2(?!_)", "red_s2_10m"),
  c("green_s2(?!_)", "green_s2_10m"),
  c("blue_s2(?!_)", "blue_s2_10m"),
  
  # Topography (use 250m for consistency)
  c("\\belevation(?!_)", "elevation_250m"),
  c("\\bslope(?!_)", "slope_250m"),
  c("\\baspect(?!_)", "aspect_250m"),
  
  # Climate (use 250m for consistency)
  c("\\btmean(?!_)", "tmean_250m"),
  c("\\bppt(?!_)", "ppt_250m")
)

# Scripts to update
scripts <- c(
  "R/06_analysis/01_aggregate_to_hexagons.R",
  "R/06_analysis/01_filter_empty_hexagons.R",
  "R/06_analysis/02_compare_datasets.R",
  "R/06_analysis/03_monte_carlo_generate_jitter.R",
  "R/06_analysis/04_monte_carlo_extract_covariates.R",
  "R/06_analysis/05_monte_carlo_analyze_uncertainty.R",
  "R/06_analysis/06_combined_analysis.R",
  "R/06_analysis/07_summary_statistics.R",
  "R/06_analysis/08_within_hex_variance.R",
  "R/06_analysis/09_large_tree_analysis.R",
  "R/06_analysis/10_hexagon_scale_impact.R",
  "R/06_analysis/11_max_tree_per_plot.R"
)

# Process each script
for (script_path in scripts) {
  if (!file.exists(script_path)) {
    cat("  ⊘ Skipping (not found):", basename(script_path), "\n")
    next
  }
  
  cat("  Processing:", basename(script_path), "...")
  
  # Read script
  content <- readLines(script_path)
  original_content <- content
  
  # Apply each replacement
  for (replacement in replacements) {
    old_pattern <- replacement[1]
    new_text <- replacement[2]
    content <- str_replace_all(content, old_pattern, new_text)
  }
  
  # Check if anything changed
  if (identical(content, original_content)) {
    cat(" no changes needed\n")
  } else {
    # Create backup
    backup_path <- paste0(script_path, ".backup")
    file.copy(script_path, backup_path, overwrite = TRUE)
    
    # Write updated content
    writeLines(content, script_path)
    cat(" ✓ UPDATED (backup saved)\n")
  }
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  UPDATE COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("WHAT CHANGED:\n")
cat("  • MODIS covariates now use _250m suffix\n")
cat("  • Sentinel-2 covariates now use _10m suffix\n")
cat("  • Climate/topography variables now use _250m suffix\n\n")

cat("BACKUPS:\n")
cat("  • Original scripts saved as *.R.backup\n\n")

cat("NEXT STEP:\n")
cat("  Run the analysis pipeline:\n")
cat("    source('run_scripts/run_analysis.R')\n\n")