# =============================================================================
# FIXED: Aggregate to Hexagons
# =============================================================================
# FIX 1: Use hex_scale column (long format) instead of hex_100ha columns
# FIX 2: Handle NEFIN temporal replicates (use latest year per plot)
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE E: AGGREGATE TO HEXAGONS (FIXED)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading datasets...\n")

baseline <- read_csv("data/processed/baseline_with_covariates.csv", 
                     show_col_types = FALSE)
baseline_hex <- read_csv("data/processed/baseline_hex_assignments.csv",
                         show_col_types = FALSE)

augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                      show_col_types = FALSE)
augmented_hex <- read_csv("data/processed/augmented_hex_assignments.csv",
                          show_col_types = FALSE)

cat("  Baseline plots:", nrow(baseline), "\n")
cat("  Augmented plots:", nrow(augmented), "\n\n")

# =============================================================================
# HANDLE TEMPORAL REPLICATES (NEFIN)
# =============================================================================

cat("Checking for temporal replicates...\n")

# Check if CN is duplicated
n_unique_baseline <- n_distinct(baseline$CN)
n_unique_augmented <- n_distinct(augmented$CN)

cat("  Baseline: ", nrow(baseline), "rows,", n_unique_baseline, "unique plots\n")
cat("  Augmented:", nrow(augmented), "rows,", n_unique_augmented, "unique plots\n")

if (n_unique_augmented < nrow(augmented)) {
  cat("\n  ⚠ NEFIN has temporal replicates (same plot, multiple years)\n")
  cat("  Using latest year per plot...\n\n")
  
  # For augmented, keep only latest year per plot
  augmented <- augmented %>%
    group_by(CN) %>%
    filter(MEASYEAR == max(MEASYEAR)) %>%
    ungroup()
  
  cat("  After deduplication:", nrow(augmented), "plots\n\n")
} else {
  cat("  No temporal replicates\n\n")
}

# =============================================================================
# AGGREGATE BASELINE
# =============================================================================

cat("Aggregating baseline to hex level...\n\n")

output_dir <- "data/processed/hex_aggregated"
ensure_dir(output_dir)

for (scale in CONFIG$hex_scales) {
  cat("  Processing", scale$name, "...\n")
  
  # FIX: Filter hex assignments by scale (not looking for column)
  hex_for_scale <- baseline_hex %>%
    filter(hex_scale == scale$name) %>%
    select(CN, hex_id)
  
  if (nrow(hex_for_scale) == 0) {
    cat("    ⚠ No hex assignments for this scale, skipping\n")
    next
  }
  
  # Join baseline data with hex assignments
  baseline_with_hex <- baseline %>%
    inner_join(hex_for_scale, by = "CN") %>%
    filter(!is.na(hex_id))
  
  # Aggregate to hex level
  hex_agg <- baseline_with_hex %>%
    group_by(hex_id) %>%
    summarise(
      # Biomass
      biomass_mean = mean(biomass, na.rm = TRUE),
      biomass_sd = sd(biomass, na.rm = TRUE),
      biomass_se = sd(biomass, na.rm = TRUE) / sqrt(n()),
      biomass_min = min(biomass, na.rm = TRUE),
      biomass_max = max(biomass, na.rm = TRUE),
      
      # NDVI
      ndvi_modis_mean = mean(ndvi_modis, na.rm = TRUE),
      ndvi_s2_mean = mean(ndvi_s2, na.rm = TRUE),
      
      # Climate
      tmean_mean = mean(tmean, na.rm = TRUE),
      ppt_mean = mean(ppt, na.rm = TRUE),
      
      # Metadata
      n_plots = n(),
      
      .groups = "drop"
    )
  
  # Save
  output_path <- file.path(output_dir, paste0("baseline_hex_", scale$name, ".csv"))
  write_csv(hex_agg, output_path)
  
  cat("    ✓ Saved:", nrow(hex_agg), "hexagons,", 
      sum(hex_agg$n_plots), "plots\n")
}

cat("\n")

# =============================================================================
# AGGREGATE AUGMENTED
# =============================================================================

cat("Aggregating augmented to hex level...\n\n")

for (scale in CONFIG$hex_scales) {
  cat("  Processing", scale$name, "...\n")
  
  # Filter hex assignments by scale
  hex_for_scale <- augmented_hex %>%
    filter(hex_scale == scale$name) %>%
    select(CN, hex_id) %>%
    distinct(CN, .keep_all = TRUE)  # Keep one row per CN
  
  if (nrow(hex_for_scale) == 0) {
    cat("    ⚠ No hex assignments for this scale, skipping\n")
    next
  }
  
  # Join augmented data with hex assignments
  augmented_with_hex <- augmented %>%
    inner_join(hex_for_scale, by = "CN") %>%
    filter(!is.na(hex_id))
  
  # Aggregate to hex level (with dataset breakdown)
  hex_agg <- augmented_with_hex %>%
    group_by(hex_id) %>%
    summarise(
      # Biomass
      biomass_mean = mean(biomass, na.rm = TRUE),
      biomass_sd = sd(biomass, na.rm = TRUE),
      biomass_se = sd(biomass, na.rm = TRUE) / sqrt(n()),
      biomass_min = min(biomass, na.rm = TRUE),
      biomass_max = max(biomass, na.rm = TRUE),
      
      # NDVI
      ndvi_modis_mean = mean(ndvi_modis, na.rm = TRUE),
      ndvi_s2_mean = mean(ndvi_s2, na.rm = TRUE),
      
      # Climate
      tmean_mean = mean(tmean, na.rm = TRUE),
      ppt_mean = mean(ppt, na.rm = TRUE),
      
      # Metadata
      n_plots = n(),
      n_fia = sum(dataset == "FIA"),
      n_nefin = sum(dataset == "NEFIN"),
      pct_nefin = 100 * sum(dataset == "NEFIN") / n(),
      
      # Coordinate sources
      n_fuzzed = sum(coord_source %in% c("fuzzed", "FALSE"), na.rm = TRUE),
      n_true = sum(coord_source %in% c("true", "TRUE"), na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # Save
  output_path <- file.path(output_dir, paste0("augmented_hex_", scale$name, ".csv"))
  write_csv(hex_agg, output_path)
  
  cat("    ✓ Saved:", nrow(hex_agg), "hexagons,",
      sum(hex_agg$n_plots), "plots\n")
  cat("      FIA:", sum(hex_agg$n_fia), "NEFIN:", sum(hex_agg$n_nefin), "\n")
}

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  HEX AGGREGATION COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Output directory:", output_dir, "\n")
cat("Files created:", length(CONFIG$hex_scales) * 2, "(baseline + augmented)\n\n")

cat("Next: Run comparison analysis\n")
cat("  Rscript R/06_analysis/02_compare_datasets.R\n\n")