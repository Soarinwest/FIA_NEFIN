# =============================================================================
# Standardize NEFIN to FIA Schema - COMPREHENSIVE FIX
# =============================================================================
# FIX 1: Remove exact duplicates (same plot + same year appearing twice)
# FIX 2: Aggregate temporal resampling (multiple years per plot)
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/validation_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 3: STANDARDIZE TO FIA SCHEMA (COMPREHENSIVE FIX)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load biomass data
input_path <- file.path(CONFIG$paths$interim_nefin, "biomass", "nefin_plot_biomass.csv")
nefin <- read_csv(input_path, show_col_types = FALSE)

cat("Loaded:", nrow(nefin), "rows\n")
cat("Unique plots:", n_distinct(nefin$plot_id), "\n\n")

# =============================================================================
# FIX 1: REMOVE EXACT DUPLICATES
# =============================================================================

cat("Step 1: Removing exact duplicates...\n")

n_before <- nrow(nefin)

# Remove rows that are EXACT duplicates (same plot, same year, same biomass)
nefin_dedup <- nefin %>%
  distinct()

n_after <- nrow(nefin_dedup)
n_removed <- n_before - n_after

cat("  Removed:", n_removed, "exact duplicate rows\n")
cat("  Remaining:", n_after, "unique observations\n\n")

# =============================================================================
# FIX 2: HANDLE TEMPORAL RESAMPLING
# =============================================================================

cat("Step 2: Handling temporal resampling...\n")

# Check how many plots have multiple years
multi_year_plots <- nefin_dedup %>%
  count(plot_id) %>%
  filter(n > 1)

cat("  Plots with multiple years:", nrow(multi_year_plots), "\n")
if (nrow(multi_year_plots) > 0) {
  cat("  Max observations per plot:", max(multi_year_plots$n), "\n")
  cat("  Mean observations per plot:", round(mean(multi_year_plots$n), 1), "\n\n")
}

# Strategy: Keep MOST RECENT year within study period for each plot
cat("  Strategy: Keep most recent year per plot (within study period)\n\n")

nefin_one_per_plot <- nefin_dedup %>%
  group_by(plot_id) %>%
  # Keep the most recent year for each plot
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("  ✓ Aggregated to", nrow(nefin_one_per_plot), "plots (one per plot)\n\n")

# Show what we prevented
if (nrow(multi_year_plots) > 0) {
  example_plot <- multi_year_plots$plot_id[1]
  example_years <- nefin_dedup %>% 
    filter(plot_id == example_plot) %>% 
    pull(year) %>% 
    sort()
  
  cat("  Example: Plot", example_plot, "\n")
  cat("    Had", length(example_years), "years:", paste(example_years, collapse=", "), "\n")
  cat("    Kept:", max(example_years), "(most recent)\n\n")
}

# =============================================================================
# STANDARDIZE TO FIA SCHEMA
# =============================================================================

cat("Step 3: Standardizing to FIA schema...\n")

# Convert state abbreviations to FIPS codes
state_fips <- c(
  "CT" = 9, "MA" = 25, "ME" = 23, "NH" = 33,
  "NJ" = 34, "NY" = 36, "VT" = 50
)

nefin_std <- nefin_one_per_plot %>%
  mutate(
    CN = plot_id,
    STATECD = state_fips[state],
    MEASYEAR = year,
    lat = latitude,
    lon = longitude,
    lat_for_extraction = latitude,
    lon_for_extraction = longitude,
    dataset = "NEFIN",
    coord_source = "true"
  ) %>%
  select(
    CN, STATECD, MEASYEAR,
    lat, lon,
    lat_for_extraction, lon_for_extraction,
    biomass,
    dataset, coord_source
  )

cat("  ✓ Standardized to FIA schema\n\n")

# =============================================================================
# VALIDATION
# =============================================================================

cat("Step 4: Validation...\n")

required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", 
                   "lat_for_extraction", "lon_for_extraction",
                   "biomass", "dataset", "coord_source")

validate_columns(nefin_std, required_cols, "NEFIN")

# Check for duplicates (CRITICAL!)
n_dup <- sum(duplicated(nefin_std$CN))
if (n_dup > 0) {
  cat("  ✗ ERROR:", n_dup, "duplicate CNs found!\n")
  cat("    This should NOT happen after deduplication\n")
  stop("Duplicate CNs detected - check deduplication logic")
} else {
  cat("  ✓ No duplicate CNs\n")
}

if (!all(nefin_std$coord_source == "true")) {
  stop("ERROR: Not all coordinates marked as 'true'!")
}
cat("  ✓ All coordinates marked as 'true'\n")

if (!all(nefin_std$dataset == "NEFIN")) {
  stop("ERROR: Not all rows marked as NEFIN!")
}
cat("  ✓ All rows marked as NEFIN\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\nFinal dataset summary:\n")
cat("  Starting rows:", n_before, "\n")
cat("  After removing exact duplicates:", n_after, "\n")
cat("  After temporal aggregation:", nrow(nefin_std), "\n")
cat("  Reduction:", round(100 * (1 - nrow(nefin_std) / n_before), 1), "%\n\n")

cat("  States:", paste(sort(unique(nefin_std$STATECD)), collapse=", "), "\n")
cat("  Year range:", min(nefin_std$MEASYEAR), "-", max(nefin_std$MEASYEAR), "\n")
cat("  Biomass range:", round(min(nefin_std$biomass)), "-", 
    round(max(nefin_std$biomass)), "Mg/ha\n\n")

# =============================================================================
# SAVE
# =============================================================================

output_path <- file.path(CONFIG$paths$processed, "nefin_complete.csv")

# Backup old version if it exists
if (file.exists(output_path)) {
  backup_path <- file.path(CONFIG$paths$processed, "nefin_complete_OLD.csv")
  file.copy(output_path, backup_path, overwrite = TRUE)
  cat("⚠ Backed up old version to nefin_complete_OLD.csv\n")
}

write_csv(nefin_std, output_path)

cat("✓ Saved:", output_path, "\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("COMPREHENSIVE FIXES APPLIED:\n")
cat("  ✓ Fix 1: Removed exact duplicates (same plot + year)\n")
cat("  ✓ Fix 2: Temporal aggregation (kept most recent year per plot)\n")
cat("  ✓ Result: One unique row per plot\n")
cat("  ✓ No duplicate CNs\n")
cat("  ✓ Ready for Phase C comparison\n\n")

cat("Next: Run Phase C!\n")
cat("  Rscript run_scripts/run_phase_C.R\n\n")
