# =============================================================================
# Standardize NEFIN to FIA Schema - FINAL
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/validation_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 3: STANDARDIZE TO FIA SCHEMA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load biomass data
input_path <- file.path(CONFIG$paths$interim_nefin, "biomass", "nefin_plot_biomass.csv")
nefin <- read_csv(input_path, show_col_types = FALSE)

cat("Loaded:", nrow(nefin), "rows\n\n")

# Convert state abbreviations to FIPS codes
cat("Converting state codes...\n")
state_fips <- c(
  "CT" = 9, "MA" = 25, "ME" = 23, "NH" = 33,
  "NJ" = 34, "NY" = 36, "VT" = 50
)

nefin_std <- nefin %>%
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

cat("Standardized to FIA schema\n\n")

# Validation
cat("Validation:\n")
required_cols <- c("CN", "STATECD", "MEASYEAR", "lat", "lon", 
                   "lat_for_extraction", "lon_for_extraction",
                   "biomass", "dataset", "coord_source")

validate_columns(nefin_std, required_cols, "NEFIN")

if (!all(nefin_std$coord_source == "true")) {
  stop("ERROR: Not all coordinates marked as 'true'!")
}
cat("  ✓ All coordinates marked as 'true'\n")

if (!all(nefin_std$dataset == "NEFIN")) {
  stop("ERROR: Not all rows marked as NEFIN!")
}
cat("  ✓ All rows marked as NEFIN\n")

# Summary
cat("\nFinal dataset summary:\n")
cat("  Rows:", nrow(nefin_std), "\n")
cat("  States:", paste(sort(unique(nefin_std$STATECD)), collapse=", "), "\n")
cat("  Year range:", min(nefin_std$MEASYEAR), "-", max(nefin_std$MEASYEAR), "\n\n")

# Save
output_path <- file.path(CONFIG$paths$processed, "nefin_complete.csv")
write_csv(nefin_std, output_path)

cat("✓ Saved:", output_path, "\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Next: Run Phase C!\n")
cat("  Rscript run_scripts/run_phase_C.R\n\n")