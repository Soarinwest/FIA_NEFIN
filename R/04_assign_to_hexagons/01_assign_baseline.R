# =============================================================================
# Assign Baseline Plots to Hexagons
# =============================================================================
# Spatial join of baseline dataset to hex grids at all scales
#
# INPUTS:
#   - data/processed/baseline.csv
#   - data/hex/hex_grid_*.geojson (all scales)
#
# OUTPUTS:
#   - data/processed/baseline_hex_assignments.csv
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

source("R/00_config/config.R")
source("R/utils/spatial_utils.R")

library(sf)
library(dplyr)
library(readr)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE D - STEP 1: ASSIGN BASELINE TO HEXAGONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Load baseline
baseline <- read_csv("data/processed/baseline.csv", show_col_types = FALSE)

cat("Baseline:", nrow(baseline), "plots\n\n")

# Convert to spatial
baseline_sf <- coords_to_sf(
  baseline,
  lat_col = "lat_for_extraction",
  lon_col = "lon_for_extraction"
)

# =============================================================================
# ASSIGN TO EACH HEX SCALE
# =============================================================================

assignments_list <- list()

for (scale in CONFIG$hex_scales) {
  cat("Processing", scale$name, "...\n")
  
  # Load hex grid
  hex_sf <- load_hex_grid(scale$path)
  
  # Spatial join
  assigned <- assign_to_hexagons(baseline_sf, hex_sf)
  
  # Add scale name
  assigned$hex_scale <- scale$name
  
  assignments_list[[scale$name]] <- assigned
}

# Combine all scales
all_assignments <- bind_rows(assignments_list)

# =============================================================================
# SAVE OUTPUT
# =============================================================================

output_path <- "data/processed/baseline_hex_assignments.csv"
write_csv(all_assignments, output_path)

cat("\n✓ Saved:", output_path, "\n")
cat("Next: R/04_assign_to_hexagons/02_assign_augmented.R\n\n")
