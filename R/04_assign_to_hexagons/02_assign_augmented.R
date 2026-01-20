# =============================================================================
# Assign Augmented Plots to Hexagons
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  ASSIGN AUGMENTED TO HEXAGONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD AUGMENTED DATA
# =============================================================================

cat("Loading augmented dataset...\n")
augmented <- read_csv("data/processed/augmented.csv", show_col_types = FALSE)
cat("  Plots:", nrow(augmented), "\n\n")

# Convert to spatial
augmented_sf <- st_as_sf(augmented,
                         coords = c("lon", "lat"),
                         crs = 4326)

# Transform to analysis CRS
augmented_sf <- st_transform(augmented_sf, crs = CONFIG$crs_analysis)

# =============================================================================
# ASSIGN TO HEX GRIDS
# =============================================================================

results <- list()

for (scale in CONFIG$hex_scales) {
  cat("Processing", scale$name, "...\n")
  
  # Load hex grid
  hex_path <- scale$path
  if (!file.exists(hex_path)) {
    cat("  ⚠ Grid not found, skipping\n\n")
    next
  }
  
  hex_grid <- st_read(hex_path, quiet = TRUE)
  cat("  Loaded", nrow(hex_grid), "hexagons from", basename(hex_path), "\n")
  
  # Ensure CRS match between points and hex grid
  aug_crs <- st_crs(augmented_sf)
  hex_crs <- st_crs(hex_grid)
  cat("  CRS augmented:", ifelse(is.na(aug_crs$epsg), aug_crs$wkt, paste0("EPSG:", aug_crs$epsg)), "\n")
  if (is.na(hex_crs)) {
    cat("  ⚠ Hex grid CRS is missing — assuming augmented CRS and setting it.\n")
    st_crs(hex_grid) <- aug_crs
  } else if (!identical(hex_crs$wkt, aug_crs$wkt)) {
    cat("  Transforming hex grid to analysis CRS (EPSG:", CONFIG$crs_analysis, ")\n")
    hex_grid <- st_transform(hex_grid, crs = aug_crs)
  }

  # Spatial join
  joined <- st_join(augmented_sf, hex_grid)
  
  # Count assignments
  n_assigned <- sum(!is.na(joined$hex_id))
  pct_assigned <- 100 * n_assigned / nrow(augmented_sf)
  
  cat(sprintf("  Assigned %d / %d plots (%.1f%%)\n", 
              n_assigned, nrow(augmented_sf), pct_assigned))
  
  # Store hex_id for this scale
  col_name <- paste0("hex_", scale$name)
  augmented[[col_name]] <- joined$hex_id
}

cat("\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_path <- "data/processed/augmented_hex_assignments.csv"
write_csv(augmented, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(augmented), "\n")
cat("  Hex scales:", length(CONFIG$hex_scales), "\n\n")

cat("Next: Aggregate to hex level for comparison\n\n")
