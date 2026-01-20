# =============================================================================
# Assign Augmented to Hexagons - LONG FORMAT v2
# =============================================================================
# FIX: Handle CRS mismatches between data and hex grids
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  ASSIGN AUGMENTED TO HEXAGONS (LONG FORMAT)\n")
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

cat("Augmented CRS:", st_crs(augmented_sf)$input, "\n\n")

# =============================================================================
# ASSIGN TO HEX GRIDS - LONG FORMAT WITH CRS HANDLING
# =============================================================================

all_assignments <- list()

for (scale in CONFIG$hex_scales) {
  cat("Processing", scale$name, "...\n")
  
  # Load hex grid
  hex_path <- scale$path
  if (!file.exists(hex_path)) {
    cat("  ⚠ Grid not found, skipping\n\n")
    next
  }
  
  hex_grid <- st_read(hex_path, quiet = TRUE)
  cat("  Loaded", nrow(hex_grid), "hexagons\n")
  
  # Handle CRS - ensure match
  aug_crs <- st_crs(augmented_sf)
  hex_crs <- st_crs(hex_grid)
  
  if (is.na(hex_crs)) {
    cat("  ⚠ Hex grid missing CRS, setting to match augmented\n")
    st_crs(hex_grid) <- aug_crs
  } else if (!identical(hex_crs$wkt, aug_crs$wkt)) {
    cat("  Transforming hex grid to analysis CRS\n")
    hex_grid <- st_transform(hex_grid, crs = aug_crs)
  }
  
  # Verify CRS match
  if (!st_crs(hex_grid) == st_crs(augmented_sf)) {
    cat("  ⚠ CRS still don't match, forcing alignment\n")
    st_crs(hex_grid) <- st_crs(augmented_sf)
  }
  
  # Spatial join
  joined <- st_join(augmented_sf, hex_grid)
  
  # Count assignments
  n_assigned <- sum(!is.na(joined$hex_id))
  pct_assigned <- 100 * n_assigned / nrow(augmented_sf)
  
  cat(sprintf("  Assigned %d / %d plots (%.1f%%)\n", 
              n_assigned, nrow(augmented_sf), pct_assigned))
  
  # Create assignment record in LONG format
  assignments <- st_drop_geometry(joined) %>%
    select(CN, hex_id) %>%
    mutate(hex_scale = scale$name) %>%
    filter(!is.na(hex_id))
  
  all_assignments[[scale$name]] <- assignments
}

cat("\n")

# =============================================================================
# COMBINE ALL SCALES - LONG FORMAT
# =============================================================================

cat("Combining all scales into long format...\n")

# Stack all scales
augmented_hex_long <- bind_rows(all_assignments)

cat("  Total rows:", nrow(augmented_hex_long), "\n")
cat("  Format: CN + hex_id + hex_scale\n")
cat("  Unique plots:", n_distinct(augmented_hex_long$CN), "\n\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_path <- "data/processed/augmented_hex_assignments.csv"
write_csv(augmented_hex_long, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(augmented_hex_long), "\n")
cat("  Format: LONG (matches baseline)\n\n")

# Show example
cat("Example structure:\n")
print(head(augmented_hex_long, 6))

cat("\nNext: Aggregate to hex level for comparison\n")
cat("  Rscript R/06_analysis/01_aggregate_to_hexagons.R\n\n")