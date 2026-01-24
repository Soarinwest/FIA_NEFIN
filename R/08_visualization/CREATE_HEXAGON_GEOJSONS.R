# =============================================================================
# Create Comprehensive Hexagon GeoJSONs with Statistics and Recommendations
# =============================================================================
# Combines hex geometries with all stats, comparisons, and recommendations
# Outputs publication-ready GeoJSON files for each scale
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  CREATING COMPREHENSIVE HEXAGON GEOJSONS\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Create output directory
dir.create("data/processed/hex_geojson_with_stats", 
           showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Scales to process - ALL available hex grids
scales <- c("100ha", "500ha", "1kha", "2_4kha", "5kha", "10kha", "50kha", "64kha", "100kha")

# Convert to hectares for consistent comparison
scale_ha <- c(100, 500, 1000, 2428, 5000, 10000, 50000, 64000, 100000)
names(scale_ha) <- scales

# Recommendation thresholds
thresholds <- list(
  essential = 1000,      # <1kha: NEFIN essential
  recommended = 10000,   # 1-10kha: NEFIN recommended
  optional = 50000,      # 10-50kha: NEFIN optional
  not_needed = 100000    # >50kha: NEFIN not needed
)

# Bias warning threshold (NEFIN dominates)
bias_threshold <- 0.5  # Warn if >50% NEFIN

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Determine recommendation category
get_recommendation <- function(scale_ha) {
  if (scale_ha < thresholds$essential) {
    return("essential")
  } else if (scale_ha < thresholds$recommended) {
    return("recommended")
  } else if (scale_ha < thresholds$optional) {
    return("optional")
  } else {
    return("not_needed")
  }
}

# Determine heterogeneity level
get_heterogeneity <- function(ndvi_sd) {
  # Vectorized heterogeneity classification
  ifelse(
    is.na(ndvi_sd), "unknown",
    ifelse(ndvi_sd > 0.10, "high",
           ifelse(ndvi_sd > 0.05, "medium", "low"))
  )
}

# Calculate improvement metrics
calc_improvement <- function(baseline_se, augmented_se) {
  # Vectorized improvement calculation
  improvement_pct <- ifelse(
    is.na(baseline_se) | is.na(augmented_se) | baseline_se == 0,
    NA_real_,
    100 * (baseline_se - augmented_se) / baseline_se
  )
  return(round(improvement_pct, 2))
}

# =============================================================================
# PROCESS EACH SCALE
# =============================================================================

for (scale in scales) {
  
  cat("\n──────────────────────────────────────────────────────────────────\n")
  cat("Processing scale:", scale, "(", scale_ha[scale], "ha )\n")
  cat("──────────────────────────────────────────────────────────────────\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 1: Load hex geometry
  # ───────────────────────────────────────────────────────────────────────
  
  geom_file <- paste0("data/hex/hex_grid_", scale, ".geojson")
  
  if (!file.exists(geom_file)) {
    cat("  ⚠ Geometry file not found:", geom_file, "\n")
    cat("  Skipping", scale, "\n")
    next
  }
  
  cat("Step 1: Loading geometry...\n")
  hex_geom <- st_read(geom_file, quiet = TRUE)
  cat("  ✓ Loaded", nrow(hex_geom), "hexagons\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 2: Load baseline data (FIA-only)
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 2: Loading baseline (FIA-only) data...\n")
  
  baseline_files <- c(
    paste0("data/processed/hex_aggregated/baseline_hex_", scale, "_filtered.csv"),
    paste0("data/processed/hex_aggregated/baseline_hex_", scale, ".csv")
  )
  
  baseline_file <- baseline_files[file.exists(baseline_files)][1]
  
  if (is.na(baseline_file)) {
    cat("  ⚠ Baseline file not found\n")
    baseline_data <- NULL
  } else {
    baseline_data <- read_csv(baseline_file, show_col_types = FALSE) %>%
      filter(n_plots > 0) %>%
      rename_with(~paste0("fia_", .), -hex_id)
    cat("  ✓ Loaded baseline:", nrow(baseline_data), "hexagons\n\n")
  }
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 3: Load augmented data (FIA+NEFIN)
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 3: Loading augmented (FIA+NEFIN) data...\n")
  
  augmented_files <- c(
    paste0("data/processed/hex_aggregated/augmented_hex_", scale, "_filtered.csv"),
    paste0("data/processed/hex_aggregated/augmented_hex_", scale, ".csv")
  )
  
  augmented_file <- augmented_files[file.exists(augmented_files)][1]
  
  if (is.na(augmented_file)) {
    cat("  ⚠ Augmented file not found\n")
    augmented_data <- NULL
  } else {
    augmented_data <- read_csv(augmented_file, show_col_types = FALSE) %>%
      filter(n_plots > 0) %>%
      rename_with(~paste0("aug_", .), -hex_id)
    cat("  ✓ Loaded augmented:", nrow(augmented_data), "hexagons\n\n")
  }
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 4: Load comparison metrics
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 4: Loading comparison metrics...\n")
  
  comparison_files <- c(
    paste0("data/processed/comparisons/comparison_", scale, ".csv")
  )
  
  comparison_file <- comparison_files[file.exists(comparison_files)][1]
  
  if (is.na(comparison_file)) {
    cat("  ⚠ Comparison file not found\n")
    comparison_data <- NULL
  } else {
    comparison_data <- read_csv(comparison_file, show_col_types = FALSE)
    cat("  ✓ Loaded comparison metrics\n\n")
  }
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 5: Join all data
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 5: Joining all data...\n")
  
  hex_complete <- hex_geom
  
  if (!is.null(baseline_data)) {
    hex_complete <- hex_complete %>%
      left_join(baseline_data, by = "hex_id")
  }
  
  if (!is.null(augmented_data)) {
    hex_complete <- hex_complete %>%
      left_join(augmented_data, by = "hex_id")
  }
  
  cat("  ✓ Joined all datasets\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 6: Calculate derived metrics
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 6: Calculating derived metrics...\n")
  
  # Check if we have any data
  has_data <- !is.null(baseline_data) || !is.null(augmented_data)
  
  if (!has_data) {
    cat("  ⚠ No plot data available for this scale\n")
    cat("  Skipping", scale, "(hex grid exists but no aggregated data)\n\n")
    next
  }
  
  hex_complete <- hex_complete %>%
    mutate(
      # Basic counts
      n_plots_total = coalesce(aug_n_plots, fia_n_plots, 0),
      n_plots_fia = coalesce(fia_n_plots, 0),
      n_plots_nefin = n_plots_total - n_plots_fia,
      pct_nefin = round(100 * n_plots_nefin / n_plots_total, 2),
      
      # Improvement metrics
      improvement_pct = calc_improvement(fia_biomass_se, aug_biomass_se),
      se_reduced = !is.na(improvement_pct) & improvement_pct > 0,
      
      # Biomass change (compositional bias indicator)
      biomass_change = aug_biomass_mean - fia_biomass_mean,
      biomass_change_pct = 100 * biomass_change / fia_biomass_mean,
      
      # Recommendation
      recommendation = get_recommendation(scale_ha[scale]),
      use_nefin = recommendation %in% c("essential", "recommended"),
      
      # Bias warnings
      bias_warning = pct_nefin > (bias_threshold * 100),
      compositional_bias = abs(biomass_change_pct) > 10 & pct_nefin > 20,
      
      # Quality flags
      sufficient_plots = n_plots_total >= 3,
      reliable = sufficient_plots & !bias_warning,
      
      # Scale info
      scale_name = scale,
      scale_ha = scale_ha[scale]
    )
  
  # Add heterogeneity if NDVI columns exist
  if ("aug_ndvi_s2_sd" %in% names(hex_complete) | "fia_ndvi_s2_sd" %in% names(hex_complete)) {
    hex_complete <- hex_complete %>%
      mutate(
        heterogeneity = get_heterogeneity(
          coalesce(
            if("aug_ndvi_s2_sd" %in% names(.)) aug_ndvi_s2_sd else NA_real_,
            if("fia_ndvi_s2_sd" %in% names(.)) fia_ndvi_s2_sd else NA_real_
          )
        )
      )
  } else {
    hex_complete$heterogeneity <- "unknown"
  }
  
  # Add summary-level comparison metrics if available
  if (!is.null(comparison_data)) {
    if ("rmse_baseline" %in% names(comparison_data)) {
      hex_complete$rmse_baseline <- comparison_data$rmse_baseline[1]
      hex_complete$rmse_augmented <- comparison_data$rmse_augmented[1]
      hex_complete$overall_improvement <- comparison_data$pct_improved[1]
    }
  }
  
  cat("  ✓ Calculated", sum(!is.na(hex_complete$improvement_pct)), 
      "hexagons with improvement metrics\n")
  cat("  ✓", sum(hex_complete$use_nefin, na.rm = TRUE), 
      "hexagons where NEFIN is recommended\n")
  cat("  ✓", sum(hex_complete$bias_warning, na.rm = TRUE), 
      "hexagons with bias warning\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 7: Select final columns for output
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 7: Preparing final dataset...\n")
  
  # Define output columns in logical order
  output_cols <- c(
    "hex_id",
    "scale_name",
    "scale_ha",
    
    # Plot counts
    "n_plots_total",
    "n_plots_fia",
    "n_plots_nefin",
    "pct_nefin",
    
    # FIA-only metrics
    "fia_biomass_mean",
    "fia_biomass_sd",
    "fia_biomass_se",
    "fia_biomass_min",
    "fia_biomass_max",
    
    # Augmented metrics
    "aug_biomass_mean",
    "aug_biomass_sd",
    "aug_biomass_se",
    "aug_biomass_min",
    "aug_biomass_max",
    
    # Change metrics
    "biomass_change",
    "biomass_change_pct",
    "improvement_pct",
    "se_reduced",
    
    # Recommendations
    "recommendation",
    "use_nefin",
    "heterogeneity",
    
    # Warnings
    "bias_warning",
    "compositional_bias",
    "sufficient_plots",
    "reliable",
    
    # Covariates (if available)
    "aug_ndvi_s2_mean",
    "aug_ndvi_s2_sd",
    "aug_ndvi_modis_mean",
    "aug_tmean_mean",
    "aug_ppt_mean",
    
    # Summary metrics
    "rmse_baseline",
    "rmse_augmented",
    "overall_improvement"
  )
  
  # Keep only columns that exist
  output_cols_present <- output_cols[output_cols %in% names(hex_complete)]
  
  hex_final <- hex_complete %>%
    select(any_of(output_cols_present), geometry)
  
  cat("  ✓ Selected", length(output_cols_present), "attribute columns\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 8: Save GeoJSON
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 8: Saving GeoJSON...\n")
  
  output_file <- paste0("data/processed/hex_geojson_with_stats/hex_", 
                        scale, "_complete.geojson")
  
  st_write(hex_final, output_file, delete_dsn = TRUE, quiet = TRUE)
  
  file_size_mb <- round(file.size(output_file) / 1024^2, 2)
  
  cat("  ✓ Saved:", output_file, "\n")
  cat("  ✓ File size:", file_size_mb, "MB\n")
  cat("  ✓ Features:", nrow(hex_final), "\n")
  cat("  ✓ Attributes:", ncol(hex_final) - 1, "\n\n")  # -1 for geometry
  
  # ───────────────────────────────────────────────────────────────────────
  # Step 9: Create summary statistics
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 9: Summary statistics for", scale, "\n")
  
  summary_stats <- hex_final %>%
    st_drop_geometry() %>%
    summarise(
      scale = scale,
      total_hexagons = n(),
      hexagons_with_plots = sum(n_plots_total > 0, na.rm = TRUE),
      mean_plots_per_hex = mean(n_plots_total, na.rm = TRUE),
      mean_pct_nefin = mean(pct_nefin, na.rm = TRUE),
      hexagons_use_nefin = sum(use_nefin, na.rm = TRUE),
      hexagons_bias_warning = sum(bias_warning, na.rm = TRUE),
      hexagons_reliable = sum(reliable, na.rm = TRUE),
      mean_improvement_pct = mean(improvement_pct, na.rm = TRUE),
      hexagons_improved = sum(se_reduced, na.rm = TRUE)
    )
  
  print(summary_stats)
  cat("\n")
}

# =============================================================================
# CREATE METADATA FILE
# =============================================================================

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("Creating metadata documentation...\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

metadata <- list(
  title = "Comprehensive Hexagon GeoJSONs with Statistics and Recommendations",
  description = "Hexagonal grid data at multiple scales with FIA/NEFIN comparison metrics",
  date_created = Sys.Date(),
  projection = "EPSG:5070 (CONUS Albers Equal Area)",
  
  scales = list(
    `100ha` = "100 hectares (1 km²) - Very fine scale",
    `500ha` = "500 hectares (5 km²) - Fine scale",
    `1kha` = "1,000 hectares (10 km²) - Landscape scale",
    `2_4kha` = "2,428 hectares (24 km²) - Intermediate scale",
    `5kha` = "5,000 hectares (50 km²) - Intermediate scale",
    `10kha` = "10,000 hectares (100 km²) - Regional scale",
    `50kha` = "50,000 hectares (500 km²) - Large regional scale",
    `64kha` = "64,000 hectares (640 km²) - FIA big map hex ⭐",
    `100kha` = "100,000 hectares (1,000 km²) - Macro scale"
  ),
  
  recommendations = list(
    essential = "NEFIN essential: <1,000 ha - Coordinate precision critical",
    recommended = "NEFIN recommended: 1,000-10,000 ha - Moderate precision benefit",
    optional = "NEFIN optional: 10,000-50,000 ha - Evaluate cost-benefit",
    not_needed = "NEFIN not needed: >50,000 ha - FIA adequate, spatial averaging sufficient"
  ),
  
  key_attributes = list(
    plot_counts = c("n_plots_total", "n_plots_fia", "n_plots_nefin", "pct_nefin"),
    fia_metrics = c("fia_biomass_mean", "fia_biomass_sd", "fia_biomass_se"),
    augmented_metrics = c("aug_biomass_mean", "aug_biomass_sd", "aug_biomass_se"),
    changes = c("biomass_change", "biomass_change_pct", "improvement_pct"),
    recommendations = c("recommendation", "use_nefin", "heterogeneity"),
    warnings = c("bias_warning", "compositional_bias", "reliable")
  ),
  
  interpretation = list(
    improvement_pct = "Percent reduction in SE when adding NEFIN. Positive = improvement.",
    biomass_change_pct = "Percent change in mean biomass FIA→FIA+NEFIN. Large values suggest compositional bias.",
    bias_warning = "TRUE if NEFIN comprises >50% of plots. May indicate non-representative sampling.",
    compositional_bias = "TRUE if biomass changes >10% AND NEFIN >20%. Pooling may be problematic.",
    use_nefin = "Recommendation to use NEFIN data at this scale.",
    reliable = "Sufficient plots (≥3) AND no bias warning."
  ),
  
  usage_examples = list(
    QGIS = "Load GeoJSON → Style by 'recommendation' → Create thematic map",
    Python = "gdf = gpd.read_file('hex_1kha_complete.geojson')",
    R = "hex <- st_read('hex_1kha_complete.geojson')",
    JavaScript = "L.geoJSON(data, {style: feature => styleByRecommendation(feature)})"
  )
)

# Save as JSON
metadata_file <- "data/processed/hex_geojson_with_stats/METADATA.json"
write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)

cat("✓ Saved metadata:", metadata_file, "\n\n")

# Save as Markdown (more readable)
metadata_md <- "data/processed/hex_geojson_with_stats/README.md"

readme_content <- paste0(
  "# Comprehensive Hexagon GeoJSONs with Statistics and Recommendations\n\n",
  "Created: ", Sys.Date(), "\n\n",
  "## Overview\n\n",
  "This directory contains hexagonal grid data at multiple spatial scales with complete ",
  "FIA/NEFIN comparison statistics, recommendations for when to use NEFIN data, and bias warnings.\n\n",
  "## Files\n\n",
  "- `hex_100ha_complete.geojson` - 100 hectare hexagons (very fine scale)\n",
  "- `hex_500ha_complete.geojson` - 500 hectare hexagons (fine scale)\n",
  "- `hex_1kha_complete.geojson` - 1,000 hectare hexagons (landscape scale)\n",
  "- `hex_2_4kha_complete.geojson` - 2,428 hectare hexagons (intermediate scale)\n",
  "- `hex_5kha_complete.geojson` - 5,000 hectare hexagons (intermediate scale)\n",
  "- `hex_10kha_complete.geojson` - 10,000 hectare hexagons (regional scale)\n",
  "- `hex_50kha_complete.geojson` - 50,000 hectare hexagons (large regional)\n",
  "- `hex_64kha_complete.geojson` - 64,000 hectare hexagons (FIA big map) ⭐\n",
  "- `hex_100kha_complete.geojson` - 100,000 hectare hexagons (macro scale)\n\n",
  "**Note**: ⭐ marks FIA reference scale (big map hexagons).\n\n",
  "## Key Attributes\n\n",
  "### Plot Counts\n",
  "- `n_plots_total` - Total plots in hexagon (FIA + NEFIN)\n",
  "- `n_plots_fia` - FIA plots only\n",
  "- `n_plots_nefin` - NEFIN plots only\n",
  "- `pct_nefin` - Percentage of NEFIN plots\n\n",
  "### Biomass Metrics\n",
  "- `fia_biomass_*` - Statistics from FIA-only plots\n",
  "- `aug_biomass_*` - Statistics from FIA+NEFIN combined\n",
  "- `biomass_change` - Difference in mean (aug - fia)\n",
  "- `biomass_change_pct` - Percent change in mean\n\n",
  "### Improvement Metrics\n",
  "- `improvement_pct` - Percent reduction in SE when adding NEFIN\n",
  "- `se_reduced` - Boolean: TRUE if SE decreased\n\n",
  "### Recommendations\n",
  "- `recommendation` - Category: essential/recommended/optional/not_needed\n",
  "- `use_nefin` - Boolean: TRUE if NEFIN recommended at this scale\n",
  "- `heterogeneity` - NDVI heterogeneity: high/medium/low\n\n",
  "### Warnings\n",
  "- `bias_warning` - TRUE if NEFIN >50% (may bias sample)\n",
  "- `compositional_bias` - TRUE if large biomass shift + high NEFIN %\n",
  "- `reliable` - TRUE if ≥3 plots AND no bias warning\n\n",
  "## Recommendation Categories\n\n",
  "| Category | Scale Range | Description |\n",
  "|----------|-------------|-------------|\n",
  "| **Essential** | <1,000 ha | Coordinate precision critical for fine-scale work |\n",
  "| **Recommended** | 1,000-10,000 ha | Moderate precision benefit, worth considering |\n",
  "| **Optional** | 10,000-50,000 ha | Evaluate cost-benefit, FIA may suffice |\n",
  "| **Not Needed** | >50,000 ha | Spatial averaging mitigates fuzzing, FIA adequate |\n\n",
  "## Usage Examples\n\n",
  "### QGIS\n",
  "```\n",
  "1. Layer → Add Layer → Add Vector Layer\n",
  "2. Select hex_1kha_complete.geojson\n",
  "3. Right-click → Properties → Symbology\n",
  "4. Categorized → Column: recommendation\n",
  "5. Classify → Apply custom colors\n",
  "```\n\n",
  "### Python\n",
  "```python\n",
  "import geopandas as gpd\n",
  "hex_data = gpd.read_file('hex_1kha_complete.geojson')\n",
  "\n",
  "# Filter to hexagons where NEFIN is recommended\n",
  "use_nefin = hex_data[hex_data['use_nefin'] == True]\n",
  "\n",
  "# Map improvement percentage\n",
  "hex_data.plot(column='improvement_pct', cmap='RdYlGn', legend=True)\n",
  "```\n\n",
  "### R\n",
  "```r\n",
  "library(sf)\n",
  "library(ggplot2)\n",
  "\n",
  "hex <- st_read('hex_1kha_complete.geojson')\n",
  "\n",
  "# Map recommendations\n",
  "ggplot(hex) +\n",
  "  geom_sf(aes(fill = recommendation)) +\n",
  "  scale_fill_viridis_d()\n",
  "```\n\n",
  "## Interpretation Guide\n\n",
  "**High improvement_pct** (>20%): NEFIN provides substantial precision benefit\n",
  "\n**Low improvement_pct** (<5%): FIA adequate, NEFIN adds minimal value\n",
  "\n**High biomass_change_pct** (>15%): Compositional differences between datasets\n",
  "\n**bias_warning = TRUE**: NEFIN-dominated hexagon, may not represent typical FIA conditions\n",
  "\n**compositional_bias = TRUE**: Pooling FIA+NEFIN may introduce bias, use caution\n",
  "\n## Citation\n\n",
  "If using these data, please cite:\n",
  "[Your manuscript citation here]\n\n",
  "## Contact\n\n",
  "[Your contact information]\n"
)

writeLines(readme_content, metadata_md)
cat("✓ Saved README:", metadata_md, "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("  HEXAGON GEOJSON CREATION COMPLETE\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Created GeoJSON files in: data/processed/hex_geojson_with_stats/\n\n")

cat("Files created:\n")
for (scale in scales) {
  file <- paste0("hex_", scale, "_complete.geojson")
  if (file.exists(paste0("data/processed/hex_geojson_with_stats/", file))) {
    cat("  ✓", file, "\n")
  }
}

cat("\n  ✓ METADATA.json\n")
cat("  ✓ README.md\n\n")

cat("These GeoJSONs include:\n")
cat("  • Plot counts (FIA, NEFIN, total)\n")
cat("  • Biomass statistics (FIA-only vs augmented)\n")
cat("  • Improvement metrics\n")
cat("  • Recommendations (when to use NEFIN)\n")
cat("  • Bias warnings\n")
cat("  • Covariates (NDVI, climate)\n")
cat("  • Quality flags\n\n")

cat("Use these for:\n")
cat("  • Interactive web maps\n")
cat("  • QGIS visualization\n")
cat("  • Python/R spatial analysis\n")
cat("  • Supplementary materials\n")
cat("  • Sharing with collaborators\n\n")

cat("Complete! ✨\n\n")