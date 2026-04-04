# =============================================================================
# Enhanced Hexagon GeoJSONs with Decision Framework
# =============================================================================
# Extends CREATE_HEXAGON_GEOJSONS.R to add spatial decision attributes
# Each hex includes recommendation based on scale and composition
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  ENHANCED HEXAGON GEOJSONS WITH DECISION FRAMEWORK\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Create output directory
dir.create("data/processed/hex_geojson_with_decisions", 
           showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# DECISION FRAMEWORK LOGIC
# =============================================================================

# Decision function (matches 11b_spatial_decision_framework.R)
get_hex_recommendation <- function(scale_ha, pct_nefin, n_plots, biomass_sd) {
  
  # Scale-based primary recommendation
  scale_rec <- case_when(
    scale_ha < 1000 ~ "nefin_essential",
    scale_ha < 10000 ~ "fia_nefin_mixed",
    scale_ha < 50000 ~ "fia_nefin_optional",
    TRUE ~ "fia_adequate"
  )
  
  # Composition-based refinement
  composition <- case_when(
    pct_nefin >= 50 ~ "nefin_dominated",
    pct_nefin >= 10 ~ "mixed",
    pct_nefin > 0 ~ "fia_dominated",
    TRUE ~ "fia_only"
  )
  
  # Final integrated recommendation
  final_rec <- case_when(
    # Small scale: NEFIN preferred if available
    scale_ha < 1000 & pct_nefin > 0 ~ "Use NEFIN",
    scale_ha < 1000 & pct_nefin == 0 ~ "NEFIN Needed",
    
    # Intermediate scale: evaluate composition
    scale_ha < 10000 & pct_nefin >= 10 ~ "Use FIA + NEFIN",
    scale_ha < 10000 ~ "FIA Adequate",
    
    # Large scale: FIA generally sufficient
    scale_ha >= 50000 & pct_nefin > 50 ~ "Use FIA + NEFIN",
    scale_ha >= 50000 ~ "FIA Adequate",
    
    # Default: mixed
    TRUE ~ "Use FIA + NEFIN"
  )
  
  # Simplified 3-category version
  simple_rec <- case_when(
    final_rec %in% c("Use NEFIN", "NEFIN Needed") ~ "NEFIN-only",
    final_rec == "Use FIA + NEFIN" ~ "FIA + NEFIN",
    TRUE ~ "FIA-only"
  )
  
  # Quality assessment
  reliable <- n_plots >= 3 & pct_nefin < 80
  high_variance <- !is.na(biomass_sd) & biomass_sd > 50
  
  return(list(
    recommendation = final_rec,
    recommendation_simple = simple_rec,
    scale_category = scale_rec,
    composition = composition,
    reliable = reliable,
    high_variance = high_variance
  ))
}

# =============================================================================
# CONFIGURATION
# =============================================================================

scales <- c("100ha", "500ha", "1kha", "2_4kha", "5kha", "10kha", "50kha", "64kha", "100kha")
scale_ha <- c(100, 500, 1000, 2428, 5000, 10000, 50000, 64000, 100000)
names(scale_ha) <- scales

# =============================================================================
# PROCESS EACH SCALE
# =============================================================================

for (scale in scales) {
  
  cat("\n──────────────────────────────────────────────────────────────────\n")
  cat("Processing scale:", scale, "(", scale_ha[scale], "ha )\n")
  cat("──────────────────────────────────────────────────────────────────\n\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Load geometry
  # ───────────────────────────────────────────────────────────────────────
  
  geom_file <- paste0("data/hex/hex_grid_", scale, ".geojson")
  if (!file.exists(geom_file)) {
    cat("  ⚠ Geometry not found, skipping\n")
    next
  }
  
  hex_geom <- st_read(geom_file, quiet = TRUE)
  cat("Step 1: Loaded", nrow(hex_geom), "hexagons\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Load data
  # ───────────────────────────────────────────────────────────────────────
  
  # Try multiple file locations
  data_files <- c(
    paste0("data/processed/hex_aggregated/augmented_hex_", scale, "_filtered.csv"),
    paste0("data/processed/hex_aggregated/augmented_hex_", scale, ".csv"),
    paste0("data/processed/hex_aggregated/baseline_hex_", scale, ".csv")
  )
  
  data_file <- data_files[file.exists(data_files)][1]
  
  if (is.na(data_file)) {
    cat("  ⚠ Data file not found, skipping\n")
    next
  }
  
  hex_data <- read_csv(data_file, show_col_types = FALSE) %>%
    filter(n_plots > 0)
  
  cat("Step 2: Loaded", nrow(hex_data), "hex records\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Apply decision framework
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 3: Applying decision framework...\n")
  
  # Initialize columns for vectorized operation
  hex_data <- hex_data %>%
    mutate(
      pct_nefin = ifelse(is.na(pct_nefin), 0, pct_nefin),
      scale_ha_value = scale_ha[scale]
    )
  
  # Apply decision logic row by row (necessary for list output)
  decisions <- vector("list", nrow(hex_data))
  for (i in seq_len(nrow(hex_data))) {
    decisions[[i]] <- get_hex_recommendation(
      scale_ha = hex_data$scale_ha_value[i],
      pct_nefin = hex_data$pct_nefin[i],
      n_plots = hex_data$n_plots[i],
      biomass_sd = hex_data$biomass_sd[i]
    )
  }
  
  # Extract decision components
  hex_data <- hex_data %>%
    mutate(
      recommendation = sapply(decisions, function(x) x$recommendation),
      recommendation_simple = sapply(decisions, function(x) x$recommendation_simple),
      scale_category = sapply(decisions, function(x) x$scale_category),
      composition = sapply(decisions, function(x) x$composition),
      reliable = sapply(decisions, function(x) x$reliable),
      high_variance = sapply(decisions, function(x) x$high_variance)
    )
  
  # Calculate additional decision metrics
  hex_data <- hex_data %>%
    mutate(
      # Confidence score (0-100)
      confidence_score = case_when(
        n_plots < 3 ~ 25,
        pct_nefin > 80 ~ 50,
        pct_nefin > 50 ~ 75,
        TRUE ~ 100
      ),
      
      # Uncertainty flag
      high_uncertainty = (biomass_se > 20) | (n_plots < 5),
      
      # Precision benefit estimate (qualitative)
      precision_benefit = case_when(
        scale_ha_value < 1000 ~ "Critical",
        scale_ha_value < 10000 ~ "High",
        scale_ha_value < 50000 ~ "Moderate",
        TRUE ~ "Low"
      ),
      
      # Recommendation justification
      rec_rationale = case_when(
        recommendation == "Use NEFIN" ~ "Fine scale + NEFIN available: precision critical",
        recommendation == "NEFIN Needed" ~ "Fine scale but no NEFIN: seek precise coordinates",
        recommendation == "Use FIA + NEFIN" ~ "Intermediate scale: both datasets valuable",
        recommendation == "FIA Adequate" ~ "Large scale: averaging mitigates fuzzing",
        TRUE ~ "Standard application"
      )
    )
  
  cat("  ✓ Applied framework to", nrow(hex_data), "hexagons\n")
  
  # ───────────────────────────────────────────────────────────────────────
  # Join geometry and data
  # ───────────────────────────────────────────────────────────────────────
  
  cat("Step 4: Creating enhanced GeoJSON...\n")
  
  hex_complete <- hex_geom %>%
    left_join(hex_data, by = "hex_id") %>%
    filter(!is.na(biomass_mean))
  
  # Select final columns
  hex_final <- hex_complete %>%
    select(
      # Identifiers
      hex_id,
      
      # Plot counts
      n_plots, pct_nefin,
      
      # Biomass metrics
      biomass_mean, biomass_sd, biomass_se,
      
      # NDVI metrics
      ndvi_s2_mean,
      
      # Decision framework attributes
      recommendation,
      recommendation_simple,
      scale_category,
      composition,
      confidence_score,
      precision_benefit,
      rec_rationale,
      
      # Quality flags
      reliable,
      high_variance,
      high_uncertainty,
      
      # Geometry (must be last)
      geometry
    )
  
  # ───────────────────────────────────────────────────────────────────────
  # Save GeoJSON
  # ───────────────────────────────────────────────────────────────────────
  
  output_file <- paste0("data/processed/hex_geojson_with_decisions/hex_", 
                        scale, "_decision.geojson")
  
  st_write(hex_final, output_file, delete_dsn = TRUE, quiet = TRUE)
  
  cat("  ✓ Saved:", output_file, "\n")
  cat("    Hexagons:", nrow(hex_final), "\n")
  
  # Summary statistics for this scale
  rec_summary <- hex_final %>%
    st_drop_geometry() %>%
    count(recommendation_simple) %>%
    mutate(pct = round(100 * n / sum(n), 1))
  
  cat("    Recommendations:\n")
  for (i in seq_len(nrow(rec_summary))) {
    cat("      •", rec_summary$recommendation_simple[i], ":", 
        rec_summary$pct[i], "%\n")
  }
}

# =============================================================================
# CREATE METADATA
# =============================================================================

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("Creating metadata documentation...\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

metadata <- list(
  title = "Hexagon GeoJSONs with Integrated Decision Framework",
  description = paste(
    "Enhanced hexagonal grid data with spatial decision recommendations",
    "for when to use FIA vs NEFIN vs both datasets"
  ),
  date_created = Sys.Date(),
  projection = "EPSG:5070 (CONUS Albers Equal Area)",
  
  decision_categories = list(
    `NEFIN-only` = "Use NEFIN data exclusively (fine scale + high precision need)",
    `FIA + NEFIN` = "Combine both datasets (intermediate scale, both valuable)",
    `FIA-only` = "FIA data adequate (large scale, averaging effect sufficient)"
  ),
  
  key_attributes = list(
    core = c("hex_id", "n_plots", "pct_nefin", "biomass_mean", "biomass_sd"),
    decision = c("recommendation", "recommendation_simple", "scale_category", 
                 "composition", "confidence_score"),
    quality = c("reliable", "high_variance", "high_uncertainty"),
    guidance = c("precision_benefit", "rec_rationale")
  ),
  
  interpretation = list(
    recommendation = "Detailed text recommendation for this hexagon",
    recommendation_simple = "Simplified 3-category decision (NEFIN-only / FIA+NEFIN / FIA-only)",
    scale_category = "Scale-based primary category",
    composition = "Data composition (nefin_dominated / mixed / fia_dominated / fia_only)",
    confidence_score = "Decision confidence (0-100), lower if few plots or biased sample",
    precision_benefit = "Qualitative benefit of precise coordinates (Critical / High / Moderate / Low)",
    reliable = "TRUE if >=3 plots AND NEFIN <80% (trustworthy estimates)",
    high_variance = "TRUE if biomass SD >50 Mg/ha (heterogeneous conditions)",
    high_uncertainty = "TRUE if biomass SE >20 OR n_plots <5"
  ),
  
  usage_guidelines = list(
    fine_scale = "< 1 kha: Use NEFIN wherever available; coordinate precision critical",
    intermediate = "1-10 kha: Evaluate hex composition; combine datasets when mixed",
    regional = "10-50 kha: FIA often adequate; NEFIN adds value in mixed hexes",
    macro = "> 50 kha: FIA adequate; spatial averaging mitigates fuzzing"
  ),
  
  visualization_colors = list(
    `NEFIN-only` = "#2ca02c (green)",
    `FIA + NEFIN` = "#ff7f0e (orange)",
    `FIA-only` = "#1f77b4 (blue)"
  )
)

# Save metadata
metadata_file <- "data/processed/hex_geojson_with_decisions/METADATA.json"
write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)

cat("✓ Saved metadata:", metadata_file, "\n\n")

# Create README
readme_content <- paste0(
  "# Enhanced Hexagon GeoJSONs with Decision Framework\n\n",
  "Created: ", Sys.Date(), "\n\n",
  "## Overview\n\n",
  "These GeoJSON files contain hexagonal grid data with integrated spatial decision ",
  "recommendations for when to use FIA data, NEFIN data, or both.\n\n",
  "## Files\n\n",
  "- `hex_*_decision.geojson` - One file per spatial scale\n",
  "- Each file includes geometry, biomass metrics, and recommendation attributes\n\n",
  "## Decision Categories\n\n",
  "Each hexagon is classified into one of three categories:\n\n",
  "1. **NEFIN-only** (Green) - Use NEFIN data exclusively\n",
  "   - Fine spatial scale (<1 kha)\n",
  "   - Coordinate precision critical\n",
  "   - NEFIN data available\n\n",
  "2. **FIA + NEFIN** (Orange) - Combine both datasets\n",
  "   - Intermediate scale (1-50 kha)\n",
  "   - Both datasets provide value\n",
  "   - Mixed data composition\n\n",
  "3. **FIA-only** (Blue) - FIA data adequate\n",
  "   - Large spatial scale (>50 kha)\n",
  "   - Spatial averaging effect\n",
  "   - Coordinate fuzzing impact minimized\n\n",
  "## Key Attributes\n\n",
  "### Core Metrics\n",
  "- `hex_id` - Unique hexagon identifier\n",
  "- `n_plots` - Number of forest inventory plots\n",
  "- `pct_nefin` - Percentage of NEFIN plots\n",
  "- `biomass_mean` - Mean aboveground biomass (Mg/ha)\n",
  "- `biomass_sd` - Biomass standard deviation\n\n",
  "### Decision Framework\n",
  "- `recommendation` - Detailed text recommendation\n",
  "- `recommendation_simple` - 3-category classification\n",
  "- `scale_category` - Scale-based category\n",
  "- `composition` - Data composition type\n",
  "- `confidence_score` - Decision confidence (0-100)\n",
  "- `precision_benefit` - Qualitative benefit level\n",
  "- `rec_rationale` - Justification for recommendation\n\n",
  "### Quality Flags\n",
  "- `reliable` - TRUE if trustworthy (≥3 plots, <80% NEFIN)\n",
  "- `high_variance` - TRUE if heterogeneous (SD >50)\n",
  "- `high_uncertainty` - TRUE if uncertain estimates\n\n",
  "## Usage Examples\n\n",
  "### QGIS\n",
  "```\n",
  "1. Load GeoJSON file\n",
  "2. Symbology → Categorized\n",
  "3. Column: recommendation_simple\n",
  "4. Colors: Green (NEFIN-only), Orange (FIA+NEFIN), Blue (FIA-only)\n",
  "```\n\n",
  "### Python\n",
  "```python\n",
  "import geopandas as gpd\n",
  "import matplotlib.pyplot as plt\n\n",
  "# Load data\n",
  "hex_data = gpd.read_file('hex_1kha_decision.geojson')\n\n",
  "# Filter by recommendation\n",
  "use_nefin = hex_data[hex_data['recommendation_simple'] == 'NEFIN-only']\n\n",
  "# Map recommendations\n",
  "colors = {'NEFIN-only': 'green', 'FIA + NEFIN': 'orange', 'FIA-only': 'blue'}\n",
  "hex_data.plot(column='recommendation_simple', \n",
  "             categorical=True, \n",
  "             color=[colors[x] for x in hex_data['recommendation_simple']],\n",
  "             legend=True)\n",
  "```\n\n",
  "### R\n",
  "```r\n",
  "library(sf)\n",
  "library(ggplot2)\n\n",
  "# Load data\n",
  "hex_data <- st_read('hex_1kha_decision.geojson')\n\n",
  "# Create map\n",
  "ggplot(hex_data) +\n",
  "  geom_sf(aes(fill = recommendation_simple), color = NA) +\n",
  "  scale_fill_manual(\n",
  "    values = c('NEFIN-only' = '#2ca02c',\n",
  "               'FIA + NEFIN' = '#ff7f0e',\n",
  "               'FIA-only' = '#1f77b4')\n",
  "  )\n",
  "```\n\n",
  "## Interpretation Guide\n\n",
  "**High confidence_score (>90)**: Strong recommendation, sufficient data\n\n",
  "**Low confidence_score (<50)**: Use caution, limited data or biased sample\n\n",
  "**reliable = TRUE**: Trustworthy estimates from adequate sample\n\n",
  "**high_variance = TRUE**: Heterogeneous conditions within hexagon\n\n",
  "**high_uncertainty = TRUE**: Uncertain estimates, interpret with care\n\n",
  "## Citation\n\n",
  "[Your citation information]\n\n"
)

readme_file <- "data/processed/hex_geojson_with_decisions/README.md"
writeLines(readme_content, readme_file)
cat("✓ Saved README:", readme_file, "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("  ENHANCED GEOJSON CREATION COMPLETE\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Location: data/processed/hex_geojson_with_decisions/\n\n")

cat("Files include:\n")
cat("  • Hexagon geometry\n")
cat("  • Biomass statistics\n")
cat("  • Decision recommendations\n")
cat("  • Confidence scores\n")
cat("  • Quality flags\n")
cat("  • Interpretive rationale\n\n")

cat("Use these for:\n")
cat("  • Publication figures\n")
cat("  • Web mapping applications\n")
cat("  • GIS analysis (QGIS, ArcGIS)\n")
cat("  • Practitioner guidance\n")
cat("  • Decision support tools\n\n")

cat("Complete! ✨\n\n")
