# =============================================================================
# Compute Plot-Level Biomass from FIA Tree Data
# =============================================================================
# Aggregates tree-level biomass to plot level with proper expansion factors
#
# INPUTS:
#   - data/interim/fia/extracted/plot.csv
#   - data/interim/fia/extracted/tree.csv
#   - data/interim/fia/extracted/cond.csv
#
# OUTPUTS:
#   - data/interim/fia/biomass/fia_plot_biomass.csv
#   - data/interim/fia/biomass/biomass_summary.txt
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================
# =============================================================================
# Compute Plot-Level Biomass - STANDALONE (NO UTILITY FUNCTIONS)
# =============================================================================
# Does everything inline without any dependencies on utility functions
# =============================================================================

source("R/00_config/config.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  STEP 2: COMPUTE PLOT-LEVEL BIOMASS (STANDALONE)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Directories
input_dir <- file.path(CONFIG$paths$interim_fia, "extracted")
output_dir <- file.path(CONFIG$paths$interim_fia, "biomass")

# Create output dir if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading extracted data...\n")

plot_df <- read_csv(file.path(input_dir, "plot.csv"), show_col_types = FALSE)
tree_df <- read_csv(file.path(input_dir, "tree.csv"), show_col_types = FALSE)

cat("  Plots:", nrow(plot_df), "\n")
cat("  Trees:", nrow(tree_df), "\n\n")

# =============================================================================
# FILTER TO LIVE TREES
# =============================================================================

cat("Filtering to live trees with biomass...\n")

tree_live <- tree_df %>%
  filter(
    STATUSCD == 1,
    !is.na(DRYBIO_AG),
    DRYBIO_AG >= 0,
    !is.na(TPA_UNADJ),
    TPA_UNADJ > 0
  )

cat("  Live trees:", nrow(tree_live), "\n\n")

# =============================================================================
# AGGREGATE TO PLOT LEVEL - INLINE!
# =============================================================================

cat("Aggregating trees to plots...\n")

plot_biomass <- tree_live %>%
  mutate(
    # Expand biomass: pounds * trees per acre
    biomass_expanded_lbs_per_acre = DRYBIO_AG * TPA_UNADJ
  ) %>%
  group_by(PLT_CN) %>%
  summarise(
    # Sum expanded biomass in pounds/acre
    total_lbs_per_acre = sum(biomass_expanded_lbs_per_acre, na.rm = TRUE),
    n_trees = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert pounds/acre → Mg/ha
    # 1 lb/acre = 0.001121 Mg/ha (from CONFIG)
    biomass = total_lbs_per_acre * CONFIG$fia$lb_per_acre_to_Mg_per_ha
  ) %>%
  select(PLT_CN, biomass, n_trees)

cat("  ✓ Aggregated to", nrow(plot_biomass), "plots\n")
cat("  Expected ~", nrow(plot_df), "plots\n\n")

# Check if aggregation worked
if (nrow(plot_biomass) > nrow(plot_df) * 1.5) {
  cat("  ⚠ WARNING: Too many rows after aggregation!\n")
  cat("    This suggests group_by didn't work properly\n\n")
}

# =============================================================================
# JOIN WITH PLOT METADATA
# =============================================================================

cat("Joining biomass with plot metadata...\n")
cat("  Join: plot_df$CN = plot_biomass$PLT_CN\n")

# Show sample values for debugging
cat("  Sample plot CNs:", paste(head(plot_df$CN, 3), collapse=", "), "\n")
cat("  Sample PLT_CNs:", paste(head(plot_biomass$PLT_CN, 3), collapse=", "), "\n")

# Check for matches BEFORE join
n_matches <- sum(plot_df$CN %in% plot_biomass$PLT_CN)
cat("  Matching plots:", n_matches, "out of", nrow(plot_df), "\n\n")

if (n_matches == 0) {
  cat("  ⚠ ERROR: NO MATCHES FOUND!\n")
  cat("    plot_df$CN and plot_biomass$PLT_CN have no overlap\n")
  cat("    This will result in 0 rows after join\n\n")
  stop("Join will fail - no matching CNs!")
}

# Perform join
plot_complete <- plot_df %>%
  inner_join(plot_biomass, by = c("CN" = "PLT_CN")) %>%
  rename(lat = LAT, lon = LON) %>%
  select(CN, STATECD, COUNTYCD, PLOT, MEASYEAR, lat, lon, biomass, n_trees)

cat("  ✓ Join complete:", nrow(plot_complete), "plots\n\n")

# =============================================================================
# SIMPLE VALIDATION
# =============================================================================

cat("Validating...\n")

n_before <- nrow(plot_complete)

n_after <- nrow(plot_complete)

if (n_before > n_after) {
  cat("  Removed", n_before - n_after, "invalid plots\n")
}

cat("  ✓ Valid plots:", n_after, "\n\n")

# =============================================================================
# SAVE
# =============================================================================

output_path <- file.path(output_dir, "fia_plot_biomass.csv")
write_csv(plot_complete, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(plot_complete), "\n\n")

# Summary
if (nrow(plot_complete) > 0) {
  cat("Summary:\n")
  cat("  Mean biomass:", sprintf("%.2f Mg/ha\n", mean(plot_complete$biomass)))
  cat("  Median biomass:", sprintf("%.2f Mg/ha\n", median(plot_complete$biomass)))
  cat("  Range:", sprintf("%.2f - %.2f Mg/ha\n", 
                          min(plot_complete$biomass), 
                          max(plot_complete$biomass)))
  cat("  Mean trees/plot:", sprintf("%.1f\n", mean(plot_complete$n_trees)))
}

cat("\n✓ Step 2 complete!\n")
cat("Next: R/01_process_fia/03_create_fia_dataset.R\n\n")