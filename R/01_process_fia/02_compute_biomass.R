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

# Load configuration and utilities
source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/biomass_utils.R")
source("R/utils/validation_utils.R")

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  STEP 2: COMPUTE PLOT-LEVEL BIOMASS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Input directory
input_dir <- file.path(CONFIG$paths$interim_fia, "extracted")

# Output directory
output_dir <- file.path(CONFIG$paths$interim_fia, "biomass")
ensure_dir(output_dir)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading extracted data...\n\n")

plot_path <- file.path(input_dir, "plot.csv")
tree_path <- file.path(input_dir, "tree.csv")
cond_path <- file.path(input_dir, "cond.csv")

plot_df <- load_csv_validated(plot_path, c("CN", "LAT", "LON", "MEASYEAR"))
tree_df <- load_csv_validated(tree_path, c("CN", "PLT_CN", "DRYBIO_AG", "TPA_UNADJ"))
cond_df <- load_csv_validated(cond_path, c("CN", "PLT_CN"))

# =============================================================================
# FILTER TO LIVE TREES
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Filtering to Live Trees\n")
cat("───────────────────────────────────────\n\n")

cat("Total trees:", nrow(tree_df), "\n")

# Filter to live trees with valid biomass
tree_live <- tree_df %>%
  filter(
    STATUSCD == 1,              # Live trees only
    !is.na(DRYBIO_AG),         # Has biomass measurement
    DRYBIO_AG >= 0,            # Non-negative
    !is.na(TPA_UNADJ),         # Has expansion factor
    TPA_UNADJ > 0              # Valid expansion
  )

cat("Live trees with valid biomass:", nrow(tree_live), "\n")
cat("Percentage retained:", sprintf("%.1f%%", 100 * nrow(tree_live) / nrow(tree_df)), "\n")

# =============================================================================
# COMPUTE PLOT-LEVEL BIOMASS
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Computing Plot-Level Biomass\n")
cat("───────────────────────────────────────\n\n")

# Use utility function to compute plot biomass
plot_biomass <- compute_plot_biomass_fia(
  tree_live,
  biomass_col = "DRYBIO_AG",
  tpa_col = "TPA_UNADJ"
)

cat("Plots with biomass:", nrow(plot_biomass), "\n")

# Add summary statistics
biomass_stats <- summarize_biomass(plot_biomass, "biomass")
cat("\nBiomass distribution:\n")
print(biomass_stats)

# =============================================================================
# JOIN WITH PLOT METADATA
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Joining with Plot Metadata\n")
cat("───────────────────────────────────────\n\n")

# Join plot biomass with plot locations and metadata
plot_complete <- plot_df %>%
  inner_join(plot_biomass, by = "CN") %>%
  # Standardize column names
  rename(
    lat = LAT,
    lon = LON
  ) %>%
  # Select key columns
  select(
    CN, STATECD, COUNTYCD, PLOT, MEASYEAR,
    lat, lon,
    biomass, n_trees,
    everything()
  )

cat("Final dataset:", nrow(plot_complete), "plots\n")

# =============================================================================
# VALIDATION
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Validation\n")
cat("───────────────────────────────────────\n\n")

# Validate coordinates
plot_complete <- validate_coordinates(
  plot_complete,
  bounds = list(
    lat_min = 40,  # Southern boundary
    lat_max = 48,  # Northern boundary
    lon_min = -75, # Western boundary
    lon_max = -66  # Eastern boundary
  )
)

# Validate biomass
plot_complete <- validate_biomass(
  plot_complete,
  min_valid = 0,
  max_valid = CONFIG$thresholds$max_biomass_Mg_ha
)

# Remove invalid records
n_before <- nrow(plot_complete)

plot_complete <- plot_complete %>%
  filter(coord_valid, biomass_valid) %>%
  select(-coord_valid, -coord_flag, -biomass_valid, -biomass_flag)

n_after <- nrow(plot_complete)

if (n_before > n_after) {
  cat("\nRemoved", n_before - n_after, "plots with invalid coordinates or biomass\n")
}

cat("\nFinal valid plots:", n_after, "\n")

# =============================================================================
# SAVE OUTPUT
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Saving Output\n")
cat("───────────────────────────────────────\n\n")

output_path <- file.path(output_dir, "fia_plot_biomass.csv")

write_csv(plot_complete, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(plot_complete), "\n")
cat("  Size:", get_file_size(output_path), "\n")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

summary_path <- file.path(output_dir, "biomass_summary.txt")

sink(summary_path)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  FIA PLOT BIOMASS SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Run time:", as.character(Sys.time()), "\n")
cat("Input trees:", nrow(tree_df), "\n")
cat("Live trees with biomass:", nrow(tree_live), "\n")
cat("Output plots:", nrow(plot_complete), "\n\n")

cat("BIOMASS STATISTICS (Mg/ha):\n\n")
stats <- summarize_biomass(plot_complete, "biomass")
cat(sprintf("  Mean:     %.2f\n", stats$mean))
cat(sprintf("  Median:   %.2f\n", stats$median))
cat(sprintf("  SD:       %.2f\n", stats$sd))
cat(sprintf("  Min:      %.2f\n", stats$min))
cat(sprintf("  Max:      %.2f\n", stats$max))
cat(sprintf("  Q25:      %.2f\n", stats$q25))
cat(sprintf("  Q75:      %.2f\n", stats$q75))

cat("\n")
cat("PLOTS BY STATE:\n\n")
by_state <- plot_complete %>%
  count(STATECD) %>%
  arrange(desc(n))

for (i in 1:nrow(by_state)) {
  state_code <- by_state$STATECD[i]
  state_name <- names(CONFIG$state_codes)[match(state_code, CONFIG$state_codes)]
  cat(sprintf("  %s (%2d): %5d plots\n", 
              state_name, state_code, by_state$n[i]))
}

cat("\n")
cat("PLOTS BY YEAR:\n\n")
by_year <- plot_complete %>%
  count(MEASYEAR) %>%
  arrange(MEASYEAR)

for (i in 1:nrow(by_year)) {
  cat(sprintf("  %d: %5d plots\n", 
              by_year$MEASYEAR[i], by_year$n[i]))
}

cat("\n")
cat("TREE DENSITY:\n\n")
tree_stats <- plot_complete %>%
  summarise(
    mean_trees = mean(n_trees),
    median_trees = median(n_trees),
    min_trees = min(n_trees),
    max_trees = max(n_trees)
  )

cat(sprintf("  Mean trees per plot:   %.1f\n", tree_stats$mean_trees))
cat(sprintf("  Median trees per plot: %.0f\n", tree_stats$median_trees))
cat(sprintf("  Min trees per plot:    %d\n", tree_stats$min_trees))
cat(sprintf("  Max trees per plot:    %d\n", tree_stats$max_trees))

cat("\n")
cat("OUTPUT FILE:\n\n")
cat("  ", output_path, "\n")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")

sink()

cat("\n✓ Summary written:", summary_path, "\n")

# =============================================================================
# DIAGNOSTIC PLOTS
# =============================================================================

cat("\n")
cat("───────────────────────────────────────\n")
cat("Creating Diagnostic Plots\n")
cat("───────────────────────────────────────\n\n")

suppressPackageStartupMessages(library(ggplot2))

# Biomass histogram
p1 <- ggplot(plot_complete, aes(x = biomass)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "FIA Plot Biomass Distribution",
    x = "Aboveground Biomass (Mg/ha)",
    y = "Number of Plots"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "biomass_histogram.png"),
  p1, width = 8, height = 5, dpi = 300
)

cat("✓ Saved: biomass_histogram.png\n")

# Biomass by year
p2 <- ggplot(plot_complete, aes(x = factor(MEASYEAR), y = biomass)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(
    title = "FIA Biomass by Measurement Year",
    x = "Year",
    y = "Biomass (Mg/ha)"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "biomass_by_year.png"),
  p2, width = 8, height = 5, dpi = 300
)

cat("✓ Saved: biomass_by_year.png\n")

# =============================================================================
# COMPLETE
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  BIOMASS COMPUTATION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Next step: Run R/01_process_fia/03_create_fia_dataset.R\n\n")
