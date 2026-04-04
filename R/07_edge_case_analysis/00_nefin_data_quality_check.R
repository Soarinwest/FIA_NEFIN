# =============================================================================
# NEFIN Growth Rate Anomaly Check - SIMPLIFIED
# =============================================================================
# Purpose:
#   Track individual trees measured multiple times and flag impossible 
#   growth rates that indicate data entry errors
#
# Focus: Only growth rate anomalies, not other data quality issues
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
})

set.seed(42)  # matches CONFIG$monte_carlo$seed

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  NEFIN GROWTH RATE ANOMALY CHECK\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

# Maximum reasonable annual growth rates (cm/year diameter)
MAX_ANNUAL_GROWTH_CM <- 2.0  # Very generous - most trees grow < 1 cm/year
MIN_ANNUAL_SHRINKAGE_CM <- -1.0  # Allow some measurement error

# Minimum interval between measurements (years)
MIN_INTERVAL_YEARS <- 1

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading NEFIN tree data...\n")

nefin_raw <- read_csv("data/raw/nefin/TREE_RAW_DATA.csv", show_col_types = FALSE)

cat("  Total records:", format(nrow(nefin_raw), big.mark = ","), "\n\n")

# =============================================================================
# ANALYZE GROWTH RATES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("GROWTH RATE ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Track trees measured multiple times
cat("Finding trees with multiple measurements...\n")

tree_histories <- nefin_raw %>%
  filter(!is.na(`_nefin_treeID`), !is.na(DBH), !is.na(treeSampleYear), DBH > 0) %>%
  group_by(`_nefin_treeID`) %>%
  filter(n() > 1) %>%  # Only trees measured 2+ times
  arrange(`_nefin_treeID`, treeSampleYear) %>%
  mutate(
    n_measurements = n(),
    years_since_last = treeSampleYear - lag(treeSampleYear),
    dbh_change = DBH - lag(DBH),
    annual_growth = dbh_change / years_since_last,
    dbh_previous = lag(DBH),
    year_previous = lag(treeSampleYear)
  ) %>%
  ungroup() %>%
  filter(!is.na(annual_growth), years_since_last >= MIN_INTERVAL_YEARS)

if (nrow(tree_histories) == 0) {
  cat("\n⚠ No trees found with multiple measurements!\n")
  cat("  This suggests _nefin_treeID may not reliably track individuals\n")
  cat("  Or trees are only measured once in your dataset\n\n")
  stop("Cannot proceed without remeasurement data")
}

cat("  Trees with 2+ measurements:", 
    n_distinct(tree_histories$`_nefin_treeID`), "\n")
cat("  Total remeasurement intervals analyzed:", 
    format(nrow(tree_histories), big.mark = ","), "\n\n")

# =============================================================================
# GROWTH RATE SUMMARY
# =============================================================================

cat("Overall growth rate statistics:\n")

growth_summary <- tree_histories %>%
  summarise(
    intervals = n(),
    mean_years = mean(years_since_last, na.rm = TRUE),
    median_years = median(years_since_last, na.rm = TRUE),
    mean_growth = mean(annual_growth, na.rm = TRUE),
    median_growth = median(annual_growth, na.rm = TRUE),
    sd_growth = sd(annual_growth, na.rm = TRUE),
    min_growth = min(annual_growth, na.rm = TRUE),
    max_growth = max(annual_growth, na.rm = TRUE),
    q05 = quantile(annual_growth, 0.05, na.rm = TRUE),
    q95 = quantile(annual_growth, 0.95, na.rm = TRUE),
    pct_negative = 100 * mean(annual_growth < 0, na.rm = TRUE)
  )

print(as.data.frame(growth_summary))

cat("\n")

# =============================================================================
# FLAG ANOMALIES
# =============================================================================

cat("Flagging anomalous growth rates...\n")

# Too fast
too_fast <- tree_histories %>%
  filter(annual_growth > MAX_ANNUAL_GROWTH_CM) %>%
  arrange(desc(annual_growth))

# Too much shrinkage
too_much_shrink <- tree_histories %>%
  filter(annual_growth < MIN_ANNUAL_SHRINKAGE_CM) %>%
  arrange(annual_growth)

cat("\n")
cat("  Impossible growth (>", MAX_ANNUAL_GROWTH_CM, "cm/yr):", 
    nrow(too_fast), "intervals\n")
cat("  Excessive shrinkage (<", MIN_ANNUAL_SHRINKAGE_CM, "cm/yr):", 
    nrow(too_much_shrink), "intervals\n")
cat("  Total anomalies:", 
    nrow(too_fast) + nrow(too_much_shrink), 
    sprintf("(%.2f%% of intervals)", 
            100 * (nrow(too_fast) + nrow(too_much_shrink)) / nrow(tree_histories)), "\n\n")

# =============================================================================
# DETAILED ANOMALY REPORTS
# =============================================================================

if (nrow(too_fast) > 0) {
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("TREES WITH IMPOSSIBLE GROWTH RATES\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("Top 30 cases of impossible growth:\n\n")
  
  too_fast_display <- too_fast %>%
    select(`_nefin_treeID`, `_nefin_plotID`, treeSpecies, 
           year_previous, treeSampleYear, years_since_last,
           dbh_previous, DBH, dbh_change, annual_growth) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  
  print(head(too_fast_display, 30), n = 30)
  
  # Summary by species
  cat("\n\nGrowth anomalies by species (top 20):\n")
  species_summary <- too_fast %>%
    count(treeSpecies, sort = TRUE) %>%
    mutate(pct = 100 * n / sum(n))
  
  print(head(species_summary, 20), n = 20)
}

if (nrow(too_much_shrink) > 0) {
  cat("\n\n═══════════════════════════════════════════════════════════════════\n")
  cat("TREES WITH EXCESSIVE SHRINKAGE\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("Top 20 cases of excessive shrinkage:\n\n")
  
  shrink_display <- too_much_shrink %>%
    select(`_nefin_treeID`, `_nefin_plotID`, treeSpecies, 
           year_previous, treeSampleYear, years_since_last,
           dbh_previous, DBH, dbh_change, annual_growth) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  
  print(head(shrink_display, 20), n = 20)
}

# =============================================================================
# CREATE FLAGGED DATASET
# =============================================================================

cat("\n\n═══════════════════════════════════════════════════════════════════\n")
cat("CREATING CLEANED DATASET\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Get list of problematic tree IDs
anomalous_tree_ids <- unique(c(
  too_fast$`_nefin_treeID`,
  too_much_shrink$`_nefin_treeID`
))

cat("Trees with anomalous growth:", length(anomalous_tree_ids), "\n")

# Flag all measurements of problematic trees
nefin_flagged <- nefin_raw %>%
  mutate(
    flag_growth_anomaly = `_nefin_treeID` %in% anomalous_tree_ids
  )

# Count how many records are affected
flag_counts <- nefin_flagged %>%
  filter(!is.na(DBH), DBH > 0) %>%
  summarise(
    total_trees = n(),
    flagged_trees = sum(flag_growth_anomaly, na.rm = TRUE),
    pct_flagged = 100 * flagged_trees / total_trees
  )

cat("  Total tree records:", format(flag_counts$total_trees, big.mark = ","), "\n")
cat("  Flagged records:", format(flag_counts$flagged_trees, big.mark = ","), 
    sprintf("(%.2f%%)\n", flag_counts$pct_flagged))

# Create clean dataset
nefin_clean <- nefin_flagged %>%
  filter(!flag_growth_anomaly | is.na(flag_growth_anomaly))

cat("  Clean records:", format(nrow(nefin_clean), big.mark = ","), "\n\n")

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

output_dir <- "data/processed/nefin_growth_check"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save flagged dataset
write_csv(nefin_flagged, file.path(output_dir, "nefin_trees_flagged.csv"))
cat("✓ Saved flagged dataset:", 
    file.path(output_dir, "nefin_trees_flagged.csv"), "\n")

# Save clean dataset
write_csv(nefin_clean, file.path(output_dir, "nefin_trees_clean.csv"))
cat("✓ Saved clean dataset:", 
    file.path(output_dir, "nefin_trees_clean.csv"), "\n")

# Save growth history analysis
write_csv(tree_histories, file.path(output_dir, "tree_growth_histories.csv"))
cat("✓ Saved growth histories:", 
    file.path(output_dir, "tree_growth_histories.csv"), "\n")

# Save anomalous cases
if (nrow(too_fast) > 0 || nrow(too_much_shrink) > 0) {
  anomalies <- bind_rows(
    too_fast %>% mutate(anomaly_type = "impossible_growth"),
    too_much_shrink %>% mutate(anomaly_type = "excessive_shrinkage")
  )
  
  write_csv(anomalies, file.path(output_dir, "growth_anomalies_detailed.csv"))
  cat("✓ Saved anomaly details:", 
      file.path(output_dir, "growth_anomalies_detailed.csv"), "\n")
}

# =============================================================================
# VISUALIZATIONS
# =============================================================================

cat("\nCreating diagnostic plots...\n")

# Plot 1: Growth rate distribution
p_growth_dist <- ggplot(tree_histories, aes(x = annual_growth)) +
  geom_histogram(bins = 100, fill = "gray70", alpha = 0.8) +
  geom_vline(xintercept = c(MIN_ANNUAL_SHRINKAGE_CM, MAX_ANNUAL_GROWTH_CM),
             linetype = "dashed", color = "red", linewidth = 1) +
  coord_cartesian(xlim = c(-5, 5)) +
  theme_minimal() +
  labs(
    title = "Annual Growth Rate Distribution",
    subtitle = "Red lines = thresholds for flagging",
    x = "Annual DBH Growth (cm/year)",
    y = "Count"
  )

ggsave(file.path(output_dir, "growth_rate_distribution.png"),
       p_growth_dist, width = 10, height = 6, dpi = 300)

# Plot 2: Growth vs initial size
p_growth_size <- tree_histories %>%
  filter(abs(annual_growth) < 5) %>%  # Remove extreme outliers for viz
  ggplot(aes(x = dbh_previous, y = annual_growth)) +
  geom_hex(bins = 50) +
  geom_hline(yintercept = c(MIN_ANNUAL_SHRINKAGE_CM, MAX_ANNUAL_GROWTH_CM),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(trans = "log10") +
  theme_minimal() +
  labs(
    title = "Growth Rate vs Initial Tree Size",
    subtitle = "Red lines = anomaly thresholds",
    x = "Initial DBH (cm)",
    y = "Annual Growth Rate (cm/year)",
    fill = "Count"
  )

ggsave(file.path(output_dir, "growth_vs_size.png"),
       p_growth_size, width = 10, height = 7, dpi = 300)

# Plot 3: Growth over time
p_growth_time <- tree_histories %>%
  filter(abs(annual_growth) < 5) %>%
  ggplot(aes(x = treeSampleYear, y = annual_growth)) +
  geom_hex(bins = 50) +
  geom_hline(yintercept = c(MIN_ANNUAL_SHRINKAGE_CM, MAX_ANNUAL_GROWTH_CM),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(trans = "log10") +
  theme_minimal() +
  labs(
    title = "Growth Rate Over Time",
    x = "Measurement Year",
    y = "Annual Growth Rate (cm/year)",
    fill = "Count"
  )

ggsave(file.path(output_dir, "growth_over_time.png"),
       p_growth_time, width = 10, height = 6, dpi = 300)

cat("✓ Plots saved\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Growth rate analysis:\n")
cat(sprintf("  Trees tracked over time: %s\n", 
            format(n_distinct(tree_histories$`_nefin_treeID`), big.mark = ",")))
cat(sprintf("  Remeasurement intervals: %s\n", 
            format(nrow(tree_histories), big.mark = ",")))
cat(sprintf("  Median interval: %.1f years\n", 
            growth_summary$median_years))
cat(sprintf("  Median growth rate: %.3f cm/year\n", 
            growth_summary$median_growth))

cat("\nAnomalies detected:\n")
cat(sprintf("  Impossible growth: %s intervals\n", 
            format(nrow(too_fast), big.mark = ",")))
cat(sprintf("  Excessive shrinkage: %s intervals\n", 
            format(nrow(too_much_shrink), big.mark = ",")))
cat(sprintf("  Trees affected: %s\n", 
            format(length(anomalous_tree_ids), big.mark = ",")))

cat("\nData cleaning impact:\n")
cat(sprintf("  Original records: %s\n", 
            format(flag_counts$total_trees, big.mark = ",")))
cat(sprintf("  Flagged records: %s (%.2f%%)\n", 
            format(flag_counts$flagged_trees, big.mark = ","),
            flag_counts$pct_flagged))
cat(sprintf("  Clean records: %s\n", 
            format(nrow(nefin_clean), big.mark = ",")))

cat("\n\nRecommendation:\n")
if (flag_counts$pct_flagged > 5) {
  cat("  ⚠ HIGH percentage of flagged records!\n")
  cat("  → Carefully review growth_anomalies_detailed.csv\n")
  cat("  → Consider adjusting thresholds if needed\n")
  cat("  → Use nefin_trees_clean.csv for tail analysis\n")
} else if (flag_counts$pct_flagged > 1) {
  cat("  ⚠ Moderate percentage of flagged records\n")
  cat("  → Review anomalies in growth_anomalies_detailed.csv\n")
  cat("  → Use clean dataset for analysis\n")
} else {
  cat("  ✓ Low percentage of flagged records\n")
  cat("  → Growth data quality appears good\n")
  cat("  → Safe to use clean dataset\n")
}

cat("\n\nNext steps:\n")
cat("  1. Review growth_anomalies_detailed.csv\n")
cat("  2. Check if anomalies are species-specific\n")
cat("  3. Update gap analysis scripts to use nefin_trees_clean.csv\n")
cat("  4. Re-run extreme tail analysis\n")

cat("\n═══════════════════════════════════════════════════════════════════\n\n")
