# =============================================================================
# Calculate Max Tree Metrics Per Plot
# =============================================================================
# Purpose: Extract maximum DBH and HT per plot for use as covariates
#          in Phase 4 predictive modeling
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MAX TREE PER PLOT ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD PLOT-LEVEL DATA
# =============================================================================

cat("Loading plot data...\n")

# NEFIN plots (use TREE_PLOT_DATA.csv which has plot-level info)
nefin_plot_file <- "data/raw/nefin/TREE_PLOT_DATA.csv"

if (!file.exists(nefin_plot_file)) {
  cat("  ⚠ TREE_PLOT_DATA.csv not found, trying NEFIN_plots.csv...\n")
  nefin_plot_file <- "data/raw/nefin/NEFIN_plots.csv"
}

if (!file.exists(nefin_plot_file)) {
  cat("  ⚠ Using processed plots file...\n")
  nefin_plot_file <- "data/processed/nefin_plots.csv"
}

nefin_plots <- read_csv(nefin_plot_file, show_col_types = FALSE)

# FIA plots - try multiple locations
fia_plot_file <- "data/processed/fia_plots.csv"
if (!file.exists(fia_plot_file)) {
  cat("  ⚠ fia_plots.csv not found, will extract from trees\n")
  fia_plots <- NULL
} else {
  fia_plots <- read_csv(fia_plot_file, show_col_types = FALSE)
}

cat("  NEFIN plots:", nrow(nefin_plots), "\n")
if (!is.null(fia_plots)) {
  cat("  FIA plots:", nrow(fia_plots), "\n\n")
} else {
  cat("  FIA plots: Will calculate from trees\n\n")
}

# =============================================================================
# LOAD TREE-LEVEL DATA
# =============================================================================

cat("Loading tree data...\n")

# NEFIN trees
nefin_tree_file <- "data/raw/nefin/TREE_RAW_DATA.csv"

nefin_trees <- read_csv(nefin_tree_file, show_col_types = FALSE)

# Flexible column detection
nefin_trees <- nefin_trees %>%
  transmute(
    dataset = "NEFIN",
    # Plot ID - try multiple column names
    plot_id = as.character(
      if ("plotID" %in% names(.)) plotID
      else if ("plot_id" %in% names(.)) plot_id
      else if ("PLOT_ID" %in% names(.)) PLOT_ID
      else row_number()
    ),
    tree_id = as.character(row_number()),
    # Species
    species = if ("treeSpecies" %in% names(.)) str_squish(treeSpecies)
    else if ("species" %in% names(.)) str_squish(species)
    else NA_character_,
    # DBH
    dbh_cm = if ("DBH" %in% names(.)) as.numeric(DBH)
    else if ("dbh" %in% names(.)) as.numeric(dbh)
    else NA_real_,
    # Height
    ht_m = if ("fldTotalHeight" %in% names(.)) as.numeric(fldTotalHeight)
    else if ("HT" %in% names(.)) as.numeric(HT)
    else if ("totalHeight" %in% names(.)) as.numeric(totalHeight)
    else NA_real_,
    # Status
    status = if ("treeStatus" %in% names(.)) treeStatus
    else if ("status" %in% names(.)) status
    else 1
  ) %>%
  filter(status == 1, !is.na(dbh_cm), dbh_cm > 0)

cat("  NEFIN trees loaded:", format(nrow(nefin_trees), big.mark = ","), "\n")

# FIA trees - check if we have tree-level extract
fia_tree_file <- "data/processed/fia_tree.csv"

if (file.exists(fia_tree_file)) {
  fia_trees <- read_csv(fia_tree_file, show_col_types = FALSE) %>%
    transmute(
      dataset = "FIA",
      plot_id = PLT_CN,  # Or appropriate plot ID
      tree_id = TREE_CN,
      species = COMMON_NAME,
      dbh_cm = DIA * 2.54,
      ht_m = HT * 0.3048,
      status = STATUSCD
    ) %>%
    filter(status == 1, !is.na(dbh_cm), dbh_cm > 0)
  
  cat("  FIA trees loaded:", format(nrow(fia_trees), big.mark = ","), "\n\n")
} else {
  cat("  ⚠ FIA tree file not found, will extract from databases...\n")
  
  # Extract from databases (simplified version)
  fia_base_dir <- "data/raw/fia_sqlite"
  state_dirs <- list.dirs(fia_base_dir, recursive = FALSE, full.names = TRUE)
  
  fia_dbs <- c()
  for (state_dir in state_dirs) {
    unzipped_dir <- file.path(state_dir, "unzipped")
    if (dir.exists(unzipped_dir)) {
      dbs <- list.files(unzipped_dir, pattern = "SQLite_FIADB.*\\.db$", 
                        full.names = TRUE)
      if (length(dbs) > 0) {
        fia_dbs <- c(fia_dbs, dbs)
      }
    }
  }
  
  # Pull trees from all databases
  library(RSQLite)
  
  pull_trees <- function(db_path) {
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    query <- "
      SELECT 
        p.CN as plot_cn,
        t.CN as tree_cn,
        t.DIA,
        t.HT
      FROM TREE t
      JOIN PLOT p ON t.PLT_CN = p.CN
      WHERE t.STATUSCD = 1 AND t.DIA IS NOT NULL
    "
    
    trees <- tryCatch(dbGetQuery(con, query), error = function(e) NULL)
    
    if (!is.null(trees)) {
      trees %>%
        transmute(
          dataset = "FIA",
          plot_id = as.character(plot_cn),
          tree_id = as.character(tree_cn),
          dbh_cm = DIA * 2.54,
          ht_m = if (!is.null(HT)) HT * 0.3048 else NA_real_
        )
    }
  }
  
  fia_trees_list <- lapply(fia_dbs, pull_trees)
  fia_trees <- bind_rows(fia_trees_list[!sapply(fia_trees_list, is.null)])
  
  cat("  FIA trees extracted:", format(nrow(fia_trees), big.mark = ","), "\n\n")
}

# =============================================================================
# CALCULATE MAX TREE METRICS
# =============================================================================

cat("Computing max tree metrics per plot...\n")

# NEFIN max trees
nefin_max <- nefin_trees %>%
  group_by(plot_id) %>%
  summarise(
    dataset = "NEFIN",
    max_dbh_cm = max(dbh_cm, na.rm = TRUE),
    max_ht_m = max(ht_m, na.rm = TRUE),
    max_tree_species = species[which.max(dbh_cm)],
    n_trees = n(),
    mean_dbh_cm = mean(dbh_cm, na.rm = TRUE),
    p95_dbh_cm = quantile(dbh_cm, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    max_ht_m = if_else(is.infinite(max_ht_m), NA_real_, max_ht_m)
  )

cat("  NEFIN plots with max metrics:", nrow(nefin_max), "\n")

# FIA max trees
fia_max <- fia_trees %>%
  group_by(plot_id) %>%
  summarise(
    dataset = "FIA",
    max_dbh_cm = max(dbh_cm, na.rm = TRUE),
    max_ht_m = max(ht_m, na.rm = TRUE),
    n_trees = n(),
    mean_dbh_cm = mean(dbh_cm, na.rm = TRUE),
    p95_dbh_cm = quantile(dbh_cm, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    max_ht_m = if_else(is.infinite(max_ht_m), NA_real_, max_ht_m)
  )

cat("  FIA plots with max metrics:", nrow(fia_max), "\n\n")

# Combine
max_tree_metrics <- bind_rows(nefin_max, fia_max)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY STATISTICS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

summary_stats <- max_tree_metrics %>%
  group_by(dataset) %>%
  summarise(
    n_plots = n(),
    max_dbh_mean = mean(max_dbh_cm, na.rm = TRUE),
    max_dbh_median = median(max_dbh_cm, na.rm = TRUE),
    max_dbh_p95 = quantile(max_dbh_cm, 0.95, na.rm = TRUE),
    max_dbh_max = max(max_dbh_cm, na.rm = TRUE),
    max_ht_mean = mean(max_ht_m, na.rm = TRUE),
    max_ht_median = median(max_ht_m, na.rm = TRUE),
    max_ht_p95 = quantile(max_ht_m, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

cat("Max DBH by dataset:\n")
print(summary_stats %>% 
        select(dataset, n_plots, max_dbh_mean, max_dbh_median, 
               max_dbh_p95, max_dbh_max))

cat("\nMax HT by dataset:\n")
print(summary_stats %>% 
        select(dataset, n_plots, max_ht_mean, max_ht_median, max_ht_p95))

cat("\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_dir <- "data/processed/max_tree_metrics"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(max_tree_metrics, file.path(output_dir, "plot_max_tree_metrics.csv"))
write_csv(summary_stats, file.path(output_dir, "summary_by_dataset.csv"))

cat("✓ Saved results to:", output_dir, "\n")
cat("  - plot_max_tree_metrics.csv (", nrow(max_tree_metrics), "plots)\n")
cat("  - summary_by_dataset.csv\n\n")

# =============================================================================
# USAGE NOTES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  USAGE FOR PHASE 4 MODELING\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("To use in predictive models:\n\n")
cat("  plots <- read_csv('data/processed/fia_plots.csv') %>%\n")
cat("    left_join(read_csv('data/processed/max_tree_metrics/plot_max_tree_metrics.csv'),\n")
cat("              by = 'plot_id')\n\n")

cat("Model formulas:\n")
cat("  Baseline:  biomass ~ ndvi + tmean + ppt\n")
cat("  Enhanced:  biomass ~ ndvi + tmean + ppt + max_dbh_cm\n")
cat("  Full:      biomass ~ ndvi + tmean + ppt + max_dbh_cm + max_ht_m\n\n")

cat("Expected benefit:\n")
cat("  Max tree metrics capture stand development stage\n")
cat("  Reduces within-plot correlation\n")
cat("  Likely improves model R² by 0.05-0.10\n\n")

cat("Analysis complete!\n\n")
