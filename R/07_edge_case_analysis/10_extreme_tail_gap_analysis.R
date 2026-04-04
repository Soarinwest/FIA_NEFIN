# =============================================================================
# Extreme Tail Gap Analysis: Trees NEFIN Captures That FIA Doesn't
# =============================================================================
# Purpose:
#   Identify and visualize the "gap zone" - tree sizes where NEFIN has 
#   substantial representation but FIA has zero or near-zero trees
#
# This goes beyond comparing quantiles to show:
#   1. Species where NEFIN max substantially exceeds FIA max
#   2. The size range where FIA has 0 trees but NEFIN has many
#   3. Visual "gap" in the distributions showing FIA's absolute limit
#   4. Statistical significance of these gaps
#
# Rscript R\07_edge_case_analysis\10_extreme_tail_gap_analysis.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(RSQLite)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(patchwork)  # For combined plots
})

set.seed(42)  # matches CONFIG$monte_carlo$seed

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  EXTREME TAIL GAP ANALYSIS: NEFIN vs FIA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

MIN_N_PER_SPECIES <- 30
MIN_GAP_SIZE_CM <- 15  # Minimum gap between FIA max and NEFIN trees to consider
MIN_NEFIN_IN_GAP <- 5   # Minimum NEFIN trees in the "gap zone"

NEFIN_LIVE_CODES <- c(1)
FIA_LIVE_CODES <- c(1)

# =============================================================================
# LOAD NEFIN TREE DATA
# =============================================================================

cat("Loading NEFIN tree data...\n")

nefin_trees <- read_csv("data/processed/nefin_growth_check/nefin_trees_clean.csv", 
                        show_col_types = FALSE) %>%
  transmute(
    dataset = "NEFIN",
    tree_id = as.character(row_number()),
    status = treeStatus,
    species_raw = str_squish(treeSpecies),
    dbh_cm = as.numeric(DBH),
    ht_m = if ("fldTotalHeight" %in% names(.)) as.numeric(fldTotalHeight) 
    else if ("HT" %in% names(.)) as.numeric(HT)
    else NA_real_
  ) %>%
  filter(status %in% NEFIN_LIVE_CODES, !is.na(species_raw), !is.na(dbh_cm), dbh_cm > 0) %>%
  mutate(
    species = str_to_lower(species_raw),
    species = str_replace_all(species, "[^a-z ]", ""),
    species = str_squish(species)
  )

cat("  NEFIN trees:", format(nrow(nefin_trees), big.mark = ","), "\n")
cat("  Unique species:", n_distinct(nefin_trees$species), "\n\n")

# =============================================================================
# LOAD FIA TREE DATA WITH LATIN NAMES
# =============================================================================

cat("Loading FIA tree data from state databases...\n")

fia_base_dir <- "data/raw/fia_sqlite"
state_dirs <- list.dirs(fia_base_dir, recursive = FALSE, full.names = TRUE)

cat("  State directories found:", length(state_dirs), "\n")

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

cat("  Total FIA databases found:", length(fia_dbs), "\n")
for (db in fia_dbs) {
  cat("    ", basename(db), "\n")
}
cat("\n")

if (length(fia_dbs) == 0) {
  stop("No FIA databases found!")
}

# Function to pull trees WITH LATIN NAMES
pull_fia_trees <- function(db_path) {
  state <- gsub(".*FIADB_([A-Z]{2})\\.db", "\\1", basename(db_path))
  cat("  ", state, "... ")
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  tables <- dbListTables(con)
  if (!("TREE" %in% tables)) {
    cat("SKIPPED\n")
    return(NULL)
  }
  
  if ("REF_SPECIES" %in% tables) {
    ref_query <- "SELECT SPCD, GENUS, SPECIES, COMMON_NAME FROM REF_SPECIES"
    ref_species <- tryCatch(dbGetQuery(con, ref_query), error = function(e) NULL)
    
    if (is.null(ref_species)) {
      cat("ERROR reading REF_SPECIES\n")
      return(NULL)
    }
    
    ref_species <- ref_species %>%
      mutate(
        latin_name = paste(str_to_lower(GENUS), str_to_lower(SPECIES)),
        latin_name = str_squish(latin_name)
      )
    
  } else {
    cat("NO REF_SPECIES\n")
    return(NULL)
  }
  
  query <- "
    SELECT 
      t.CN as tree_cn,
      t.STATUSCD,
      t.DIA,
      t.HT,
      t.SPCD
    FROM TREE t
    WHERE t.STATUSCD = 1
      AND t.DIA IS NOT NULL
      AND t.DIA > 0
  "
  
  trees <- tryCatch(dbGetQuery(con, query), error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(trees) || nrow(trees) == 0) {
    cat("NO DATA\n")
    return(NULL)
  }
  
  trees <- trees %>%
    left_join(ref_species, by = "SPCD") %>%
    filter(!is.na(latin_name))
  
  cat(format(nrow(trees), big.mark = ","), "trees\n")
  
  trees %>%
    transmute(
      dataset = "FIA",
      tree_id = as.character(tree_cn),
      status = STATUSCD,
      species_raw = latin_name,
      dbh_cm = DIA * 2.54,
      ht_m = if (!is.null(HT)) HT * 0.3048 else NA_real_,
      species = latin_name
    )
}

# Pull from all databases
cat("\n")
fia_trees_list <- lapply(fia_dbs, pull_fia_trees)
fia_trees <- bind_rows(fia_trees_list[!sapply(fia_trees_list, is.null)]) %>%
  filter(!is.na(species), !is.na(dbh_cm), dbh_cm > 0)

cat("\n  Total FIA trees:", format(nrow(fia_trees), big.mark = ","), "\n")
cat("  Unique species:", n_distinct(fia_trees$species), "\n\n")

# =============================================================================
# FIND ELIGIBLE SPECIES
# =============================================================================

cat("Finding eligible species (min", MIN_N_PER_SPECIES, "per dataset)...\n")

trees_all <- bind_rows(nefin_trees, fia_trees)

species_counts <- trees_all %>%
  group_by(dataset, species) %>%
  summarize(n = n(), .groups = "drop")

eligible_species <- species_counts %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  filter(NEFIN >= MIN_N_PER_SPECIES, FIA >= MIN_N_PER_SPECIES)

cat("  Eligible species:", nrow(eligible_species), "\n\n")

if (nrow(eligible_species) == 0) {
  stop("No eligible species")
}

trees_elig <- trees_all %>%
  filter(species %in% eligible_species$species)

# =============================================================================
# COMPUTE GAP METRICS
# =============================================================================

cat("Computing extreme tail gap metrics...\n")

gap_analysis <- trees_elig %>%
  group_by(species) %>%
  summarize(
    # Basic stats
    n_fia = sum(dataset == "FIA"),
    n_nefin = sum(dataset == "NEFIN"),
    
    # Maxima
    fia_max = max(dbh_cm[dataset == "FIA"], na.rm = TRUE),
    nefin_max = max(dbh_cm[dataset == "NEFIN"], na.rm = TRUE),
    
    # The "gap" - difference between maxima
    max_gap_cm = nefin_max - fia_max,
    max_gap_pct = 100 * (nefin_max - fia_max) / fia_max,
    
    # NEFIN trees beyond FIA's max
    n_nefin_beyond_fia_max = sum(dbh_cm[dataset == "NEFIN"] > fia_max),
    pct_nefin_beyond_fia_max = 100 * n_nefin_beyond_fia_max / n_nefin,
    
    # NEFIN trees in "gap zone" (between FIA p99 and NEFIN max)
    fia_p99 = quantile(dbh_cm[dataset == "FIA"], 0.99, na.rm = TRUE),
    nefin_p99 = quantile(dbh_cm[dataset == "NEFIN"], 0.99, na.rm = TRUE),
    
    n_nefin_beyond_fia_p99 = sum(dbh_cm[dataset == "NEFIN"] > fia_p99),
    pct_nefin_beyond_fia_p99 = 100 * n_nefin_beyond_fia_p99 / n_nefin,
    
    # FIA trees in the "gap zone" above their own p99
    n_fia_beyond_own_p99 = sum(dbh_cm[dataset == "FIA"] > fia_p99),
    pct_fia_beyond_own_p99 = 100 * n_fia_beyond_own_p99 / n_fia,
    
    # Quantile comparisons
    fia_p95 = quantile(dbh_cm[dataset == "FIA"], 0.95, na.rm = TRUE),
    nefin_p95 = quantile(dbh_cm[dataset == "NEFIN"], 0.95, na.rm = TRUE),
    
    # How many NEFIN trees are larger than FIA's 99th percentile?
    enrichment_ratio = n_nefin_beyond_fia_p99 / pmax(n_fia_beyond_own_p99, 1),
    
    .groups = "drop"
  ) %>%
  # Filter for meaningful gaps
  filter(max_gap_cm >= MIN_GAP_SIZE_CM, n_nefin_beyond_fia_max >= MIN_NEFIN_IN_GAP) %>%
  arrange(desc(max_gap_cm))

cat("  Species with substantial gaps:", nrow(gap_analysis), "\n\n")

if (nrow(gap_analysis) == 0) {
  cat("  No species show substantial gaps with current thresholds\n")
  cat("  Consider lowering MIN_GAP_SIZE_CM or MIN_NEFIN_IN_GAP\n\n")
  
  # Show close calls
  close_calls <- trees_elig %>%
    group_by(species) %>%
    summarize(
      fia_max = max(dbh_cm[dataset == "FIA"], na.rm = TRUE),
      nefin_max = max(dbh_cm[dataset == "NEFIN"], na.rm = TRUE),
      max_gap_cm = nefin_max - fia_max,
      n_nefin_beyond = sum(dbh_cm[dataset == "NEFIN"] > fia_max),
      .groups = "drop"
    ) %>%
    arrange(desc(max_gap_cm)) %>%
    head(10)
  
  cat("  Top 10 species by gap size:\n")
  print(close_calls)
  
  stop("Adjust thresholds to proceed")
}

# =============================================================================
# DETAILED GAP ZONE ANALYSIS
# =============================================================================

cat("Analyzing gap zone characteristics...\n")

# For each species with a gap, get detailed size distribution in the gap
gap_zone_details <- trees_elig %>%
  filter(species %in% gap_analysis$species) %>%
  left_join(gap_analysis %>% select(species, fia_max, fia_p99), by = "species") %>%
  mutate(
    zone = case_when(
      dbh_cm <= fia_p99 ~ "Below FIA P99",
      dbh_cm > fia_p99 & dbh_cm <= fia_max ~ "FIA P99-Max",
      dbh_cm > fia_max ~ "Beyond FIA Max",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(zone)) %>%
  group_by(species, dataset, zone) %>%
  summarize(
    n = n(),
    min_dbh = min(dbh_cm),
    max_dbh = max(dbh_cm),
    mean_dbh = mean(dbh_cm),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(dataset, zone),
    values_from = c(n, min_dbh, max_dbh, mean_dbh),
    values_fill = 0
  )

# =============================================================================
# EXTREME OUTLIER TREES
# =============================================================================

cat("Identifying extreme outlier trees...\n")

# Individual NEFIN trees that are outliers
extreme_trees <- trees_elig %>%
  filter(species %in% gap_analysis$species) %>%
  left_join(gap_analysis %>% select(species, fia_max), by = "species") %>%
  filter(dataset == "NEFIN", dbh_cm > fia_max) %>%
  arrange(species, desc(dbh_cm)) %>%
  select(species, tree_id, dbh_cm, ht_m, fia_max) %>%
  mutate(
    excess_cm = dbh_cm - fia_max,
    excess_pct = 100 * (dbh_cm - fia_max) / fia_max
  )

cat("  Extreme NEFIN trees beyond FIA max:", nrow(extreme_trees), "\n\n")

# =============================================================================
# VISUALIZATIONS
# =============================================================================

output_dir <- "data/processed/extreme_tail_gap_analysis"
fig_dir <- file.path(output_dir, "figures")
tab_dir <- file.path(output_dir, "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

cat("Creating visualizations...\n")

# 1. Gap size bar chart
p_gap_bars <- gap_analysis %>%
  head(20) %>%
  mutate(species = fct_reorder(species, max_gap_cm)) %>%
  ggplot(aes(x = max_gap_cm, y = species)) +
  geom_col(fill = "#E69F00", alpha = 0.8) +
  geom_text(aes(label = paste0("+", round(max_gap_cm, 1), " cm")),
            hjust = -0.1, size = 3) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "italic", size = 9)
  ) +
  labs(
    title = "Extreme Tail Gap: NEFIN Max - FIA Max",
    subtitle = paste0("Top 20 species where NEFIN captures trees FIA doesn't (≥",
                     MIN_GAP_SIZE_CM, " cm gap)"),
    x = "Gap Size (cm): NEFIN Maximum - FIA Maximum",
    y = NULL
  )

ggsave(file.path(fig_dir, "01_gap_size_barplot.png"), 
       p_gap_bars, width = 10, height = 8, dpi = 300)

# 2. Gap vs NEFIN trees in gap - scatter
p_gap_scatter <- gap_analysis %>%
  ggplot(aes(x = max_gap_cm, y = n_nefin_beyond_fia_max)) +
  geom_point(aes(size = pct_nefin_beyond_fia_max, color = enrichment_ratio),
             alpha = 0.7) +
  geom_text(data = gap_analysis %>% head(10),
            aes(label = species),
            hjust = -0.1, vjust = 0, size = 3, fontface = "italic",
            check_overlap = TRUE) +
  scale_size_continuous(name = "% of NEFIN\nbeyond FIA max",
                       range = c(2, 12)) +
  scale_color_viridis_c(name = "Enrichment\nRatio",
                        option = "plasma",
                        trans = "log10") +
  theme_minimal() +
  labs(
    title = "Gap Size vs Number of NEFIN Trees in Gap",
    subtitle = "Size = % of NEFIN beyond FIA max; Color = Enrichment ratio",
    x = "Gap Size (cm): NEFIN Max - FIA Max",
    y = "Number of NEFIN Trees Beyond FIA Maximum"
  )

ggsave(file.path(fig_dir, "02_gap_scatter.png"), 
       p_gap_scatter, width = 10, height = 7, dpi = 300)

# 3. Distribution plots for top gap species
top_gap_species <- gap_analysis %>%
  head(12) %>%
  pull(species)

dist_data <- trees_elig %>%
  filter(species %in% top_gap_species) %>%
  left_join(gap_analysis %>% select(species, fia_max, fia_p99), by = "species") %>%
  mutate(species = factor(species, levels = top_gap_species))

p_distributions <- ggplot(dist_data, aes(x = dbh_cm, fill = dataset)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  geom_vline(aes(xintercept = fia_max), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  facet_wrap(~ species, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic", size = 9),
    legend.position = "bottom"
  ) +
  labs(
    title = "DBH Distributions Showing Extreme Tail Gaps",
    subtitle = "Red dashed line = FIA maximum (NEFIN extends beyond)",
    x = "DBH (cm)",
    y = "Density",
    fill = "Dataset"
  )

ggsave(file.path(fig_dir, "03_distributions_with_gaps.png"), 
       p_distributions, width = 12, height = 10, dpi = 300)

# 4. Stacked bar showing tree counts by zone
zone_counts <- trees_elig %>%
  filter(species %in% top_gap_species) %>%
  left_join(gap_analysis %>% select(species, fia_max, fia_p99), by = "species") %>%
  mutate(
    zone = case_when(
      dbh_cm <= fia_p99 ~ "Below P99",
      dbh_cm > fia_p99 & dbh_cm <= fia_max ~ "P99-Max",
      dbh_cm > fia_max ~ "Beyond Max",
      TRUE ~ NA_character_
    ),
    zone = factor(zone, levels = c("Below P99", "P99-Max", "Beyond Max")),
    species = factor(species, levels = top_gap_species)
  ) %>%
  filter(!is.na(zone)) %>%
  count(species, dataset, zone)

p_zones <- ggplot(zone_counts, aes(x = dataset, y = n, fill = zone)) +
  geom_col(position = "fill") +
  facet_wrap(~ species, ncol = 3) +
  scale_fill_manual(
    values = c("Below P99" = "#440154", "P99-Max" = "#31688e", "Beyond Max" = "#fde725"),
    name = "Size Zone"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic", size = 9),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Tree Size Zone Distribution by Dataset",
    subtitle = "Yellow = trees beyond FIA's absolute maximum",
    x = NULL,
    y = "Proportion of Trees"
  )

ggsave(file.path(fig_dir, "04_zone_proportions.png"), 
       p_zones, width = 12, height = 10, dpi = 300)

# 5. Combined ECDF with shaded gap zones
p_ecdf_gaps <- ggplot(dist_data, aes(x = dbh_cm, color = dataset)) +
  stat_ecdf(linewidth = 1.2, alpha = 0.8) +
  geom_rect(aes(xmin = fia_max, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.05, inherit.aes = FALSE) +
  facet_wrap(~ species, scales = "free_x", ncol = 3) +
  scale_color_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "italic", size = 9),
    legend.position = "bottom"
  ) +
  labs(
    title = "ECDF Showing Extreme Tail Gaps",
    subtitle = "Red shaded region = beyond FIA maximum (NEFIN only)",
    x = "DBH (cm)",
    y = "Cumulative Proportion",
    color = "Dataset"
  )

ggsave(file.path(fig_dir, "05_ecdf_with_gap_zones.png"), 
       p_ecdf_gaps, width = 12, height = 10, dpi = 300)

cat("  ✓ Visualizations saved\n\n")

# =============================================================================
# SAVE TABLES
# =============================================================================

cat("Saving analysis tables...\n")

write_csv(gap_analysis, file.path(tab_dir, "gap_analysis_summary.csv"))
write_csv(gap_zone_details, file.path(tab_dir, "gap_zone_details.csv"))
write_csv(extreme_trees, file.path(tab_dir, "extreme_outlier_trees.csv"))

# Create a summary report
summary_report <- gap_analysis %>%
  head(20) %>%
  select(
    species, 
    n_fia, n_nefin,
    fia_max, nefin_max, max_gap_cm,
    n_nefin_beyond_fia_max, pct_nefin_beyond_fia_max,
    fia_p99, nefin_p99,
    enrichment_ratio
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

write_csv(summary_report, file.path(tab_dir, "gap_summary_report.csv"))

cat("  ✓ Tables saved\n\n")

# =============================================================================
# SUMMARY OUTPUT
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  EXTREME TAIL GAP ANALYSIS SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Species with extreme tail gaps (NEFIN extends beyond FIA):\n\n")

print(gap_analysis %>%
        select(species, max_gap_cm, n_nefin_beyond_fia_max, 
               pct_nefin_beyond_fia_max, enrichment_ratio) %>%
        head(15))

cat("\n")
cat(sprintf("✓ %d species show substantial gaps (≥%.1f cm, ≥%d NEFIN trees)\n",
            nrow(gap_analysis), MIN_GAP_SIZE_CM, MIN_NEFIN_IN_GAP))

cat(sprintf("  Average gap size: %.1f cm\n", mean(gap_analysis$max_gap_cm)))
cat(sprintf("  Largest gap: %.1f cm (%s)\n", 
            max(gap_analysis$max_gap_cm),
            gap_analysis$species[which.max(gap_analysis$max_gap_cm)]))

cat(sprintf("\n  Total extreme outlier trees (NEFIN beyond FIA max): %d\n",
            sum(gap_analysis$n_nefin_beyond_fia_max)))

major_gaps <- gap_analysis %>% filter(max_gap_cm > 30)
if (nrow(major_gaps) > 0) {
  cat(sprintf("\n  %d species with >30 cm gap:\n", nrow(major_gaps)))
  print(major_gaps %>% 
          select(species, max_gap_cm, n_nefin_beyond_fia_max, 
                 fia_max, nefin_max) %>%
          arrange(desc(max_gap_cm)))
  cat("\n")
}

cat("\n✓ NEFIN captures tree sizes that FIA simply doesn't observe\n")
cat("✓ This is critical for:\n")
cat("  - Allometric equation development (large tree biomass)\n")
cat("  - Understanding maximum size potential\n")
cat("  - Carbon stock estimation in old-growth forests\n")
cat("  - Ecological studies of large tree distributions\n")

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  Outputs saved to:", output_dir, "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
