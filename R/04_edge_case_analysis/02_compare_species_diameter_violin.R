# =============================================================================
# 02_compare_species_diameter_violin.R - USING FHM MAPPING
# =============================================================================
# Purpose:
#   Compare FIA vs NEFIN DBH distributions by species using violin plots
#
# Approach:
#   1. Load FHM species mapping (SPCD → Latin names)
#   2. Join FIA via SPCD
#   3. Join NEFIN via Latin name (already in treeSpecies)
#   4. Compare on shared species
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
})

# Paths
path_fhm_mapping <- "data/processed/fhm_species_mapping.csv"
path_fia_tree    <- "data/interim/fia/extracted/tree.csv"
path_nefin_tree  <- "data/raw/nefin/TREE_RAW_DATA.csv"

out_dir <- "data/processed/edge_case_analysis_species_structure"
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

LARGE_DBH_CM <- 50
MIN_N_PER_DATASET <- 150
TOP_K_SPECIES <- 12

inch_to_cm <- function(x) x * 2.54

# =============================================================================
# LOAD SPECIES MAPPING
# =============================================================================

message("Loading FHM species mapping...")
if (!file.exists(path_fhm_mapping)) {
  stop("\nSpecies mapping not found! Run first:\n",
       "  Rscript extract_fhm_species_mapping.R")
}

fhm_mapping <- read_csv(path_fhm_mapping, show_col_types = FALSE)
message("  ✓ ", nrow(fhm_mapping), " species codes loaded")

# =============================================================================
# PROCESS FIA DATA
# =============================================================================

message("\nLoading: ", path_fia_tree)
fia_raw <- read_csv(path_fia_tree, show_col_types = FALSE)

if (!"SPCD" %in% names(fia_raw)) {
  stop("Cannot find SPCD column in FIA data!")
}
if (!"DIA" %in% names(fia_raw) && !"DBH" %in% names(fia_raw)) {
  stop("Cannot find DIA or DBH column in FIA data!")
}

fia_dbh_col <- if ("DIA" %in% names(fia_raw)) "DIA" else "DBH"

# Join with FHM mapping
fia <- fia_raw %>%
  select(SPCD, dbh_inches = all_of(fia_dbh_col)) %>%
  filter(!is.na(SPCD), !is.na(dbh_inches), dbh_inches > 0) %>%
  left_join(fhm_mapping, by = "SPCD") %>%
  filter(!is.na(latin_name)) %>%  # Remove unmapped species
  mutate(
    dbh_cm = inch_to_cm(dbh_inches),
    dataset = "FIA"
  ) %>%
  select(dataset, latin_name, common_name, dbh_cm)

message("  ✓ FIA: ", format(nrow(fia), big.mark = ","), " trees")
message("    Unique species: ", n_distinct(fia$common_name))

# =============================================================================
# PROCESS NEFIN DATA
# =============================================================================

message("\nLoading: ", path_nefin_tree)
nefin_raw <- read_csv(path_nefin_tree, show_col_types = FALSE)

if (!"treeSpecies" %in% names(nefin_raw)) {
  stop("Cannot find treeSpecies column in NEFIN data!")
}
if (!"DBH" %in% names(nefin_raw)) {
  stop("Cannot find DBH column in NEFIN data!")
}

# NEFIN already has Latin names - just join to get common names
nefin <- nefin_raw %>%
  select(treeSpecies, DBH) %>%
  filter(!is.na(treeSpecies), !is.na(DBH), DBH > 0) %>%
  left_join(fhm_mapping, by = c("treeSpecies" = "latin_name")) %>%
  filter(!is.na(common_name)) %>%  # Keep only species that match FIA
  mutate(
    dataset = "NEFIN",
    latin_name = treeSpecies,
    dbh_cm = DBH
  ) %>%
  select(dataset, latin_name, common_name, dbh_cm)

message("  ✓ NEFIN: ", format(nrow(nefin), big.mark = ","), " trees")
message("    Unique species: ", n_distinct(nefin$common_name))

# =============================================================================
# COMBINE AND FIND SHARED SPECIES
# =============================================================================

tree_species <- bind_rows(fia, nefin)

# Count by species and dataset
species_counts <- tree_species %>%
  group_by(dataset, common_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  filter(FIA >= MIN_N_PER_DATASET, NEFIN >= MIN_N_PER_DATASET) %>%
  mutate(n_total = FIA + NEFIN) %>%
  arrange(desc(n_total))

message("\n✓ Species with ≥", MIN_N_PER_DATASET, " trees in BOTH datasets: ", 
        nrow(species_counts))

if (nrow(species_counts) == 0) {
  stop("No shared species meet threshold! Lower MIN_N_PER_DATASET or check mappings.")
}

# Print top species
message("\nTop 10 shared species:")
print(species_counts %>% select(common_name, FIA, NEFIN, n_total) %>% head(10), n = 10)

# Select top K
shared_top <- species_counts %>%
  slice_head(n = TOP_K_SPECIES) %>%
  pull(common_name)

message("\nUsing top ", length(shared_top), " species for plots\n")

# Filter for plotting
plot_df <- tree_species %>%
  filter(common_name %in% shared_top) %>%
  mutate(common_name = factor(common_name, levels = shared_top))

# =============================================================================
# CREATE VIOLIN PLOT
# =============================================================================

p_violin <- ggplot(plot_df, aes(x = common_name, y = dbh_cm, fill = dataset)) +
  geom_violin(scale = "width", trim = TRUE, alpha = 0.6) +
  geom_boxplot(width = 0.12, outlier.alpha = 0.15, 
               position = position_dodge(width = 0.9)) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_fill_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
    legend.position = "top"
  ) +
  labs(
    title = paste0("DBH Distributions by Species (Top ", length(shared_top), 
                   " shared species)"),
    subtitle = paste0("Both datasets have ≥", MIN_N_PER_DATASET, 
                      " trees per species"),
    x = NULL,
    y = "DBH (cm)",
    fill = "Dataset"
  )

out_plot <- file.path(fig_dir, "species_dbh_violin_top.png")
ggsave(out_plot, p_violin, width = 12, height = 6, dpi = 200)
message("✓ Saved: ", out_plot)

# =============================================================================
# TAIL ENRICHMENT ANALYSIS
# =============================================================================

species_tail <- plot_df %>%
  group_by(common_name) %>%
  summarise(
    # FIA quantiles
    fia_q95 = quantile(dbh_cm[dataset == "FIA"], 0.95, na.rm = TRUE),
    fia_q99 = quantile(dbh_cm[dataset == "FIA"], 0.99, na.rm = TRUE),
    fia_median = median(dbh_cm[dataset == "FIA"], na.rm = TRUE),
    
    # NEFIN quantiles
    nefin_q95 = quantile(dbh_cm[dataset == "NEFIN"], 0.95, na.rm = TRUE),
    nefin_median = median(dbh_cm[dataset == "NEFIN"], na.rm = TRUE),
    
    # Tail enrichment
    pct_nefin_over_fia_q95 = 100 * mean(dbh_cm[dataset == "NEFIN"] > fia_q95, 
                                        na.rm = TRUE),
    pct_nefin_over_fia_q99 = 100 * mean(dbh_cm[dataset == "NEFIN"] > fia_q99, 
                                        na.rm = TRUE),
    
    # Large tree percentages
    pct_nefin_over_50 = 100 * mean(dbh_cm[dataset == "NEFIN"] > LARGE_DBH_CM, 
                                   na.rm = TRUE),
    pct_fia_over_50 = 100 * mean(dbh_cm[dataset == "FIA"] > LARGE_DBH_CM, 
                                 na.rm = TRUE),
    
    # Sample sizes
    n_fia = sum(dataset == "FIA"),
    n_nefin = sum(dataset == "NEFIN"),
    
    .groups = "drop"
  ) %>%
  arrange(desc(pct_nefin_over_fia_q95))

out_table <- file.path(tab_dir, "species_tail_enrichment.csv")
write_csv(species_tail, out_table)
message("✓ Saved: ", out_table)

# =============================================================================
# SUMMARY
# =============================================================================

message("\n=== TAIL ENRICHMENT SUMMARY ===")
message("Species with highest NEFIN enrichment in large trees:\n")
print(species_tail %>%
        select(common_name, pct_nefin_over_fia_q95, pct_nefin_over_50, 
               fia_q95, nefin_q95) %>%
        mutate(across(where(is.numeric), ~round(., 1))) %>%
        head(10), 
      n = 10)

message("\n✓ DONE! Outputs in: ", out_dir)