# =============================================================================
# 03_compare_species_dbh_ecdf.R - USING FHM MAPPING
# =============================================================================
# Purpose:
#   Compare FIA vs NEFIN DBH distributions BY SPECIES using ECDF curves
#   Strong visualization of tail support differences
#
# Approach:
#   1. Load FHM species mapping (SPCD → Latin names)
#   2. Join FIA via SPCD
#   3. Join NEFIN via Latin name (already in treeSpecies)
#   4. Compare on shared species using ECDF plots
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

MIN_N_PER_DATASET <- 150
TOP_K_SPECIES <- 12
XMAX_DBH <- 120
TAIL_MIN_DBH <- 30
LARGE_DBH_CM <- 50

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
  filter(!is.na(latin_name)) %>%
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

# NEFIN already has Latin names
nefin <- nefin_raw %>%
  select(treeSpecies, DBH) %>%
  filter(!is.na(treeSpecies), !is.na(DBH), DBH > 0) %>%
  left_join(fhm_mapping, by = c("treeSpecies" = "latin_name")) %>%
  filter(!is.na(common_name)) %>%
  mutate(
    dataset = "NEFIN",
    latin_name = treeSpecies,
    dbh_cm = DBH
  ) %>%
  select(dataset, latin_name, common_name, dbh_cm)

message("  ✓ NEFIN: ", format(nrow(nefin), big.mark = ","), " trees")
message("    Unique species: ", n_distinct(nefin$common_name))

# =============================================================================
# COMBINE AND SELECT TOP SHARED SPECIES
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

if (nrow(species_counts) == 0) {
  stop("No shared species meet MIN_N_PER_DATASET = ", MIN_N_PER_DATASET)
}

message("\n✓ Species with ≥", MIN_N_PER_DATASET, " trees in BOTH datasets: ", 
        nrow(species_counts))

message("\nTop 10 shared species:")
print(species_counts %>% select(common_name, FIA, NEFIN, n_total) %>% head(10), n = 10)

top_species <- species_counts %>%
  slice_head(n = TOP_K_SPECIES) %>%
  pull(common_name)

message("\nUsing top ", length(top_species), " species for plots\n")

# Filter and order
plot_df <- tree_species %>%
  filter(common_name %in% top_species) %>%
  mutate(common_name = factor(common_name, levels = top_species))

# =============================================================================
# FULL RANGE ECDF PLOT
# =============================================================================

p_ecdf <- ggplot(plot_df, aes(x = dbh_cm, color = dataset)) +
  stat_ecdf(linewidth = 1.2, alpha = 0.8) +
  coord_cartesian(xlim = c(0, XMAX_DBH)) +
  facet_wrap(~ common_name, scales = "fixed", ncol = 4) +
  scale_color_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = paste0("DBH ECDF by Species (Top ", length(top_species), 
                   " shared species)"),
    subtitle = paste0("Both datasets have ≥", MIN_N_PER_DATASET, 
                      " trees per species"),
    x = "DBH (cm)",
    y = "Cumulative Proportion",
    color = "Dataset"
  )

out_file <- file.path(fig_dir, "species_dbh_ecdf_top.png")
ggsave(out_file, p_ecdf, width = 13, height = 8, dpi = 200)
message("✓ Saved: ", out_file)

# =============================================================================
# TAIL ZOOM ECDF PLOT
# =============================================================================

p_ecdf_tail <- ggplot(plot_df %>% filter(dbh_cm >= TAIL_MIN_DBH),
                      aes(x = dbh_cm, color = dataset)) +
  stat_ecdf(linewidth = 1.2, alpha = 0.8) +
  coord_cartesian(xlim = c(TAIL_MIN_DBH, XMAX_DBH)) +
  facet_wrap(~ common_name, scales = "fixed", ncol = 4) +
  scale_color_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = paste0("DBH Tail ECDF by Species (DBH ≥ ", TAIL_MIN_DBH, " cm)"),
    subtitle = "Zoomed view showing large tree differences",
    x = "DBH (cm)",
    y = "Cumulative Proportion",
    color = "Dataset"
  )

out_file_tail <- file.path(fig_dir, "species_dbh_ecdf_tail_top.png")
ggsave(out_file_tail, p_ecdf_tail, width = 13, height = 8, dpi = 200)
message("✓ Saved: ", out_file_tail)

# =============================================================================
# TAIL ENRICHMENT TABLE
# =============================================================================

species_tail <- plot_df %>%
  group_by(common_name) %>%
  summarise(
    # FIA quantiles
    fia_q50 = quantile(dbh_cm[dataset == "FIA"], 0.50, na.rm = TRUE),
    fia_q95 = quantile(dbh_cm[dataset == "FIA"], 0.95, na.rm = TRUE),
    fia_q99 = quantile(dbh_cm[dataset == "FIA"], 0.99, na.rm = TRUE),
    fia_max = max(dbh_cm[dataset == "FIA"], na.rm = TRUE),
    
    # NEFIN quantiles
    nefin_q50 = quantile(dbh_cm[dataset == "NEFIN"], 0.50, na.rm = TRUE),
    nefin_q95 = quantile(dbh_cm[dataset == "NEFIN"], 0.95, na.rm = TRUE),
    nefin_q99 = quantile(dbh_cm[dataset == "NEFIN"], 0.99, na.rm = TRUE),
    nefin_max = max(dbh_cm[dataset == "NEFIN"], na.rm = TRUE),
    
    # Tail enrichment
    pct_nefin_over_fia_q95 = 100 * mean(dbh_cm[dataset == "NEFIN"] > fia_q95, 
                                        na.rm = TRUE),
    pct_nefin_over_fia_q99 = 100 * mean(dbh_cm[dataset == "NEFIN"] > fia_q99, 
                                        na.rm = TRUE),
    
    # Large tree percentages
    pct_fia_over_50 = 100 * mean(dbh_cm[dataset == "FIA"] > LARGE_DBH_CM, 
                                 na.rm = TRUE),
    pct_nefin_over_50 = 100 * mean(dbh_cm[dataset == "NEFIN"] > LARGE_DBH_CM, 
                                   na.rm = TRUE),
    
    # Enrichment ratio
    enrichment_ratio_50cm = pct_nefin_over_50 / pmax(pct_fia_over_50, 0.1),
    
    # Sample sizes
    n_fia = sum(dataset == "FIA"),
    n_nefin = sum(dataset == "NEFIN"),
    
    .groups = "drop"
  ) %>%
  arrange(desc(pct_nefin_over_fia_q95))

out_table <- file.path(tab_dir, "species_tail_enrichment_ecdf.csv")
write_csv(species_tail, out_table)
message("✓ Saved: ", out_table)

# =============================================================================
# SUMMARY OUTPUT
# =============================================================================

message("\n=== TAIL ENRICHMENT SUMMARY ===")
message("Species ranked by NEFIN enrichment beyond FIA 95th percentile:\n")

print(species_tail %>%
        select(common_name, pct_nefin_over_fia_q95, enrichment_ratio_50cm, 
               fia_q95, nefin_q95, nefin_max) %>%
        mutate(across(where(is.numeric), ~round(., 1))) %>%
        head(10),
      n = 10)

message("\n✓ DONE! Outputs in: ", out_dir)
message("\nInterpretation:")
message("- ECDF curves show cumulative distribution")
message("- NEFIN curve to the right = more large trees")
message("- Tail zoom highlights differences in large trees (≥", TAIL_MIN_DBH, "cm)")
message("- Enrichment ratio >1 means NEFIN has proportionally more large trees")