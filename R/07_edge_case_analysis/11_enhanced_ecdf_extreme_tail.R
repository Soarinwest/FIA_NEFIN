# =============================================================================
# Enhanced ECDF with Extreme Tail Highlights - FIXED VERSION
# =============================================================================
# Purpose:
#   Enhanced ECDF analysis highlighting extreme tail zones where NEFIN 
#   extends beyond FIA. Fixed to handle all eligible species properly.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# Paths
path_fhm_mapping <- "data/processed/fhm_species_mapping.csv"
path_fia_tree    <- "data/interim/fia/extracted/tree.csv"
path_nefin_tree  <- "data/processed/nefin_growth_check/nefin_trees_clean.csv"

out_dir <- "data/processed/edge_case_analysis_species_structure"
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

MIN_N_PER_DATASET <- 150
TOP_K_SPECIES <- 15  # Increased to show more species
MIN_GAP_CM <- 10

inch_to_cm <- function(x) x * 2.54

cat("\n===================================================================\n")
cat("  ENHANCED ECDF WITH EXTREME TAIL ANALYSIS - FIXED\n")
cat("===================================================================\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading FHM species mapping...\n")
if (!file.exists(path_fhm_mapping)) {
  stop("\nSpecies mapping not found!")
}

fhm_mapping <- read_csv(path_fhm_mapping, show_col_types = FALSE)
cat("  ok Loaded", nrow(fhm_mapping), "species codes\n")

cat("\nLoading FIA data...\n")
fia_raw <- read_csv(path_fia_tree, show_col_types = FALSE)
fia_dbh_col <- if ("DIA" %in% names(fia_raw)) "DIA" else "DBH"

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

cat("  ok FIA:", format(nrow(fia), big.mark = ","), "trees\n")

cat("\nLoading NEFIN data...\n")
nefin_raw <- read_csv(path_nefin_tree, show_col_types = FALSE)

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

cat("  ok NEFIN:", format(nrow(nefin), big.mark = ","), "trees\n")

# =============================================================================
# FIND SHARED SPECIES
# =============================================================================

tree_species <- bind_rows(fia, nefin)

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

cat("\nok Species with >=", MIN_N_PER_DATASET, " trees in BOTH datasets:", 
    nrow(species_counts), "\n\n")

cat("Top 20 species by total count:\n")
print(species_counts %>% select(common_name, FIA, NEFIN, n_total) %>% head(20), n = 20)

# =============================================================================
# COMPUTE GAP METRICS FOR ALL ELIGIBLE SPECIES
# =============================================================================

cat("\n\nComputing gap metrics for all eligible species...\n")

gap_metrics <- tree_species %>%
  filter(common_name %in% species_counts$common_name) %>%
  group_by(common_name) %>%
  summarise(
    n_fia = sum(dataset == "FIA"),
    n_nefin = sum(dataset == "NEFIN"),
    
    fia_max = max(dbh_cm[dataset == "FIA"], na.rm = TRUE),
    nefin_max = max(dbh_cm[dataset == "NEFIN"], na.rm = TRUE),
    gap_cm = nefin_max - fia_max,
    
    fia_p99 = quantile(dbh_cm[dataset == "FIA"], 0.99, na.rm = TRUE),
    fia_p95 = quantile(dbh_cm[dataset == "FIA"], 0.95, na.rm = TRUE),
    nefin_p99 = quantile(dbh_cm[dataset == "NEFIN"], 0.99, na.rm = TRUE),
    nefin_p95 = quantile(dbh_cm[dataset == "NEFIN"], 0.95, na.rm = TRUE),
    
    n_nefin_beyond_fia_max = sum(dbh_cm[dataset == "NEFIN"] > fia_max),
    pct_nefin_beyond_fia_max = 100 * n_nefin_beyond_fia_max / sum(dataset == "NEFIN"),
    
    n_fia_above_p99 = sum(dbh_cm[dataset == "FIA"] > fia_p99),
    n_nefin_above_fia_p99 = sum(dbh_cm[dataset == "NEFIN"] > fia_p99),
    
    .groups = "drop"
  ) %>%
  mutate(
    has_substantial_gap = gap_cm >= MIN_GAP_CM & n_nefin_beyond_fia_max >= 5
  ) %>%
  arrange(desc(gap_cm))

cat("  ok Computed metrics for", nrow(gap_metrics), "species\n")

# Show gap summary
cat("\nGap Summary:\n")
gap_summary <- gap_metrics %>%
  summarise(
    species_with_gaps = sum(has_substantial_gap),
    species_without_gaps = sum(!has_substantial_gap),
    mean_gap = mean(gap_cm),
    max_gap = max(gap_cm),
    total_nefin_beyond = sum(n_nefin_beyond_fia_max)
  )
print(gap_summary)

cat("\nTop 15 species by gap size:\n")
print(gap_metrics %>% 
        select(common_name, gap_cm, fia_max, nefin_max, 
               n_nefin_beyond_fia_max, has_substantial_gap) %>%
        head(15), n = 15)

# =============================================================================
# SELECT SPECIES FOR PLOTTING
# =============================================================================

# Strategy: Show top species by sample size, but highlight those with gaps
top_by_count <- species_counts %>%
  head(TOP_K_SPECIES) %>%
  pull(common_name)

# Get gap info for these species
plot_species_info <- gap_metrics %>%
  filter(common_name %in% top_by_count) %>%
  arrange(desc(has_substantial_gap), desc(gap_cm))

cat("\n\nSpecies selected for plotting (", nrow(plot_species_info), "):\n")
print(plot_species_info %>% 
        select(common_name, gap_cm, has_substantial_gap, n_nefin_beyond_fia_max), 
      n = nrow(plot_species_info))

top_species_ordered <- plot_species_info$common_name

cat("\n  Species with substantial gaps:", sum(plot_species_info$has_substantial_gap), "\n")
cat("  Species without substantial gaps:", sum(!plot_species_info$has_substantial_gap), "\n\n")

# =============================================================================
# PREPARE PLOTTING DATA
# =============================================================================

plot_df <- tree_species %>%
  filter(common_name %in% top_species_ordered) %>%
  left_join(gap_metrics, by = "common_name") %>%
  mutate(
    common_name = factor(common_name, levels = top_species_ordered),
    in_gap_zone = dataset == "NEFIN" & dbh_cm > fia_max
  )

cat("Plotting data prepared:", format(nrow(plot_df), big.mark = ","), "trees\n")

# =============================================================================
# 1. FULL ECDF WITH GAP ZONES HIGHLIGHTED
# =============================================================================

cat("\nCreating Plot 1: Full ECDF with gap zones...\n")

# Annotation data
annotation_data <- gap_metrics %>%
  filter(common_name %in% top_species_ordered, has_substantial_gap) %>%
  mutate(
    label = sprintf("Gap: +%.0f cm", gap_cm),
    x_pos = pmin(fia_max + gap_cm * 0.3, nefin_max - 5),
    y_pos = 0.90
  )

p_ecdf_full <- ggplot(plot_df, aes(x = dbh_cm, color = dataset)) +
  # Shaded gap zone
  geom_rect(
    data = gap_metrics %>% filter(common_name %in% top_species_ordered, has_substantial_gap),
    aes(xmin = fia_max, xmax = nefin_max, ymin = 0, ymax = 1),
    fill = "#fee08b", alpha = 0.25,
    inherit.aes = FALSE
  ) +
  # ECDF lines
  stat_ecdf(linewidth = 1.1, alpha = 0.9) +
  # FIA maximum line
  geom_vline(
    data = gap_metrics %>% filter(common_name %in% top_species_ordered),
    aes(xintercept = fia_max),
    linetype = "dashed", color = "#d73027", linewidth = 0.6
  ) +
  # Gap annotations
  geom_text(
    data = annotation_data,
    aes(x = x_pos, y = y_pos, label = label),
    hjust = 0.5, vjust = 1, size = 2.5,
    color = "black", fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ common_name, scales = "free_x", ncol = 5) +
  scale_color_manual(
    values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9"),
    name = "Dataset"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.3),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 10)
  ) +
  labs(
    title = "DBH ECDF with Extreme Tail Gaps Highlighted",
    subtitle = "Yellow zone = NEFIN extends beyond FIA | Red dashed line = FIA absolute maximum",
    x = "DBH (cm)",
    y = "Cumulative Proportion"
  )

out_file1 <- file.path(fig_dir, "ecdf_full_with_gaps.png")
ggsave(out_file1, p_ecdf_full, width = 16, height = 10, dpi = 300)
cat("  ok Saved:", out_file1, "\n")

# =============================================================================
# 2. SIMPLIFIED ECDF - TOP GAP SPECIES ONLY
# =============================================================================

gap_species_only <- gap_metrics %>%
  filter(has_substantial_gap) %>%
  arrange(desc(gap_cm)) %>%
  head(12) %>%
  pull(common_name)

if (length(gap_species_only) > 0) {
  cat("\nCreating Plot 2: Gap species only (", length(gap_species_only), " species)...\n")
  
  gap_plot_df <- plot_df %>%
    filter(common_name %in% gap_species_only) %>%
    mutate(common_name = factor(common_name, 
                                levels = gap_species_only))
  
  gap_annotation <- gap_metrics %>%
    filter(common_name %in% gap_species_only) %>%
    mutate(
      label = sprintf("+%.0f cm\n%d trees", gap_cm, n_nefin_beyond_fia_max),
      x_pos = fia_max,
      y_pos = 0.80
    )
  
  p_gaps_only <- ggplot(gap_plot_df, aes(x = dbh_cm, color = dataset)) +
    # Highlight gap
    geom_rect(
      data = gap_metrics %>% filter(common_name %in% gap_species_only),
      aes(xmin = fia_max, xmax = nefin_max, ymin = 0, ymax = 1),
      fill = "#d73027", alpha = 0.15,
      inherit.aes = FALSE
    ) +
    # ECDF
    stat_ecdf(linewidth = 1.3) +
    # FIA max
    geom_vline(
      data = gap_metrics %>% filter(common_name %in% gap_species_only),
      aes(xintercept = fia_max),
      linetype = "solid", color = "#d73027", linewidth = 1
    ) +
    # Annotations
    geom_label(
      data = gap_annotation,
      aes(x = x_pos, y = y_pos, label = label),
      hjust = 0, vjust = 1, size = 2.5,
      fill = "white", alpha = 0.8,
      label.padding = unit(0.15, "lines"),
      inherit.aes = FALSE
    ) +
    facet_wrap(~ common_name, scales = "free_x", ncol = 4) +
    scale_color_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 9, face = "bold"),
      panel.border = element_rect(color = "gray50", fill = NA)
    ) +
    labs(
      title = "Extreme Tail Gaps: Species Where NEFIN Extends Beyond FIA",
      subtitle = "Red zone and line = FIA's absolute limit | NEFIN continues beyond",
      x = "DBH (cm)",
      y = "Cumulative Proportion",
      color = "Dataset"
    )
  
  out_file2 <- file.path(fig_dir, "ecdf_gap_species_only.png")
  ggsave(out_file2, p_gaps_only, width = 14, height = 10, dpi = 300)
  cat("  ok Saved:", out_file2, "\n")
}

# =============================================================================
# 3. DISTRIBUTION COMPARISON FOR TOP GAPS
# =============================================================================

top_gaps <- gap_metrics %>%
  filter(has_substantial_gap) %>%
  head(9) %>%
  pull(common_name)

if (length(top_gaps) > 0) {
  cat("\nCreating Plot 3: Distribution comparison (", length(top_gaps), " species)...\n")
  
  dist_df <- plot_df %>%
    filter(common_name %in% top_gaps) %>%
    mutate(common_name = factor(common_name, levels = top_gaps))
  
  p_distributions <- ggplot(dist_df, aes(x = dbh_cm, fill = dataset)) +
    geom_density(alpha = 0.5, adjust = 1.2) +
    # FIA maximum
    geom_vline(
      data = gap_metrics %>% filter(common_name %in% top_gaps),
      aes(xintercept = fia_max),
      linetype = "dashed", color = "red", linewidth = 0.8
    ) +
    # Shade gap zone
    geom_rect(
      data = gap_metrics %>% filter(common_name %in% top_gaps),
      aes(xmin = fia_max, xmax = nefin_max, ymin = -Inf, ymax = Inf),
      fill = "red", alpha = 0.08,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ common_name, scales = "free", ncol = 3) +
    scale_fill_manual(values = c("FIA" = "#E69F00", "NEFIN" = "#56B4E9")) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom"
    ) +
    labs(
      title = "DBH Density: Gaps Where NEFIN Continues Beyond FIA",
      subtitle = "Red zone = tree sizes FIA never observes | Dashed line = FIA maximum",
      x = "DBH (cm)",
      y = "Density",
      fill = "Dataset"
    )
  
  out_file3 <- file.path(fig_dir, "distributions_with_gap_zones.png")
  ggsave(out_file3, p_distributions, width = 13, height = 9, dpi = 300)
  cat("  ok Saved:", out_file3, "\n")
}

# =============================================================================
# 4. DETAILED GAP TABLE
# =============================================================================

cat("\nCreating detailed gap tables...\n")

gap_table_full <- gap_metrics %>%
  arrange(desc(gap_cm)) %>%
  select(
    common_name, n_fia, n_nefin,
    fia_max, nefin_max, gap_cm,
    n_nefin_beyond_fia_max, pct_nefin_beyond_fia_max,
    fia_p99, nefin_p99
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

write_csv(gap_table_full, file.path(tab_dir, "extreme_tail_gap_analysis_all_species.csv"))
cat("  ok Saved: extreme_tail_gap_analysis_all_species.csv\n")

gap_table_substantial <- gap_table_full %>%
  filter(gap_cm >= MIN_GAP_CM, n_nefin_beyond_fia_max >= 5)

write_csv(gap_table_substantial, 
          file.path(tab_dir, "extreme_tail_gap_analysis_substantial_gaps.csv"))
cat("  ok Saved: extreme_tail_gap_analysis_substantial_gaps.csv\n")

# =============================================================================
# SUMMARY OUTPUT
# =============================================================================

cat("\n===================================================================\n")
cat("  EXTREME TAIL GAP ANALYSIS SUMMARY\n")
cat("===================================================================\n\n")

if (nrow(gap_table_substantial) > 0) {
  cat("Species with substantial extreme tail gaps (>=", MIN_GAP_CM, " cm):\n\n")
  
  print(gap_table_substantial %>%
          select(common_name, gap_cm, fia_max, nefin_max, 
                 n_nefin_beyond_fia_max, pct_nefin_beyond_fia_max) %>%
          head(15),
        n = 15)
  
  cat("\n")
  cat(sprintf("ok %d species show NEFIN captures tree sizes FIA doesn't\n",
              nrow(gap_table_substantial)))
  cat(sprintf("  Average gap: %.1f cm\n", mean(gap_table_substantial$gap_cm)))
  cat(sprintf("  Maximum gap: %.1f cm (%s)\n", 
              max(gap_table_substantial$gap_cm),
              gap_table_substantial$common_name[which.max(gap_table_substantial$gap_cm)]))
  cat(sprintf("  Total NEFIN trees in 'unreachable' zones: %s\n",
              format(sum(gap_table_substantial$n_nefin_beyond_fia_max), big.mark = ",")))
  
} else {
  cat("No species show substantial gaps with current threshold (", MIN_GAP_CM, " cm)\n")
}

cat("\n===================================================================\n")
cat("  PLOTS CREATED:\n")
cat("===================================================================\n\n")
cat("1. ecdf_full_with_gaps.png - All", length(top_species_ordered), "species with gaps highlighted\n")
if (length(gap_species_only) > 0) {
  cat("2. ecdf_gap_species_only.png -", length(gap_species_only), "species with substantial gaps\n")
}
if (length(top_gaps) > 0) {
  cat("3. distributions_with_gap_zones.png -", length(top_gaps), "species density plots\n")
}
cat("\nok All outputs saved to:", out_dir, "\n")
cat("\n===================================================================\n\n")
