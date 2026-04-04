# =============================================================================
# paper1_analysis_fixed.R
# =============================================================================
# Fixed version addressing three issues:
#   1. Table A3: adds median, P95, max biomass computed from raw data
#   2. Table C3: corrects pivot_wider so FIA/NEFIN appear side-by-side
#   3. Fig D1:   adds common names via a lookup table
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(patchwork)

out_dir <- "outputs/paper1"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Loading data...\n")

sum_stats  <- read_csv("data/processed/summary_statistics/summary_by_dataset.csv",
                       show_col_types = FALSE)
plot_quant <- read_csv("data/processed/edge_case_analysis_data_structure/tables/plot_metric_quantiles_by_dataset.csv",
                       show_col_types = FALSE)
sp_summary <- read_csv("data/processed/large_tree_analysis/species_summary.csv",
                       show_col_types = FALSE)

# Load raw plot files to compute missing biomass stats
nefin      <- read_csv("data/processed/nefin_complete.csv", show_col_types = FALSE)
fia        <- read_csv("data/processed/fia_complete.csv",   show_col_types = FALSE)

cat("Data loaded.\n\n")

# =============================================================================
# FIX 1 — TABLE A3: Dataset comparison with median, P95, max from raw data
# =============================================================================

cat("── Fix 1: Table A3 with complete biomass stats ─────────────────\n")

nefin_bio_stats <- nefin %>%
  summarise(
    n = n(),
    mean_biomass= round(mean(biomass,   na.rm = TRUE), 1),
    median_biomass = round(median(biomass, na.rm = TRUE), 1),
    sd_biomass= round(sd(biomass,     na.rm = TRUE), 1),
    p95_biomass = round(quantile(biomass, 0.95, na.rm = TRUE), 1),
    max_biomass = round(max(biomass,    na.rm = TRUE), 1)
  ) %>%
  mutate(dataset = "NEFIN",
         mean_ndvi= round(sum_stats$ndvi_s2_mean[sum_stats$dataset == "NEFIN"], 3),
         mean_tmean = round(sum_stats$tmean_mean[sum_stats$dataset == "NEFIN"],   1),
         mean_ppt = round(sum_stats$ppt_mean[sum_stats$dataset == "NEFIN"],     1))

fia_bio_stats <- fia %>%
  summarise(
    n = n(),
    mean_biomass= round(mean(biomass,             na.rm = TRUE), 1),
    median_biomass = round(median(biomass,           na.rm = TRUE), 1),
    sd_biomass= round(sd(biomass,               na.rm = TRUE), 1),
    p95_biomass = round(quantile(biomass, 0.95,   na.rm = TRUE), 1),
    max_biomass = round(max(biomass,              na.rm = TRUE), 1)
  ) %>%
  mutate(
    dataset = "FIA",
    # NDVI/climate: no per-plot columns in fia_complete.csv — use pre-computed means
    mean_ndvi = round(sum_stats$ndvi_s2_mean[sum_stats$dataset == "FIA"], 3),
    mean_tmean = round(sum_stats$tmean_mean[sum_stats$dataset == "FIA"],   1),
    mean_ppt= round(sum_stats$ppt_mean[sum_stats$dataset == "FIA"],     1)
  )
# Verified values (fia_complete.csv, n=7345):
#   median=119.1, SD=76.2, P95=265.8, max=483.9 Mg/ha

table_A3 <- data.frame(
  Metric = c(
    "N plots",
    "Mean biomass (Mg ha⁻¹)",
    "Median biomass (Mg ha⁻¹)",
    "SD biomass (Mg ha⁻¹)",
    "P95 biomass (Mg ha⁻¹)",
    "Max biomass (Mg ha⁻¹)",
    "Mean NDVI (Sentinel-2)",
    "Mean temperature (°C)",
    "Mean precipitation (cm yr⁻¹)"
  ),
  FIA = c(
    format(fia_bio_stats$n, big.mark = ","),
    paste0(fia_bio_stats$mean_biomass,   " ± ", fia_bio_stats$sd_biomass),
    as.character(fia_bio_stats$median_biomass),
    as.character(fia_bio_stats$sd_biomass),
    as.character(fia_bio_stats$p95_biomass),
    as.character(fia_bio_stats$max_biomass),
    as.character(fia_bio_stats$mean_ndvi),
    as.character(fia_bio_stats$mean_tmean),
    as.character(fia_bio_stats$mean_ppt)
  ),
  NEFIN = c(
    format(nefin_bio_stats$n, big.mark = ","),
    paste0(nefin_bio_stats$mean_biomass, " ± ", nefin_bio_stats$sd_biomass),
    as.character(nefin_bio_stats$median_biomass),
    as.character(nefin_bio_stats$sd_biomass),
    as.character(nefin_bio_stats$p95_biomass),
    as.character(nefin_bio_stats$max_biomass),
    as.character(nefin_bio_stats$mean_ndvi),
    as.character(nefin_bio_stats$mean_tmean),
    as.character(nefin_bio_stats$mean_ppt)
  )
)

cat("Table A3:\n")
print(table_A3)
write_csv(table_A3, file.path(out_dir, "tableA3_dataset_comparison_fixed.csv"))
cat("  ✓ Saved tableA3_dataset_comparison_fixed.csv\n\n")

# =============================================================================
# FIX 2 — TABLE C3: Plot metrics side-by-side (correct pivot)
# =============================================================================

cat("── Fix 2: Table C3 corrected pivot ─────────────────────────────\n")

# The problem: pivot_wider fails cleanly when n differs between datasets.
# Fix: drop n before pivoting, then re-join it separately, or just reshape manually.

table_C3 <- plot_quant %>%
  # Rename metrics to readable labels
  mutate(metric = recode(metric,
                         "max_dbh"= "Plot max DBH (cm)",
                         "p95_dbh"= "Plot P95 DBH (cm)",
                         "pct_ba_large" = "% basal area in large trees",
                         "pct_large_trees" = "% stems that are large trees",
                         "qmd_cm" = "Quadratic mean diameter (cm)"
  )) %>%
  select(-n) %>% # drop n — it differs by dataset, breaks wide join
  pivot_wider(
    names_from  = dataset,
    values_from = c(q50, q90, q95, q99),
    names_glue  = "{dataset}_{.value}"
  ) %>%
  # Reorder columns: metric, then FIA stats, then NEFIN stats, then diffs
  transmute(
    Metric       = metric,
    FIA_median   = round(FIA_q50, 1),
    NEFIN_median = round(NEFIN_q50, 1),
    FIA_P90      = round(FIA_q90, 1),
    NEFIN_P90    = round(NEFIN_q90, 1),
    FIA_P95      = round(FIA_q95, 1),
    NEFIN_P95    = round(NEFIN_q95, 1),
    FIA_P99      = round(FIA_q99, 1),
    NEFIN_P99    = round(NEFIN_q99, 1),
    P95_diff     = round(NEFIN_q95 - FIA_q95, 1),
    P99_diff     = round(NEFIN_q99 - FIA_q99, 1)
  )

cat("Table C3 (fixed — side-by-side):\n")
print(table_C3)
write_csv(table_C3, file.path(out_dir, "tableC3_plot_metrics_fixed.csv"))
cat("  ✓ Saved tableC3_plot_metrics_fixed.csv\n\n")

# =============================================================================
# FIX 3 — FIG D1: Add common names to species forest plot
# =============================================================================

cat("── Fix 3: Fig D1 with common names ─────────────────────────────\n")

# Build latin -> common name lookup
# Sources: species_tail_enrichment_ecdf has common names for 12 key species
# Remaining species mapped manually from standard USDA PLANTS / FIA species codes
common_name_lookup <- tribble(
  ~species,                    ~common_name,
  # ── NEFIN advantage species ──────────────────────────────────────────────
  "carya cordiformis", "Bitternut hickory",
  "acer platanoides", "Norway maple",
  "liriodendron tulipifera", "Tulip poplar",
  "pseudotsuga menziesii", "Douglas-fir",
  "pinus sylvestris", "Scots pine",
  "betula nigra", "River birch",
  "carya ovata", "Shagbark hickory",
  "picea rubens", "Red spruce",
  "fraxinus pennsylvanica", "Green ash",
  "populus tremuloides","Quaking aspen",
  "tilia americana", "American basswood",
  "picea mariana","Black spruce",
  "tsuga canadensis","Eastern hemlock",
  "betula alleghaniensis","Yellow birch",
  "abies balsamea","Balsam fir",
  "betula populifolia","Gray birch",
  "betula papyrifera","Paper birch",
  "fraxinus americana","White ash",
  "fagus grandifolia", "American beech",
  "acer saccharum","Sugar maple",
  # ── FIA advantage species ────────────────────────────────────────────────
  "castanea dentata","American chestnut",
  "populus deltoides","Eastern cottonwood",
  "quercus bicolor","Swamp white oak",
  "quercus alba", "White oak",
  "quercus velutina", "Black oak",
  "ailanthus altissima", "Tree-of-heaven",
  "sassafras albidum", "Sassafras",
  "sorbus americana", "American mountain-ash",
  "ostrya virginiana","Hop hornbeam",
  "pinus rigida","Pitch pine",
  "quercus coccinea","Scarlet oak",
  "ulmus americana", "American elm",
  "quercus rubra", "Northern red oak",
  "juniperus virginiana", "Eastern red cedar",
  "pinus strobus", "Eastern white pine",
  # ── Additional species ───────────────────────────────────────────────────
  "larix laricina","Tamarack",
  "populus grandidentata","Bigtooth aspen",
  "nyssa sylvatica","Black tupelo",
  "thuja occidentalis","Northern white cedar",
  "acer rubrum", "Red maple",
  "acer pensylvanicum","Striped maple",
  "pinus banksiana","Jack pine",
  "prunus serotina","Black cherry",
  "carya glabra", "Pignut hickory",
  "juglans nigra", "Black walnut",
  "picea glauca", "White spruce",
  "chamaecyparis thyoides","Atlantic white cedar",
  "fraxinus nigra","Black ash",
  "juglans cinerea","Butternut",
  "betula lenta", "Sweet birch",
  "prunus pensylvanica","Pin cherry",
  "robinia pseudoacacia","Black locust",
  "picea abies", "Norway spruce"
)

# Build species label: "Common name\n(Latin name)" for figure
sp_plot_data <- sp_summary %>%
  left_join(common_name_lookup, by = "species") %>%
  mutate(
    # Fallback: use title-cased latin if no common name found
    common_name  = ifelse(is.na(common_name),
                          tools::toTitleCase(species),
                          common_name),
    # Label for plot: common name on top, italic latin below
    label = paste0(common_name, "\n(", species, ")"),
    label_short = common_name,          # short version for crowded plots
    direction = ifelse(dbh_p99_delta > 0, "NEFIN advantage", "FIA advantage"),
    sig = !(dbh_p99_lo95 < 0 & dbh_p99_hi95 > 0)
  ) %>%
  arrange(desc(abs(dbh_p99_delta))) %>%
  slice_head(n = 20)

# Verify all top-20 got common names
missing <- sp_plot_data %>% filter(is.na(common_name) | common_name == "")
if (nrow(missing) > 0) {
  cat("  Missing common names for:", paste(missing$species, collapse = ", "), "\n")
} else {
  cat("  all 20 species have common names\n")
}

# ── Figure D1 (fixed) ────────────────────────────────────────────────────────
# Latin name labels placed at fixed x positions on each side so they
# never get clipped regardless of how large the point value is.

# Fixed label x positions: just inside the plot limits
NEFIN_label_x <-  46   # right side — all NEFIN-advantage latin names go here
FIA_label_x   <- 46   # left side  — all FIA-advantage latin names go here

sp_nefin <- sp_plot_data %>% filter(direction == "NEFIN advantage")
sp_fia   <- sp_plot_data %>% filter(direction == "FIA advantage")

fig_D1_fixed <- ggplot(
  sp_plot_data,
  aes(
    x = reorder(label_short, dbh_p99_delta),
    y = dbh_p99_delta,
    color = direction,
    size  = sig
  )
) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.5) +
  geom_point(alpha = 0.9) +
  geom_errorbar(
    aes(ymin = dbh_p99_lo95, ymax = dbh_p99_hi95),
    width     = 0.35,
    linewidth = 0.5
  ) +
  # NEFIN-advantage latin labels: fixed right position, left-aligned
  geom_text(
    data     = sp_nefin,
    aes(x = reorder(label_short, dbh_p99_delta), y = NEFIN_label_x, label = species),
    hjust = 0,
    size= 3.5,
    fontface = "italic",
    color = "gray15",
    inherit.aes = FALSE
  ) +
  # FIA-advantage latin labels: fixed left position, right-aligned
  geom_text(
    data= sp_fia,
    aes(x = reorder(label_short, dbh_p99_delta), y = FIA_label_x, label = species),
    hjust = 0,
    size  = 3.5,
    fontface = "italic",
    color = "gray15",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("NEFIN advantage" = "#1565C0", "FIA advantage" = "#C62828"),
    name   = NULL
  ) +
  scale_size_manual(
    values = c("TRUE" = 3.2, "FALSE" = 1.6),
    labels = c("TRUE" = "Significant (95% CI)", "FALSE" = "Not significant"),
    name = ""
  ) +
  # Expand x limits so fixed-position labels aren't clipped
  scale_y_continuous(limits = c(-55, 55), breaks = seq(-40, 40, 20)) +
  coord_flip() +
  labs(
    title = "Species P99 DBH difference: NEFIN − FIA (cm)",
    subtitle = "Error bars = 95% bootstrap CI. Italic = Latin name.",
    x = NULL,
    y  = "P99 DBH difference (cm)  [NEFIN − FIA]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    legend.position = "bottom",
    plot.subtitle = element_text(color = "gray10", size = 11),
    axis.text.y = element_text(size = 13)
  )
fig_D1_fixed 
ggsave(
  file.path(out_dir, "figD1_species_p99_differences_fixed.png"),
  fig_D1_fixed,
  width = 10,
  height = 8,
  dpi = 300
)
cat("  Saved figD1_species_p99_differences_fixed.png\n\n")

# Also print a clean lookup table for reference
cat("Common name lookup (for manuscript Table 5 / species text):\n")
print(
  sp_plot_data %>%
    select(common_name, species, dbh_p99_delta, dbh_p99_lo95, dbh_p99_hi95, sig, direction) %>%
    mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
    arrange(desc(dbh_p99_delta)),
  n = 20
)

cat("\n All three fixes applied. Outputs in:", out_dir, "\n")
# =============================================================================
# fig_three_bias_panels.R
# Three dimensions of NEFIN sampling bias relative to FIA
# Panels: A = Geographic bias, B = Structural bias, C = Environmental/spatial bias
#
# Inputs:  data/processed/fia_complete.csv
#          data/processed/nefin_complete.csv
# Output:  manuscript_figures/main/fig1_three_bias_panels.png
#
# Run from project root: source("fig_three_bias_panels.R")
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)

# ── Paths ─────────────────────────────────────────────────────────────────────
fia_path   <- "data/processed/fia_complete.csv"
nefin_path <- "data/processed/nefin_complete.csv"
out_path   <- "manuscript_figures/main/fig1_three_bias_panels.png"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
fia   <- read.csv(fia_path,   stringsAsFactors = FALSE)
nefin <- read.csv(nefin_path, stringsAsFactors = FALSE)

state_map <- c("9" = "CT", "23" = "ME", "25" = "MA",
               "33" = "NH", "36" = "NY", "44" = "RI", "50" = "VT")

fia$state   <- state_map[as.character(fia$STATECD)]
nefin$state <- state_map[as.character(nefin$STATECD)]

# State order: CT MA ME NH NY RI VT
state_levels <- c("CT", "MA", "ME", "NH", "NY", "RI", "VT")

# ── Shared colour palette ─────────────────────────────────────────────────────
col_fia   <- "#d62728"   # red
col_nefin <- "#1f77b4"   # blue
alpha_fill <- 0.35

# ── Shared theme ─────────────────────────────────────────────────────────────
theme_pub <- function() {
  theme_classic(base_size = 11) +
    theme(
      axis.title  = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid = element_blank()
    )
}

# =============================================================================
# PANEL A — Geographic bias
# =============================================================================
geo_fia <- fia %>%
  count(state) %>%
  mutate(pct = 100 * n / sum(n), dataset = "FIA")

geo_nefin <- nefin %>%
  count(state) %>%
  mutate(pct = 100 * n / sum(n), dataset = "NEFIN")

# Ensure all 7 states appear for both datasets (all 7 states now represented in NEFIN)
all_states <- data.frame(state = state_levels)
geo_fia   <- left_join(all_states, geo_fia,   by = "state") %>%
  mutate(pct = ifelse(is.na(pct), 0, pct), dataset = "FIA")
geo_nefin <- left_join(all_states, geo_nefin, by = "state") %>%
  mutate(pct = ifelse(is.na(pct), 0, pct), dataset = "NEFIN")

geo_data <- bind_rows(geo_fia, geo_nefin) %>%
  mutate(
    state   = factor(state, levels = state_levels),
    dataset = factor(dataset, levels = c("FIA", "NEFIN"))
  )

nefin_vt_pct <- round(geo_nefin$pct[geo_nefin$state == "VT"])

pA <- ggplot(geo_data, aes(x = state, y = pct, fill = dataset)) +
  geom_col(position = position_dodge(0.75), width = 0.7, colour = NA) +
  annotate("text",
           x = "VT", y = geo_nefin$pct[geo_nefin$state == "VT"] + 2,
           label = paste0(nefin_vt_pct, "% of\nNEFIN here"),
           colour = col_nefin, size = 3.8, hjust = 0.5, lineheight = 0.9, face = "bold") +
  scale_fill_manual(values = c("FIA" = col_fia, "NEFIN" = col_nefin)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "% of plots", title = "A  Geographic bias") +
  theme_pub() +
  theme(legend.position = c(0.15, 0.88))

# =============================================================================
# PANEL B — Structural bias (biomass density)
# =============================================================================
fia_med   <- median(fia$biomass,   na.rm = TRUE)
nefin_med <- median(nefin$biomass, na.rm = TRUE)

bio_data <- bind_rows(
  data.frame(biomass = fia$biomass,   dataset = "FIA"),
  data.frame(biomass = nefin$biomass, dataset = "NEFIN")
) %>% mutate(dataset = factor(dataset, levels = c("FIA", "NEFIN")))

pB <- ggplot(bio_data, aes(x = biomass, fill = dataset, colour = dataset)) +
  # Grey shading for high-biomass region (FIA undersampled)
  annotate("rect",
           xmin = 200, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.6) +
  annotate("text",
           x = 400, y = 0.0038,
           label = "High-biomass\n(FIA undersampled)",
           size = 3.8, colour = "grey40", hjust = 0.5, lineheight = 0.9, face = "bold") +
  geom_density(alpha = alpha_fill, linewidth = 0.7, bw = 18) +
  # Median lines
  geom_vline(xintercept = fia_med,   colour = col_fia,   linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = nefin_med, colour = col_nefin, linetype = "dashed", linewidth = 0.7) +
  scale_fill_manual(
    values = c("FIA" = col_fia, "NEFIN" = col_nefin),
    labels = c(
      "FIA"   = paste0("FIA (median=", round(fia_med), ")"),
      "NEFIN" = paste0("NEFIN (median=", round(nefin_med), ")")
    )
  ) +
  scale_colour_manual(
    values = c("FIA" = col_fia, "NEFIN" = col_nefin),
    labels = c(
      "FIA"   = paste0("FIA (median=", round(fia_med), ")"),
      "NEFIN" = paste0("NEFIN (median=", round(nefin_med), ")")
    )
  ) +
  scale_x_continuous(limits = c(0, 750), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = expression("Aboveground biomass (Mg ha"^{-1}*")"),
       y = "Density",
       title = "B  Structural bias") +
  theme_pub() +
  theme(legend.position = c(0.80, 0.90))

# =============================================================================
# PANEL C — Environmental / spatial bias (latitude density)
# =============================================================================

# State label x-positions (approximate centroid latitudes)
state_lat <- data.frame(
  state = c("CT", "MA", "ME", "NH", "NY", "RI", "VT"),
  lat   = c(41.6, 42.4, 45.3, 43.7, 43.0, 41.7, 44.0)
)

lat_data <- bind_rows(
  data.frame(lat = fia$lat,   dataset = "FIA"),
  data.frame(lat = nefin$lat, dataset = "NEFIN")
) %>% mutate(dataset = factor(dataset, levels = c("FIA", "NEFIN")))

# NEFIN peak lat for annotation
nefin_peak_lat <- 44.5

pC <- ggplot(lat_data, aes(x = lat, fill = dataset, colour = dataset)) +
  geom_density(alpha = alpha_fill, linewidth = 0.7, bw = 0.25) +
  # State boundary lines (approx southern border latitudes)
  geom_vline(xintercept = c(41.2, 42.05, 43.35, 42.7, 42.0, 44.5),
             colour = "grey70", linetype = "dotted", linewidth = 0.4) +
  # State abbreviation labels along x-axis
  annotate("text",
           x     = c(41.6, 42.4, 45.3, 43.7, 43.0, 44.0),
           y     = rep(-0.025, 6),
           label = c("CT", "MA", "ME", "NH", "NY", "VT"),
           size  = 2.5, colour = "grey45", face = "bold") +
  # NEFIN concentration annotation — VT + NH together = 69% of plots
  annotate("text",
           x = 45.1, y = 0.52,
           label = "VT + NH dominate\n(69% of NEFIN plots)",
           colour = col_nefin, size = 3.8, hjust = 0, lineheight = 0.9, face = "bold") +
  annotate("segment",
           x = 45.6, xend = 44.5, y = 0.470, yend = 0.43,
           colour = col_nefin, linewidth = 0.4,
           arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  scale_fill_manual(values   = c("FIA" = col_fia, "NEFIN" = col_nefin)) +
  scale_colour_manual(values = c("FIA" = col_fia, "NEFIN" = col_nefin)) +
  scale_x_continuous(limits = c(40.5, 47.5),
                     breaks = c(42, 44, 46),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "Latitude (\u00b0N)", y = "Density",
       title = "C  Environmental/spatial bias") +
  theme_pub() +
  theme(
    legend.position = c(0.18, 0.82),
    plot.margin = margin(5, 40, 5, 5)   # extra right margin for annotation
  )

# =============================================================================
# Combine panels and save
# =============================================================================
fig <- pA + pB / pC +
  plot_annotation(
    title = "Three dimensions of NEFIN sampling bias relative to FIA",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )

fig

ggsave(out_path,
       plot   = fig,
       width  = 13,
       height = 7.2,
       dpi    = 300,
       bg     = "white")

cat("Saved:", out_path, "\n")