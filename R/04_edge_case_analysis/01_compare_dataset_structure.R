# =============================================================================
# 01_compare_dataset_structure.R
#
# Purpose:
#   Show FIA vs NEFIN are structurally different:
#     - Tree DBH distributions (ECDF + density)
#     - Tail enrichment: % of NEFIN trees beyond FIA q95/q99
#     - Plot-level structural metrics distributions (max, p95, QMD, % large)
#     - Spatial structure (semivariograms) of plot-level metrics, if coords exist
#
# Inputs:
#   - data/interim/fia/extracted/tree.csv
#   - data/raw/nefin/TREE_RAW_DATA.csv
#   - optional: data/interim/fia/extracted/plot.csv
#   - optional: data/raw/nefin/NEFIN_plots.csv
#   - optional: data/processed/edge_case_analysis/plot_edge_metrics.csv
#
# Outputs:
#   data/processed/edge_case_analysis_data_structure/
#     figures/*.png
#     tables/*.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
})

# Optional for variograms
have_sf <- requireNamespace("sf", quietly = TRUE)
have_gstat <- requireNamespace("gstat", quietly = TRUE)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
path_fia_tree   <- "data/interim/fia/extracted/tree.csv"
path_nefin_tree <- "data/raw/nefin/TREE_RAW_DATA.csv"

path_fia_plot   <- "data/interim/fia/extracted/plot.csv"
path_nefin_plot <- "data/raw/nefin/NEFIN_plots.csv"

# If you already ran your edge metrics module:
path_plot_edge_metrics <- "data/processed/edge_case_analysis/plot_edge_metrics.csv"

out_dir <- "data/processed/edge_case_analysis_data_structure"
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

LARGE_DBH_CM <- 50

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}

inch_to_cm <- function(x) x * 2.54

ba_m2_from_dbh_cm <- function(dbh_cm) pi * ((dbh_cm / 100) / 2)^2

w_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_quantile <- function(x, w, probs = c(0.95)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

w_qmd <- function(dbh_cm, w) {
  ok <- is.finite(dbh_cm) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sqrt(sum(w[ok] * dbh_cm[ok]^2) / sum(w[ok]))
}

# -----------------------------------------------------------------------------
# Load and harmonize tree data
# -----------------------------------------------------------------------------
message("Loading: ", path_fia_tree)
fia_raw <- read_csv(path_fia_tree, show_col_types = FALSE)

message("Loading: ", path_nefin_tree)
nefin_raw <- read_csv(path_nefin_tree, show_col_types = FALSE)

# FIA columns (expected: PLT_CN, DIA inches)
fia_plot_col <- pick_first_existing(fia_raw, c("PLT_CN", "CN", "plot_id", "plotid"))
if (is.null(fia_plot_col)) stop("Cannot find FIA plot id column in tree.csv")

fia_dbh_col <- pick_first_existing(fia_raw, c("DIA", "dbh", "DBH"))
if (is.null(fia_dbh_col)) stop("Cannot find FIA DBH column in tree.csv")

fia_status_col <- pick_first_existing(fia_raw, c("STATUSCD", "status", "treeStatus"))
if (is.null(fia_status_col)) message("Warning: FIA STATUSCD not found (mortality plots may be limited).")

fia_tree <- fia_raw %>%
  transmute(
    dataset = "FIA",
    plot_id = as.character(.data[[fia_plot_col]]),
    dbh_cm = inch_to_cm(as.numeric(.data[[fia_dbh_col]])),
    status = if (!is.null(fia_status_col)) as.integer(.data[[fia_status_col]]) else NA_integer_
  ) %>%
  filter(is.finite(dbh_cm), dbh_cm > 0) %>%
  mutate(is_large = dbh_cm > LARGE_DBH_CM)

# NEFIN columns (expected: _nefin_plotID, DBH cm, treeStatus, _nefin_treeCntPH, _nefin_treeBAPH)
nefin_plot_col <- pick_first_existing(nefin_raw, c("_nefin_plotID", "plot_id", "plotid"))
if (is.null(nefin_plot_col)) stop("Cannot find NEFIN plot id column in TREE_RAW_DATA.csv")

nefin_dbh_col <- pick_first_existing(nefin_raw, c("DBH", "dbh"))
if (is.null(nefin_dbh_col)) stop("Cannot find NEFIN DBH column in TREE_RAW_DATA.csv")

nefin_status_col <- pick_first_existing(nefin_raw, c("treeStatus", "STATUSCD", "status"))
tph_col <- pick_first_existing(nefin_raw, c("_nefin_treeCntPH", "tph", "TPH"))
baph_col <- pick_first_existing(nefin_raw, c("_nefin_treeBAPH", "baph", "BAPH"))

if (is.null(tph_col) || is.null(baph_col)) {
  message("Warning: NEFIN TPH/BAPH columns not found. Weighted plot metrics will be limited.")
}

nefin_tree <- nefin_raw %>%
  transmute(
    dataset = "NEFIN",
    plot_id = as.character(.data[[nefin_plot_col]]),
    dbh_cm = as.numeric(.data[[nefin_dbh_col]]),
    status = if (!is.null(nefin_status_col)) as.integer(.data[[nefin_status_col]]) else NA_integer_,
    tph = if (!is.null(tph_col)) as.numeric(.data[[tph_col]]) else NA_real_,
    baph = if (!is.null(baph_col)) as.numeric(.data[[baph_col]]) else NA_real_
  ) %>%
  filter(is.finite(dbh_cm), dbh_cm > 0) %>%
  mutate(is_large = dbh_cm > LARGE_DBH_CM)

tree_df <- bind_rows(
  fia_tree %>% select(dataset, dbh_cm, is_large),
  nefin_tree %>% select(dataset, dbh_cm, is_large)
)

# -----------------------------------------------------------------------------
# A) Tree-level DBH distribution figures
# -----------------------------------------------------------------------------
p_ecdf <- ggplot(tree_df, aes(x = dbh_cm, color = dataset)) +
  stat_ecdf(linewidth = 1) +
  coord_cartesian(xlim = c(0, 120)) +
  theme_minimal() +
  labs(title = "Tree DBH Distributions (ECDF)", x = "DBH (cm)", y = "ECDF")

ggsave(file.path(fig_dir, "tree_dbh_ecdf.png"), p_ecdf, width = 7, height = 4, dpi = 200)

p_density <- ggplot(tree_df, aes(x = dbh_cm, fill = dataset)) +
  geom_density(alpha = 0.35) +
  coord_cartesian(xlim = c(0, 120)) +
  theme_minimal() +
  labs(title = "Tree DBH Distributions (Density)", x = "DBH (cm)", y = "Density")

ggsave(file.path(fig_dir, "tree_dbh_density.png"), p_density, width = 7, height = 4, dpi = 200)

# Tail zoom (for 30–120cm)
p_tail <- ggplot(tree_df %>% filter(dbh_cm >= 30), aes(x = dbh_cm, color = dataset)) +
  stat_ecdf(linewidth = 1) +
  coord_cartesian(xlim = c(30, 120)) +
  theme_minimal() +
  labs(title = "Tree DBH Tail (ECDF, DBH ≥ 30cm)", x = "DBH (cm)", y = "ECDF")

ggsave(file.path(fig_dir, "tree_dbh_ecdf_tail.png"), p_tail, width = 7, height = 4, dpi = 200)

# -----------------------------------------------------------------------------
# A) Tail enrichment tables
# -----------------------------------------------------------------------------
fia_q <- quantile(fia_tree$dbh_cm, probs = c(0.90, 0.95, 0.99), na.rm = TRUE)

tail_enrichment <- tibble(
  threshold = c("FIA_q90", "FIA_q95", "FIA_q99", "DBH>50cm"),
  dbh_cm = c(as.numeric(fia_q[1]), as.numeric(fia_q[2]), as.numeric(fia_q[3]), LARGE_DBH_CM),
  pct_fia = c(
    100 * mean(fia_tree$dbh_cm > fia_q[1], na.rm = TRUE),
    100 * mean(fia_tree$dbh_cm > fia_q[2], na.rm = TRUE),
    100 * mean(fia_tree$dbh_cm > fia_q[3], na.rm = TRUE),
    100 * mean(fia_tree$dbh_cm > LARGE_DBH_CM, na.rm = TRUE)
  ),
  pct_nefin = c(
    100 * mean(nefin_tree$dbh_cm > fia_q[1], na.rm = TRUE),
    100 * mean(nefin_tree$dbh_cm > fia_q[2], na.rm = TRUE),
    100 * mean(nefin_tree$dbh_cm > fia_q[3], na.rm = TRUE),
    100 * mean(nefin_tree$dbh_cm > LARGE_DBH_CM, na.rm = TRUE)
  )
)

write_csv(tail_enrichment, file.path(tab_dir, "tree_dbh_tail_enrichment.csv"))

dbh_quantiles <- tree_df %>%
  group_by(dataset) %>%
  summarise(
    q50 = quantile(dbh_cm, 0.50, na.rm = TRUE),
    q90 = quantile(dbh_cm, 0.90, na.rm = TRUE),
    q95 = quantile(dbh_cm, 0.95, na.rm = TRUE),
    q99 = quantile(dbh_cm, 0.99, na.rm = TRUE),
    max = max(dbh_cm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write_csv(dbh_quantiles, file.path(tab_dir, "tree_dbh_quantiles_by_dataset.csv"))

# -----------------------------------------------------------------------------
# B) Plot-level metrics (recompute if needed, or read existing)
# -----------------------------------------------------------------------------
if (file.exists(path_plot_edge_metrics)) {
  message("Reading existing plot metrics: ", path_plot_edge_metrics)
  plot_metrics <- read_csv(path_plot_edge_metrics, show_col_types = FALSE)
} else {
  message("plot_edge_metrics.csv not found; computing minimal plot metrics here...")
  
  fia_plot_metrics <- fia_tree %>%
    mutate(ba_m2 = ba_m2_from_dbh_cm(dbh_cm)) %>%
    group_by(dataset, plot_id) %>%
    summarise(
      n_trees = n(),
      max_dbh = max(dbh_cm, na.rm = TRUE),
      p95_dbh = as.numeric(quantile(dbh_cm, 0.95, na.rm = TRUE)),
      pct_large_trees = 100 * mean(is_large, na.rm = TRUE),
      qmd_cm = sqrt(mean(dbh_cm^2, na.rm = TRUE)),
      pct_ba_large = 100 * (sum(ba_m2[is_large], na.rm = TRUE) / sum(ba_m2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  if (!is.null(tph_col) && !is.null(baph_col)) {
    nefin_plot_metrics <- nefin_tree %>%
      group_by(dataset, plot_id) %>%
      summarise(
        tph_total = sum(tph, na.rm = TRUE),
        max_dbh = max(dbh_cm, na.rm = TRUE),
        p95_dbh = w_quantile(dbh_cm, tph, probs = 0.95)[1],
        pct_large_trees = 100 * (sum(tph[is_large], na.rm = TRUE) / sum(tph, na.rm = TRUE)),
        qmd_cm = w_qmd(dbh_cm, tph),
        pct_ba_large = 100 * (sum(baph[is_large], na.rm = TRUE) / sum(baph, na.rm = TRUE)),
        .groups = "drop"
      )
  } else {
    nefin_plot_metrics <- nefin_tree %>%
      group_by(dataset, plot_id) %>%
      summarise(
        n_trees = n(),
        max_dbh = max(dbh_cm, na.rm = TRUE),
        p95_dbh = as.numeric(quantile(dbh_cm, 0.95, na.rm = TRUE)),
        pct_large_trees = 100 * mean(is_large, na.rm = TRUE),
        qmd_cm = sqrt(mean(dbh_cm^2, na.rm = TRUE)),
        .groups = "drop"
      ) %>% mutate(pct_ba_large = NA_real_)
  }
  
  plot_metrics <- bind_rows(fia_plot_metrics, nefin_plot_metrics)
}

# Plot-level ECDFs for a few metrics
plot_ecdf_metrics <- c("p95_dbh", "max_dbh", "pct_large_trees", "qmd_cm")
plot_ecdf_metrics <- plot_ecdf_metrics[plot_ecdf_metrics %in% names(plot_metrics)]

for (m in plot_ecdf_metrics) {
  dfp <- plot_metrics %>% filter(is.finite(.data[[m]]))
  p <- ggplot(dfp, aes(x = .data[[m]], color = dataset)) +
    stat_ecdf(linewidth = 1) +
    theme_minimal() +
    labs(title = paste0("Plot-level ECDF: ", m), x = m, y = "ECDF")
  ggsave(file.path(fig_dir, paste0("plot_metric_ecdf_", m, ".png")), p, width = 7, height = 4, dpi = 200)
}

write_csv(
  plot_metrics %>%
    pivot_longer(any_of(c("max_dbh","p95_dbh","pct_large_trees","pct_ba_large","qmd_cm")),
                 names_to="metric", values_to="value") %>%
    group_by(dataset, metric) %>%
    summarise(
      q50 = quantile(value, 0.50, na.rm=TRUE),
      q90 = quantile(value, 0.90, na.rm=TRUE),
      q95 = quantile(value, 0.95, na.rm=TRUE),
      q99 = quantile(value, 0.99, na.rm=TRUE),
      n = sum(is.finite(value)),
      .groups="drop"
    ),
  file.path(tab_dir, "plot_metric_quantiles_by_dataset.csv")
)

# -----------------------------------------------------------------------------
# C) Semivariograms (optional; requires coords)
# -----------------------------------------------------------------------------
if (!have_sf || !have_gstat) {
  message("sf/gstat not installed; skipping semivariogram section.")
} else {
  # Try load plot coordinate tables
  fia_plot_df <- if (file.exists(path_fia_plot)) read_csv(path_fia_plot, show_col_types = FALSE) else NULL
  nefin_plot_df <- if (file.exists(path_nefin_plot)) read_csv(path_nefin_plot, show_col_types = FALSE) else NULL
  
  if (is.null(fia_plot_df) || is.null(nefin_plot_df)) {
    message("plot.csv or NEFIN_plots.csv missing; skipping semivariogram section.")
  } else {
    # Detect plot id + coordinate columns
    fia_plot_id <- pick_first_existing(fia_plot_df, c("CN","PLT_CN","plot_id","PLOT_ID"))
    nef_plot_id <- pick_first_existing(nefin_plot_df, c("_nefin_plotID","plot_id","CN","PLOT_ID"))
    
    # Common coord patterns
    fia_lon <- pick_first_existing(fia_plot_df, c("LON", "lon", "LON_PUBLIC", "LONGITUDE", "ACTUAL_LON"))
    fia_lat <- pick_first_existing(fia_plot_df, c("LAT", "lat", "LAT_PUBLIC", "LATITUDE", "ACTUAL_LAT"))
    
    nef_lon <- pick_first_existing(nefin_plot_df, c("lon","LON","longitude","LONGITUDE"))
    nef_lat <- pick_first_existing(nefin_plot_df, c("lat","LAT","latitude","LATITUDE"))
    
    if (is.null(fia_plot_id) || is.null(nef_plot_id) || is.null(fia_lon) || is.null(fia_lat) || is.null(nef_lon) || is.null(nef_lat)) {
      message("Could not identify plot_id/lon/lat columns; skipping semivariograms.")
    } else {
      # Build plot coordinate frames
      fia_coords <- fia_plot_df %>%
        transmute(dataset="FIA",
                  plot_id = as.character(.data[[fia_plot_id]]),
                  lon = as.numeric(.data[[fia_lon]]),
                  lat = as.numeric(.data[[fia_lat]])) %>%
        filter(is.finite(lon), is.finite(lat))
      
      nefin_coords <- nefin_plot_df %>%
        transmute(dataset="NEFIN",
                  plot_id = as.character(.data[[nef_plot_id]]),
                  lon = as.numeric(.data[[nef_lon]]),
                  lat = as.numeric(.data[[nef_lat]])) %>%
        filter(is.finite(lon), is.finite(lat))
      
      coords_all <- bind_rows(fia_coords, nefin_coords)
      
      # Join plot metrics for semivariograms
      metrics_for_vg <- c("p95_dbh", "max_dbh", "qmd_cm", "pct_large_trees")
      metrics_for_vg <- metrics_for_vg[metrics_for_vg %in% names(plot_metrics)]
      
      plot_for_vg <- coords_all %>%
        inner_join(plot_metrics %>% select(dataset, plot_id, any_of(metrics_for_vg)),
                   by = c("dataset","plot_id"))
      
      # Convert to sf and transform to 5070 for distance in meters
      sf <- sf::st_as_sf(plot_for_vg, coords = c("lon","lat"), crs = 4326, remove = FALSE)
      sf5070 <- sf::st_transform(sf, 5070)
      
      # Semivariogram per dataset, per metric
      for (m in metrics_for_vg) {
        for (ds in c("FIA","NEFIN")) {
          sdf <- sf5070 %>% filter(dataset == ds, is.finite(.data[[m]]))
          if (nrow(sdf) < 50) next
          
          vg <- gstat::variogram(as.formula(paste0(m, " ~ 1")), sdf)
          p <- ggplot(vg, aes(x = dist, y = gamma)) +
            geom_point() +
            geom_line() +
            theme_minimal() +
            labs(
              title = paste0(ds, ": Empirical Semivariogram of ", m),
              x = "Distance (m)",
              y = "Semivariance"
            )
          
          ggsave(file.path(fig_dir, paste0("semivariogram_", ds, "_", m, ".png")),
                 p, width = 7, height = 4, dpi = 200)
          
          write_csv(vg, file.path(tab_dir, paste0("semivariogram_", ds, "_", m, ".csv")))
        }
      }
      
      message("Semivariograms written to figures/ and tables/ (when enough points exist).")
    }
  }
}

message("\nDONE. Outputs in: ", out_dir)
