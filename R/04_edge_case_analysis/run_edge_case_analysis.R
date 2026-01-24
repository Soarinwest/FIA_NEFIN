# =============================================================================
# Edge Case / Large Tree Bias Analysis (FIA vs NEFIN)
# Standalone module for:
#   - per-plot edge metrics (large trees, mortality, structure)
#   - distribution + tail comparisons
#   - forest typology
#   - optional: prediction-error interaction tests (edge x dataset x scale)
#
# Inputs (expected in your repo):
#   FIA tree:   data/interim/fia/extracted/tree.csv
#   NEFIN tree: data/raw/nefin/TREE_RAW_DATA.csv
#   Optional predictions:
#     data/processed/phase4_cv_results/fold_predictions/predictions_fine_fia_only.csv
#     data/processed/phase4_cv_results/fold_predictions/predictions_fine_nefin_only.csv
#
# Outputs:
#   data/processed/edge_case_analysis/
#     plot_edge_metrics.csv
#     quantiles_by_dataset.csv
#     tail_exceedance.csv
#     forest_state_frequencies.csv
#     (optional) error_by_strata.csv
#     figures/*.png
#
# Author: (your name)
# Updated: Jan 2026
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(purrr)
})

# -----------------------------------------------------------------------------
# Config (optional)
# -----------------------------------------------------------------------------
CONFIG <- NULL
if (file.exists("R/00_config/config.R")) {
  source("R/00_config/config.R")
}

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
path_fia_tree   <- "data/interim/fia/extracted/tree.csv"
path_nefin_tree <- "data/raw/nefin/TREE_RAW_DATA.csv"

pred_candidates <- list(
  fine_fia   = "data/processed/phase4_cv_results/fold_predictions/predictions_fine_fia_only.csv",
  fine_nefin = "data/processed/phase4_cv_results/fold_predictions/predictions_fine_nefin_only.csv"
  # add coarse files here if you have them later
)

out_dir <- "data/processed/edge_case_analysis"
fig_dir <- file.path(out_dir, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Analysis parameters
# -----------------------------------------------------------------------------
LARGE_DBH_CM <- 50

# -----------------------------------------------------------------------------
# Helpers: units + weighted stats
# -----------------------------------------------------------------------------
inch_to_cm <- function(x) x * 2.54

ba_m2_from_dbh_cm <- function(dbh_cm) {
  pi * ((dbh_cm / 100) / 2)^2
}

w_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2) return(NA_real_)
  mu <- w_mean(x[ok], w[ok])
  sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok])
}

w_sd <- function(x, w) sqrt(w_var(x, w))

w_quantile <- function(x, w, probs = 0.95) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

w_cv <- function(x, w) {
  mu <- w_mean(x, w)
  sd <- w_sd(x, w)
  if (!is.finite(mu) || mu == 0) return(NA_real_)
  sd / mu
}

w_qmd <- function(dbh_cm, w) {
  ok <- is.finite(dbh_cm) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sqrt(sum(w[ok] * dbh_cm[ok]^2) / sum(w[ok]))
}

# -----------------------------------------------------------------------------
# Robust column mapping (plot id + predictions)
# -----------------------------------------------------------------------------
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[1]
}

standardize_plot_id <- function(df, dataset = c("FIA", "NEFIN")) {
  dataset <- match.arg(dataset)
  # For tree tables, we know the canonical plot id columns from your description:
  # FIA: PLT_CN
  # NEFIN: _nefin_plotID
  col <- if (dataset == "FIA") {
    pick_first_existing(df, c("PLT_CN", "CN", "plot_id", "plotid"))
  } else {
    pick_first_existing(df, c("_nefin_plotID", "plot_id", "CN", "plotid"))
  }
  if (is.null(col)) stop("Could not identify plot id column for ", dataset)
  df %>% mutate(plot_id = as.character(.data[[col]]))
}

# For prediction files: try to find a usable plot id column automatically.
standardize_pred_plot_id <- function(df) {
  col <- pick_first_existing(df, c("CN","plot_id","PLT_CN","plt_cn","plotid","id"))
  if (is.null(col)) stop("Could not identify plot id column in predictions file.")
  df %>% mutate(plot_id = as.character(.data[[col]]))
}

# Try to infer observed/predicted columns from common names
infer_obs_pred_cols <- function(df) {
  y_col <- pick_first_existing(df, c("y","y_true","observed","obs","biomass","biomass_obs","target"))
  p_col <- pick_first_existing(df, c("pred","prediction","y_pred","predicted","biomass_pred","fit"))
  list(y = y_col, pred = p_col)
}

# -----------------------------------------------------------------------------
# 1) Load tree data
# -----------------------------------------------------------------------------
message("\nLoading FIA tree table: ", path_fia_tree)
fia_tree_raw <- read_csv(path_fia_tree, show_col_types = FALSE)

message("Loading NEFIN tree table: ", path_nefin_tree)
nefin_tree_raw <- read_csv(path_nefin_tree, show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 2) Harmonize + per-tree derived fields
# -----------------------------------------------------------------------------
fia_tree <- fia_tree_raw %>%
  standardize_plot_id("FIA") %>%
  transmute(
    dataset = "FIA",
    plot_id,
    dbh_cm = inch_to_cm(as.numeric(DIA)),
    status = as.integer(STATUSCD),         # 1 alive, 2 dead
    biomass_lb = as.numeric(DRYBIO_AG)     # aboveground dry biomass (lbs)
  ) %>%
  filter(is.finite(dbh_cm), dbh_cm > 0) %>%
  mutate(
    ba_m2 = ba_m2_from_dbh_cm(dbh_cm),
    is_dead = status == 2,
    is_large = dbh_cm > LARGE_DBH_CM
  )

nefin_tree <- nefin_tree_raw %>%
  standardize_plot_id("NEFIN") %>%
  transmute(
    dataset = "NEFIN",
    plot_id,
    dbh_cm = as.numeric(DBH),
    status = as.integer(treeStatus),           # 1 alive, 2 dead
    tph = as.numeric(`_nefin_treeCntPH`),      # expansion factor (trees/ha)
    baph = as.numeric(`_nefin_treeBAPH`)       # basal area/ha (already expanded)
    # If you have species columns, add them here later.
  ) %>%
  filter(is.finite(dbh_cm), dbh_cm > 0) %>%
  mutate(
    ba_m2 = ba_m2_from_dbh_cm(dbh_cm),
    is_dead = status == 2,
    is_large = dbh_cm > LARGE_DBH_CM
  )

# -----------------------------------------------------------------------------
# 3) Per-plot edge metrics
# -----------------------------------------------------------------------------
message("\nComputing FIA per-plot metrics (unweighted tree records)...")
fia_plot_metrics <- fia_tree %>%
  group_by(dataset, plot_id) %>%
  summarise(
    n_trees = n(),
    
    # Large-tree structure
    max_dbh = max(dbh_cm, na.rm = TRUE),
    p95_dbh = as.numeric(quantile(dbh_cm, 0.95, na.rm = TRUE, type = 7)),
    pct_large_trees = 100 * mean(is_large, na.rm = TRUE),
    pct_ba_large = 100 * (sum(ba_m2[is_large], na.rm = TRUE) / sum(ba_m2, na.rm = TRUE)),
    qmd_cm = sqrt(mean(dbh_cm^2, na.rm = TRUE)),
    
    # Mortality
    mortality_ratio = 100 * mean(is_dead, na.rm = TRUE),
    dead_biomass_fraction = 100 * (sum(biomass_lb[is_dead], na.rm = TRUE) / sum(biomass_lb, na.rm = TRUE)),
    n_dead_large = sum(is_dead & is_large, na.rm = TRUE),
    
    # Structural diversity
    cv_dbh = sd(dbh_cm, na.rm = TRUE) / mean(dbh_cm, na.rm = TRUE),
    
    .groups = "drop"
  )

message("Computing NEFIN per-plot metrics (weighted by TPH and BA/ha)...")
nefin_plot_metrics <- nefin_tree %>%
  group_by(dataset, plot_id) %>%
  summarise(
    tph_total = sum(tph, na.rm = TRUE),
    
    # Large-tree structure (TPH-weighted for abundance; BA/ha for BA fraction)
    max_dbh = max(dbh_cm, na.rm = TRUE),
    p95_dbh = w_quantile(dbh_cm, tph, probs = 0.95)[1],
    pct_large_trees = 100 * (sum(tph[is_large], na.rm = TRUE) / sum(tph, na.rm = TRUE)),
    pct_ba_large = 100 * (sum(baph[is_large], na.rm = TRUE) / sum(baph, na.rm = TRUE)),
    qmd_cm = w_qmd(dbh_cm, tph),
    
    # Mortality (TPH-weighted; BA proxy available)
    mortality_ratio = 100 * (sum(tph[is_dead], na.rm = TRUE) / sum(tph, na.rm = TRUE)),
    dead_ba_fraction = 100 * (sum(baph[is_dead], na.rm = TRUE) / sum(baph, na.rm = TRUE)),
    n_dead_large = sum(tph[is_dead & is_large], na.rm = TRUE),
    
    # Structural diversity (TPH-weighted)
    cv_dbh = w_cv(dbh_cm, tph),
    
    .groups = "drop"
  ) %>%
  # Align with FIA columns (biomass fraction not available here)
  mutate(dead_biomass_fraction = NA_real_)

# Add dead_ba_fraction for FIA too (computed from BA as a symmetric proxy)
fia_plot_metrics <- fia_plot_metrics %>%
  left_join(
    fia_tree %>%
      group_by(plot_id) %>%
      summarise(
        dead_ba_fraction = 100 * (sum(ba_m2[is_dead], na.rm = TRUE) / sum(ba_m2, na.rm = TRUE)),
        .groups = "drop"
      ),
    by = "plot_id"
  )

plot_edge_metrics <- bind_rows(fia_plot_metrics, nefin_plot_metrics)

write_csv(plot_edge_metrics, file.path(out_dir, "plot_edge_metrics.csv"))
message("✓ Wrote: ", file.path(out_dir, "plot_edge_metrics.csv"))

# -----------------------------------------------------------------------------
# 4) Distribution comparison: quantile table + ECDF figures
# -----------------------------------------------------------------------------
metrics_for_quantiles <- c(
  "max_dbh","p95_dbh","pct_large_trees","pct_ba_large","qmd_cm",
  "mortality_ratio","dead_ba_fraction","cv_dbh","n_dead_large"
)

quantiles_by_dataset <- plot_edge_metrics %>%
  pivot_longer(all_of(metrics_for_quantiles), names_to = "metric", values_to = "value") %>%
  group_by(dataset, metric) %>%
  summarise(
    q50 = quantile(value, 0.50, na.rm = TRUE),
    q90 = quantile(value, 0.90, na.rm = TRUE),
    q95 = quantile(value, 0.95, na.rm = TRUE),
    q99 = quantile(value, 0.99, na.rm = TRUE),
    n = sum(is.finite(value)),
    .groups = "drop"
  )

write_csv(quantiles_by_dataset, file.path(out_dir, "quantiles_by_dataset.csv"))
message("✓ Wrote: ", file.path(out_dir, "quantiles_by_dataset.csv"))

# ECDF plots for a few headline metrics
ecdf_metrics <- c("p95_dbh","pct_ba_large","mortality_ratio","cv_dbh")
for (m in ecdf_metrics) {
  p <- ggplot(plot_edge_metrics, aes(x = .data[[m]], color = dataset)) +
    stat_ecdf(size = 1) +
    theme_minimal() +
    labs(
      title = paste0("ECDF: ", m),
      x = m, y = "ECDF"
    )
  ggsave(filename = file.path(fig_dir, paste0("ecdf_", m, ".png")), plot = p, width = 7, height = 4, dpi = 200)
}

message("✓ Wrote ECDF figures to: ", fig_dir)

# -----------------------------------------------------------------------------
# 5) Tail analysis: % NEFIN plots exceeding FIA 95th percentiles
# -----------------------------------------------------------------------------
fia_p95 <- plot_edge_metrics %>%
  filter(dataset == "FIA") %>%
  summarise(across(all_of(metrics_for_quantiles), ~ quantile(.x, 0.95, na.rm = TRUE)))

tail_exceedance <- plot_edge_metrics %>%
  filter(dataset == "NEFIN") %>%
  summarise(across(
    all_of(metrics_for_quantiles),
    ~ 100 * mean(.x > fia_p95[[cur_column()]], na.rm = TRUE),
    .names = "exceed_{.col}"
  )) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "pct_nefin_exceed_fia95") %>%
  mutate(metric = str_replace(metric, "^exceed_", ""))

write_csv(tail_exceedance, file.path(out_dir, "tail_exceedance.csv"))
message("✓ Wrote: ", file.path(out_dir, "tail_exceedance.csv"))

# -----------------------------------------------------------------------------
# 6) Forest typology (thresholds learned from FIA distribution)
# -----------------------------------------------------------------------------
thr <- plot_edge_metrics %>%
  filter(dataset == "FIA") %>%
  summarise(
    thr_p95_dbh = quantile(p95_dbh, 0.95, na.rm = TRUE),
    thr_cv_dbh  = quantile(cv_dbh,  0.90, na.rm = TRUE),
    thr_mort    = quantile(mortality_ratio, 0.90, na.rm = TRUE)
  )

plot_typology <- plot_edge_metrics %>%
  mutate(
    is_large_structure = (pct_ba_large >= 50) | (p95_dbh >= thr$thr_p95_dbh),
    is_high_mortality  = mortality_ratio >= thr$thr_mort,
    is_uneven          = cv_dbh >= thr$thr_cv_dbh,
    forest_state = case_when(
      is_large_structure & is_high_mortality ~ "Large + High mortality",
      is_large_structure & is_uneven ~ "Large + Uneven",
      is_high_mortality & is_uneven ~ "Mortality + Uneven",
      is_large_structure ~ "Large structure",
      is_high_mortality ~ "High mortality",
      is_uneven ~ "Uneven structure",
      TRUE ~ "Typical"
    )
  )

forest_state_frequencies <- plot_typology %>%
  count(dataset, forest_state) %>%
  group_by(dataset) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

write_csv(forest_state_frequencies, file.path(out_dir, "forest_state_frequencies.csv"))
message("✓ Wrote: ", file.path(out_dir, "forest_state_frequencies.csv"))

p_state <- ggplot(forest_state_frequencies, aes(x = forest_state, y = pct, fill = dataset)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Forest State Typology Frequency", x = NULL, y = "Percent of plots")

ggsave(file.path(fig_dir, "forest_state_frequencies.png"), p_state, width = 9, height = 4.5, dpi = 200)

# -----------------------------------------------------------------------------
# 7) Optional: Edge cases vs prediction errors
# -----------------------------------------------------------------------------
existing_preds <- pred_candidates[map_lgl(pred_candidates, file.exists)]
if (length(existing_preds) == 0) {
  message("\nNo prediction files found. Skipping prediction-error tests.")
} else {
  message("\nLoading prediction files and testing edge-case interactions...")
  
  pred_list <- imap(existing_preds, function(path, name) {
    df <- read_csv(path, show_col_types = FALSE) %>%
      standardize_pred_plot_id()
    
    cols <- infer_obs_pred_cols(df)
    if (is.null(cols$y) || is.null(cols$pred)) {
      stop("Could not infer observed/pred columns in: ", path,
           "\nAdd/rename columns so script can detect y and pred.")
    }
    
    # infer dataset + scale from filename key (fine only in current candidates)
    dataset <- ifelse(str_detect(name, "nefin"), "NEFIN", "FIA")
    scale   <- ifelse(str_detect(name, "fine"), "fine", "unknown")
    
    df %>%
      transmute(
        dataset = dataset,
        scale = scale,
        plot_id,
        y = as.numeric(.data[[cols$y]]),
        pred = as.numeric(.data[[cols$pred]])
      ) %>%
      filter(is.finite(y), is.finite(pred)) %>%
      mutate(
        err = pred - y,
        abs_err = abs(err),
        sq_err = err^2
      )
  })
  
  pred_df <- bind_rows(pred_list)
  
  eval_df <- pred_df %>%
    inner_join(
      plot_typology %>%
        select(dataset, plot_id, forest_state, is_large_structure, is_high_mortality, is_uneven,
               max_dbh, p95_dbh, pct_ba_large, mortality_ratio, cv_dbh),
      by = c("dataset", "plot_id")
    ) %>%
    mutate(edge_any = is_large_structure | is_high_mortality | is_uneven)
  
  # Stratified summary
  error_by_strata <- eval_df %>%
    group_by(scale, dataset, edge_any) %>%
    summarise(
      n = n(),
      mean_abs_err = mean(abs_err, na.rm = TRUE),
      median_abs_err = median(abs_err, na.rm = TRUE),
      mean_sq_err = mean(sq_err, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(error_by_strata, file.path(out_dir, "error_by_strata.csv"))
  message("✓ Wrote: ", file.path(out_dir, "error_by_strata.csv"))
  
  # Edge score (standardized composite)
  eval_df2 <- eval_df %>%
    mutate(
      edge_score =
        (scale(p95_dbh)[,1] +
           scale(pct_ba_large)[,1] +
           scale(mortality_ratio)[,1] +
           scale(cv_dbh)[,1]) / 4,
      dataset = factor(dataset),
      scale = factor(scale)
    )
  
  # Interaction model: does dataset behave differently under edge conditions?
  fit <- lm(abs_err ~ dataset * edge_score * scale, data = eval_df2)
  sink(file.path(out_dir, "edge_interaction_model_summary.txt"))
  print(summary(fit))
  sink()
  message("✓ Wrote: ", file.path(out_dir, "edge_interaction_model_summary.txt"))
  
  # Quick viz: abs error vs edge score
  p_err <- ggplot(eval_df2, aes(x = edge_score, y = abs_err, color = dataset)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ scale) +
    theme_minimal() +
    labs(title = "Prediction Absolute Error vs Edge Score", x = "Edge score (z)", y = "|error|")
  
  ggsave(file.path(fig_dir, "abs_error_vs_edge_score.png"), p_err, width = 8, height = 4.5, dpi = 200)
  message("✓ Wrote: ", file.path(fig_dir, "abs_error_vs_edge_score.png"))
}

message("\nDONE.\nOutputs in: ", out_dir)
