# ============================================================================
# FIA-NEFIN Explorer: Global Environment
# ============================================================================
# Loads packages, data, and global variables used across all modules
# Author: Soren Walljasper
# Date: 2025-02-11 (updated 2026-04)
# ============================================================================

# Packages --------------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(glue)
library(stringr)
library(readr)
library(tibble)

# Spatial / mapping
library(leaflet)
library(leaflet.extras)
library(leafsync)
library(leafem)
library(sf)
library(terra)

# Source helper files ---------------------------------------------------------
source("R/plot_theme.R")
source("R/utils.R")
source("R/mod_summary_stats.R")
source("R/mod_distributions.R")
source("R/mod_species.R")
source("R/mod_overview.R")
source("R/mod_spatial.R")
source("R/mod_scale.R")
source("R/mod_modeling.R")

# Load data -------------------------------------------------------------------

# Ensure ggplot2 is fully loaded before sourcing plugins
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 package required but not installed")
}

# Core data for existing Dataset Comparison tab
tryCatch({
  plot_data        <- readRDS("data/plot_data.rds")
  species_summary  <- readRDS("data/species_summary.rds")
  tree_data        <- readRDS("data/tree_data.rds")
}, error = function(e) {
  stop("Failed to load core RDS files: ", conditionMessage(e))
})

# Transform species_summary column names to match expected schema
species_summary <- species_summary |>
  dplyr::rename(
    species_code  = dplyr::any_of("species"),
    fia_n_trees   = dplyr::any_of("n__FIA"),
    nefin_n_trees = dplyr::any_of("n__NEFIN"),
    p99_diff      = dplyr::any_of("dbh_p99_delta"),
    p99_est       = dplyr::any_of("dbh_p99_est"),
    p99_lo95      = dplyr::any_of("dbh_p99_lo95"),
    p99_hi95      = dplyr::any_of("dbh_p99_hi95")
  ) |>
  dplyr::mutate(
    common_name  = tools::toTitleCase(species_code),
    p99_diff_se  = (p99_hi95 - p99_lo95) / (2 * 1.96),
    p99_pvalue   = ifelse(p99_lo95 > 0 | p99_hi95 < 0, 0.01, 0.5),
    p95_diff     = NA_real_,
    max_diff     = NA_real_,
    .keep = "all"
  )

# Spatial data for Spatial Explorer tab
# Derived from unified plot_data structure
fia_plots        <- plot_data |> dplyr::filter(dataset == "FIA")
nefin_plots      <- plot_data |> dplyr::filter(dataset == "NEFIN")

# Uncertainty data (renamed from plot_uncertainty.rds)
uncertainty_data <- readRDS("data/uncertainty_data.rds")
plot_uncertainty <- uncertainty_data  # Alias for backward compatibility

# Hex data (all 9 scales stacked, extract 1kha for default display)
hex_data <- readRDS("data/hex_data.rds")
hex_1kha <- hex_data |> dplyr::filter(scale == "1kha")

# State boundaries for spatial overlay
states_sf <- tigris::states(year = 2020, progress_bar = FALSE) |>
  sf::st_transform(4326)

# Scale analysis data
scale_metrics <- readRDS("data/scale_metrics.rds")

# Bootstrap variance may be bundled in scale_metrics; provide gracefully
bootstrap_variance <- if (
  "pct_bootstrap_var" %in% names(scale_metrics) ||
  "bootstrap_var" %in% names(scale_metrics)
) {
  scale_metrics |>
    dplyr::select(scale, dplyr::any_of(
      c("pct_bootstrap_var", "bootstrap_var", "bootstrap_variance")
    ))
} else {
  NULL  # Modules handle NULL gracefully
}

# Modeling results data (loaded as list from single RDS file)
cv_results_list  <- readRDS("data/cv_results.rds")
cv_results       <- if (is.null(cv_results_list$summary)) data.frame() else cv_results_list$summary
fold_results     <- cv_results_list$folds
test_predictions <- cv_results_list$test_preds
var_importance   <- cv_results_list$importance
fuzzing_sig      <- cv_results_list$significance
fuzzing_rmse     <- cv_results_list$fuzzing

# Global variables ------------------------------------------------------------

# Dataset colors (colorblind-safe)
DATASET_COLORS <- c(
  "FIA"    = "#E69F00",  # Orange
  "NEFIN"  = "#56B4E9",  # Sky blue
  "Pooled" = "#009E73"   # Green
)

# State list for filter dropdown
STATE_LIST <- c("ME", "NH", "VT", "MA", "CT", "RI", "NY")

# Path to hex GeoJSON files (self-contained in app)
HEX_SRC_PATH <- normalizePath(
  "data/hex_geojsons",
  mustWork = FALSE
)

# Top 10 species by P99 difference (for default display in mod_species)
TOP_SPECIES <- species_summary |>
  dplyr::arrange(dplyr::desc(p99_diff)) |>
  dplyr::slice(1:10) |>
  dplyr::pull(species_code)

# Create species choices for dropdown (with counts)
SPECIES_CHOICES <- species_summary |>
  dplyr::arrange(dplyr::desc(p99_diff)) |>
  dplyr::mutate(
    label = glue::glue(
      "{common_name} (FIA: {scales::comma(fia_n_trees)} | NEFIN: {scales::comma(nefin_n_trees)})"
    )
  ) |>
  dplyr::select(species_code, label) |>
  tibble::deframe()

# Add "All Species" option at top
SPECIES_CHOICES <- c("All Species" = "ALL", SPECIES_CHOICES)
