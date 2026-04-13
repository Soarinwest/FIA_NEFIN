# ============================================================================
# FIA-NEFIN Explorer: Global Environment
# ============================================================================
# Loads packages, data, and global variables used across all modules
# Author: Soren Donisvitch
# Date: 2025-02-11 (updated 2026-04)
# ============================================================================

# PROJ fix — must be before any library() call --------------------------------
Sys.setenv(PROJ_DATA    = "")
Sys.setenv(PROJ_LIB     = "")
Sys.setenv(PROJ_NETWORK = "OFF")

# Suppress jsonlite named-vector deprecation warning (triggered by plotly
# serialization internals — harmless, will be resolved in a future plotly release)
globalCallingHandlers(
  warning = function(w) {
    if (grepl("keep_vec_names", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  }
)

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
sf::sf_use_s2(FALSE)
library(terra)
library(viridis)
library(tigris)

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
    species_code = tolower(species_code),  # Match tree_data$species_name format
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

# Hex data (tabular, all scales)
hex_data <- readRDS("data/hex_data.rds")
# Default hex for Spatial Explorer — must be sf with geometry
hex_1kha <- sf::st_read("data/hex_geojsons/hex_1kha.geojson", quiet = TRUE)

# State boundaries for spatial overlay (HTTP call — wrapped for offline safety)
states_sf <- tryCatch(
  tigris::states(year = 2020, progress_bar = FALSE) |> sf::st_transform(4326),
  error = function(e) {
    message("Could not load state boundaries: ", e$message)
    NULL
  }
)

# Scale analysis data
scale_metrics <- readRDS("data/scale_metrics.rds")

# Bootstrap variance (per-dataset, not per-scale)
bootstrap_variance <- tryCatch(
  readRDS("data/bootstrap_variance.rds"),
  error = function(e) {
    message("bootstrap_variance.rds not found -- bootstrap table will be hidden")
    NULL
  }
)

# Modeling results data (loaded as list from single RDS file)
cv_results_list  <- readRDS("data/cv_results.rds")
cv_results       <- if (is.null(cv_results_list$summary)) data.frame() else cv_results_list$summary
fold_results     <- cv_results_list$folds
test_predictions <- cv_results_list$test_preds
var_importance   <- cv_results_list$importance
fuzzing_sig      <- cv_results_list$significance
fuzzing_rmse     <- cv_results_list$fuzzing

# Global variables ------------------------------------------------------------

# Dataset colors (colorblind-safe, data-focused palette)
DATASET_COLORS <- c(
  "FIA"    = "#3b82f6",  # Soft blue
  "NEFIN"  = "#f59e0b",  # Soft orange/amber
  "Pooled" = "#14b8a6"   # Teal
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

# Create species choices for dropdown -- only species with tree data
available_species <- unique(tree_data$species_name)
SPECIES_CHOICES <- species_summary |>
  dplyr::filter(species_code %in% available_species) |>
  dplyr::arrange(dplyr::desc(p99_diff)) |>
  dplyr::mutate(
    label = glue::glue(
      "{common_name} (FIA: {scales::comma(fia_n_trees)} | NEFIN: {scales::comma(nefin_n_trees)})"
    )
  ) |>
  dplyr::select(label, species_code) |>
  tibble::deframe()

# Add "All Species" option at top
SPECIES_CHOICES <- c("All Species" = "ALL", SPECIES_CHOICES)
