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

# Core data for existing Dataset Comparison tab
plot_data        <- readRDS("data/plot_data.rds")
species_summary  <- readRDS("data/species_summary.rds")
tree_data        <- readRDS("data/tree_data.rds")

# Spatial data for Spatial Explorer tab
fia_plots        <- readRDS("data/fia_plots.rds")
nefin_plots      <- readRDS("data/nefin_plots.rds")
plot_uncertainty <- readRDS("data/plot_uncertainty.rds")
hex_1kha         <- readRDS("data/hex_1kha.rds")
states_sf        <- readRDS("data/states.rds")

# Scale analysis data
scale_metrics      <- readRDS("data/scale_metrics.rds")
bootstrap_variance <- readRDS("data/bootstrap_variance.rds")

# Modeling results data
cv_results       <- readRDS("data/cv_results.rds")
fold_results     <- readRDS("data/fold_results.rds")
test_predictions <- readRDS("data/test_predictions.rds")
var_importance   <- readRDS("data/var_importance.rds")
fuzzing_sig      <- readRDS("data/fuzzing_significance.rds")
fuzzing_rmse     <- readRDS("data/fuzzing_rmse.rds")

# Global variables ------------------------------------------------------------

# Dataset colors (colorblind-safe)
DATASET_COLORS <- c(
  "FIA"    = "#E69F00",  # Orange
  "NEFIN"  = "#56B4E9",  # Sky blue
  "Pooled" = "#009E73"   # Green
)

# State list for filter dropdown
STATE_LIST <- c("ME", "NH", "VT", "MA", "CT", "RI", "NY")

# Path to source GeoJSON hex files (for lazy loading in mod_spatial Sub-tab B)
# normalizePath resolves relative to the app working directory (FIA_NEFIN_explorer/)
HEX_SRC_PATH <- normalizePath(
  "../data/processed/hex_geojson_with_stats",
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
