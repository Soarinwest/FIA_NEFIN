# ============================================================================
# FIA-NEFIN Explorer: Global Environment
# ============================================================================
# Loads packages, data, and global variables used across all modules
# Author: Soren Walljasper
# Date: 2025-02-11
# ============================================================================

# Packages --------------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(glue)

# Source helper files ---------------------------------------------------------
source("R/plot_theme.R")
source("R/utils.R")
source("R/mod_summary_stats.R")
source("R/mod_distributions.R")
source("R/mod_species.R")

# Load data -------------------------------------------------------------------
plot_data <- readRDS("data/plot_data.rds")
species_summary <- readRDS("data/species_summary.rds")
tree_data <- readRDS("data/tree_data.rds")

# Global variables ------------------------------------------------------------

# Dataset colors (used throughout app)
DATASET_COLORS <- c(
  "FIA" = "#E69F00",      # Orange
  "NEFIN" = "#56B4E9",    # Sky blue
  "Pooled" = "#999999"    # Gray
)

# State list for filter dropdown
STATE_LIST <- c("ME", "NH", "VT", "MA", "CT", "RI", "NY")

# Top 10 species by P99 difference (for default display)
TOP_SPECIES <- species_summary %>%
  arrange(desc(p99_diff)) %>%
  slice(1:10) %>%
  pull(species_code)

# Create species choices for dropdown (with counts)
SPECIES_CHOICES <- species_summary %>%
  arrange(desc(p99_diff)) %>%
  mutate(
    label = glue("{common_name} (FIA: {scales::comma(fia_n_trees)} | NEFIN: {scales::comma(nefin_n_trees)})")
  ) %>%
  select(species_code, label) %>%
  deframe()

# Add "All Species" option at top
SPECIES_CHOICES <- c("All Species" = "ALL", SPECIES_CHOICES)
