# ============================================================================
# prep_app_data.R
# Run ONCE before first app launch to build all RDS/TIF files.
#
# Run from project root:
#   setwd("c:/Users/soren/FIA_NEFIN")
#   source("FIA_NEFIN_explorer/data/prep_app_data.R")
#
# Or from FIA_NEFIN_explorer/data/:
#   source("prep_app_data.R")
# ============================================================================

options(tigris_use_cache = TRUE)

library(dplyr)
library(readr)
library(sf)
library(terra)
library(tigris)

# ── Paths ─────────────────────────────────────────────────────────────────────
# Detect working directory and set ROOT accordingly
if (basename(getwd()) == "data" && file.exists("../../data/processed")) {
  ROOT <- normalizePath("../..")
  OUT  <- "."
} else if (file.exists("data/processed")) {
  ROOT <- normalizePath(".")
  OUT  <- "FIA_NEFIN_explorer/data"
} else {
  stop("Run from FIA_NEFIN/ or FIA_NEFIN_explorer/data/")
}

PROC    <- file.path(ROOT, "data", "processed")
WWW     <- file.path(ROOT, "FIA_NEFIN_explorer", "www")
OUT     <- file.path(ROOT, "FIA_NEFIN_explorer", "data")

dir.create(WWW, showWarnings = FALSE)
dir.create(OUT, showWarnings = FALSE)

state_lu <- c("9" = "CT", "23" = "ME", "25" = "MA",
              "33" = "NH", "36" = "NY", "44" = "RI", "50" = "VT")

message("=== prep_app_data.R starting ===")

# ── BLOCK 1: plot_data.rds ────────────────────────────────────────────────────
message("Block 1: Building plot_data.rds ...")

fia_cov <- read_csv(
  file.path(PROC, "baseline_with_covariates.csv"),
  show_col_types = FALSE
) |>
  filter(dataset == "FIA") |>
  select(
    CN, STATECD, MEASYEAR, lat, lon, biomass, n_trees, dataset, coord_source,
    ndvi_s2       = ndvi_s2_10m,
    ndvi_modis    = ndvi_modis_250m,
    temp_mean     = tmean_10m,
    precip_annual = ppt_10m,
    canopy_height = canopy_height_10m,
    elevation     = elevation_10m,
    slope         = slope_10m
  ) |>
  mutate(state = state_lu[as.character(STATECD)])

nefin_raw <- read_csv(
  file.path(PROC, "nefin_complete.csv"),
  show_col_types = FALSE
) |>
  select(CN, STATECD, MEASYEAR, lat, lon, biomass, dataset, coord_source) |>
  mutate(
    n_trees       = NA_integer_,
    ndvi_s2       = NA_real_,
    ndvi_modis    = NA_real_,
    temp_mean     = NA_real_,
    precip_annual = NA_real_,
    canopy_height = NA_real_,
    elevation     = NA_real_,
    slope         = NA_real_,
    state         = state_lu[as.character(STATECD)]
  )

plot_data <- bind_rows(fia_cov, nefin_raw)
saveRDS(plot_data, file.path(OUT, "plot_data.rds"))
message("  ✓ plot_data.rds (", nrow(plot_data), " rows)")

# ── BLOCK 2: species_summary.rds ─────────────────────────────────────────────
message("Block 2: Building species_summary.rds ...")

sp_raw <- read_csv(
  file.path(PROC, "large_tree_analysis", "species_summary.csv"),
  show_col_types = FALSE
)

species_summary <- sp_raw |>
  rename(
    species_code  = species,
    fia_n_trees   = n__FIA,
    nefin_n_trees = n__NEFIN,
    p99_diff      = dbh_p99_delta,
    p99_est       = dbh_p99_est,
    p99_lo95      = dbh_p99_lo95,
    p99_hi95      = dbh_p99_hi95
  ) |>
  mutate(
    common_name  = tools::toTitleCase(species_code),
    p99_diff_se  = (p99_hi95 - p99_lo95) / (2 * 1.96),
    p99_pvalue   = ifelse(p99_lo95 > 0 | p99_hi95 < 0, 0.01, 0.5),
    p95_diff     = NA_real_,
    max_diff     = NA_real_
  )

saveRDS(species_summary, file.path(OUT, "species_summary.rds"))
message("  ✓ species_summary.rds (", nrow(species_summary), " species)")

# ── BLOCK 3: tree_data.rds ────────────────────────────────────────────────────
message("Block 3: Building tree_data.rds ...")

sp_map <- read_csv(
  file.path(PROC, "fhm_species_mapping.csv"),
  show_col_types = FALSE
) |>
  select(SPCD, latin_name)

fia_tree <- read_csv(
  file.path(ROOT, "data", "interim", "fia", "extracted", "tree.csv"),
  col_select = c(PLT_CN, SPCD, DIA, STATUSCD),
  show_col_types = FALSE
) |>
  filter(STATUSCD == 1, !is.na(DIA), DIA > 0) |>
  left_join(sp_map, by = "SPCD") |>
  mutate(
    species_code = if_else(is.na(latin_name), paste0("SPCD_", SPCD), latin_name),
    dbh          = DIA,
    dataset      = "FIA"
  ) |>
  select(species_code, dbh, dataset)

nefin_tree <- read_csv(
  file.path(ROOT, "data", "raw", "nefin", "TREE_PLOT_DATA.csv"),
  show_col_types = FALSE
) |>
  filter(!is.na(QMD), QMD > 0) |>
  mutate(
    species_code = "unknown_nefin",
    dbh          = QMD,
    dataset      = "NEFIN"
  ) |>
  select(species_code, dbh, dataset)

tree_data <- bind_rows(fia_tree, nefin_tree)
saveRDS(tree_data, file.path(OUT, "tree_data.rds"))
message("  ✓ tree_data.rds (", nrow(tree_data), " trees)")

# ── BLOCK 4: Spatial plot data ────────────────────────────────────────────────
message("Block 4: Building spatial plot RDS files ...")

fia_plots <- read_csv(
  file.path(PROC, "fia_complete.csv"),
  show_col_types = FALSE
) |>
  select(CN, STATECD, COUNTYCD, PLOT, MEASYEAR, lat, lon, biomass, n_trees, coord_source) |>
  mutate(state = state_lu[as.character(STATECD)])
saveRDS(fia_plots, file.path(OUT, "fia_plots.rds"))
message("  ✓ fia_plots.rds (", nrow(fia_plots), " plots)")

nefin_plots <- read_csv(
  file.path(PROC, "nefin_complete.csv"),
  show_col_types = FALSE
) |>
  select(CN, STATECD, MEASYEAR, lat, lon, biomass, coord_source) |>
  mutate(state = state_lu[as.character(STATECD)])
saveRDS(nefin_plots, file.path(OUT, "nefin_plots.rds"))
message("  ✓ nefin_plots.rds (", nrow(nefin_plots), " plots)")

plot_uncertainty <- read_csv(
  file.path(PROC, "monte_carlo", "plot_uncertainty.csv"),
  show_col_types = FALSE
)
saveRDS(plot_uncertainty, file.path(OUT, "plot_uncertainty.rds"))
message("  ✓ plot_uncertainty.rds (", nrow(plot_uncertainty), " plots)")

# Hex 1kha (default scale) — pre-load as RDS to avoid re-reading 99MB GeoJSON
message("  Loading hex_1kha_complete.geojson (this may take ~30s) ...")
hex_1kha <- sf::st_read(
  file.path(PROC, "hex_geojson_with_stats", "hex_1kha_complete.geojson"),
  quiet = TRUE
)
saveRDS(hex_1kha, file.path(OUT, "hex_1kha.rds"))
message("  ✓ hex_1kha.rds (", nrow(hex_1kha), " hexagons)")

# State boundaries → WGS84 for leaflet
states_sf <- sf::st_read(
  file.path(ROOT, "data", "boundaries", "states_5070.geojson"),
  quiet = TRUE
) |>
  sf::st_transform(4326)
saveRDS(states_sf, file.path(OUT, "states.rds"))
message("  ✓ states.rds")

# Chittenden County boundary
message("  Downloading Chittenden County boundary (tigris) ...")
chittenden_boundary <- tigris::counties(state = "VT", cb = TRUE, year = 2020,
                                         progress_bar = FALSE) |>
  dplyr::filter(NAME == "Chittenden") |>
  sf::st_transform(4326)
saveRDS(chittenden_boundary, file.path(OUT, "chittenden_boundary.rds"))
message("  ✓ chittenden_boundary.rds")

# ── BLOCK 5: Chittenden TIF clips ─────────────────────────────────────────────
message("Block 5: Clipping prediction rasters to Chittenden County ...")

TIF_SRC <- file.path(ROOT, "data", "predictions", "phase4", "scenario_comparison")

# Get Chittenden in raster CRS (5070)
chitt_5070 <- tigris::counties(state = "VT", cb = TRUE, year = 2020,
                                 progress_bar = FALSE) |>
  dplyr::filter(NAME == "Chittenden") |>
  sf::st_transform(5070)

scenarios <- c("fia_only", "nefin_only", "pooled")
for (s in scenarios) {
  tif_path <- file.path(TIF_SRC, paste0("biomass_10m_", s, ".tif"))
  if (!file.exists(tif_path)) {
    warning("  ! TIF not found: ", tif_path)
    next
  }
  r         <- terra::rast(tif_path)
  chitt_v   <- terra::vect(sf::st_transform(chitt_5070, terra::crs(r)))
  r_clip    <- terra::crop(r, chitt_v, mask = TRUE)
  r_wgs84   <- terra::project(r_clip, "EPSG:4326", method = "bilinear")
  out_path  <- file.path(OUT, paste0("chittenden_biomass_", s, ".tif"))
  terra::writeRaster(r_wgs84, out_path, overwrite = TRUE)
  message("  ✓ chittenden_biomass_", s, ".tif")
}

# ── BLOCK 6: Scale + Modeling RDS files ───────────────────────────────────────
message("Block 6: Building scale and modeling RDS files ...")

scale_metrics <- read_csv(
  file.path(PROC, "recommendations", "scale_metrics_complete.csv"),
  show_col_types = FALSE
)
saveRDS(scale_metrics, file.path(OUT, "scale_metrics.rds"))
message("  ✓ scale_metrics.rds")

bootstrap_variance <- read_csv(
  file.path(PROC, "summary_statistics", "bootstrap_variance.csv"),
  show_col_types = FALSE
)
saveRDS(bootstrap_variance, file.path(OUT, "bootstrap_variance.rds"))
message("  ✓ bootstrap_variance.rds")

cv_results <- read_csv(
  file.path(PROC, "phase4_cv_results", "cv_summary.csv"),
  show_col_types = FALSE
)
saveRDS(cv_results, file.path(OUT, "cv_results.rds"))
message("  ✓ cv_results.rds")

fold_results <- read_csv(
  file.path(PROC, "phase4_cv_results", "fold_results.csv"),
  show_col_types = FALSE
)
saveRDS(fold_results, file.path(OUT, "fold_results.rds"))
message("  ✓ fold_results.rds")

test_predictions <- read_csv(
  file.path(PROC, "phase4_cv_results", "test_predictions_all_models.csv"),
  show_col_types = FALSE
)
saveRDS(test_predictions, file.path(OUT, "test_predictions.rds"))
message("  ✓ test_predictions.rds")

var_importance <- read_csv(
  file.path(PROC, "phase4_diagnostics", "variable_importance.csv"),
  show_col_types = FALSE
)
saveRDS(var_importance, file.path(OUT, "var_importance.rds"))
message("  ✓ var_importance.rds")

fuzzing_sig <- read_csv(
  file.path(PROC, "phase4_cv_results", "fuzzing_significance_tests.csv"),
  show_col_types = FALSE
)
saveRDS(fuzzing_sig, file.path(OUT, "fuzzing_significance.rds"))
message("  ✓ fuzzing_significance.rds")

fuzzing_rmse <- read_csv(
  file.path(PROC, "phase4_cv_results", "fuzzing_rmse_improvement.csv"),
  show_col_types = FALSE
)
saveRDS(fuzzing_rmse, file.path(OUT, "fuzzing_rmse.rds"))
message("  ✓ fuzzing_rmse.rds")

# ── BLOCK 7: Copy static PNGs to www/ ─────────────────────────────────────────
message("Block 7: Copying static PNGs to www/ ...")

FIG_MAIN  <- file.path(ROOT, "manuscript_figures", "main")
FIG_CV    <- file.path(ROOT, "manuscript_figures", "phase4", "spatial_cv")
FIG_PRED  <- file.path(ROOT, "data", "predictions", "phase4", "figures")

copies <- list(
  c(file.path(FIG_MAIN, "Fig1_Study_Area.png"),           file.path(WWW, "Fig1_Study_Area.png")),
  c(file.path(FIG_MAIN, "Fig3_Monte_Carlo_Uncertainty.png"), file.path(WWW, "Fig3_Monte_Carlo_Uncertainty.png")),
  c(file.path(FIG_PRED, "biomass_4panel_comprehensive.png"), file.path(WWW, "biomass_4panel.png")),
  c(file.path(FIG_CV,   "Spatial_CV_R2_boxplots.png"),    file.path(WWW, "Spatial_CV_R2_boxplots.png")),
  c(file.path(FIG_CV,   "Spatial_CV_RMSE_boxplots.png"),  file.path(WWW, "Spatial_CV_RMSE_boxplots.png"))
)

for (cp in copies) {
  if (file.exists(cp[[1]])) {
    file.copy(cp[[1]], cp[[2]], overwrite = TRUE)
    message("  ✓ ", basename(cp[[2]]))
  } else {
    warning("  ! Not found: ", cp[[1]])
  }
}

message("")
message("=== prep_app_data.R complete ===")
message("All output written to: ", OUT)
message("Static images written to: ", WWW)
