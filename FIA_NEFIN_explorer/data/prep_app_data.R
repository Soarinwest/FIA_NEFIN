# ============================================================================
# FIA_NEFIN Explorer -- App Data Preparation
# ============================================================================
# Run once locally before deploying the Shiny app.
# Reads from the analysis repo and external covariate drive.
# Writes all outputs into FIA_NEFIN_explorer/data/ and www/figures/.
# The app itself never reads from outside FIA_NEFIN_explorer/.
#
# Run from: FIA_NEFIN/FIA_NEFIN_explorer/
#   setwd("path/to/FIA_NEFIN/FIA_NEFIN_explorer")
#   source("data/prep_app_data.R")
# ============================================================================

# PROJ database conflict fix -- must come before any library() calls.
# PostgreSQL/PostGIS installs a stale proj.db that GDAL finds first,
# causing "cannot get output boundaries" and CRS mismatch errors.
# Clearing these variables forces R to use its own PROJ installation.
Sys.setenv(PROJ_DATA    = "")
Sys.setenv(PROJ_LIB     = "")
Sys.setenv(PROJ_NETWORK = "OFF")

library(dplyr)
library(readr)
library(sf)
library(terra)
library(tigris)

# ============================================================================
# PATH CONSTANTS
# Edit EXTERNAL_ROOT if your drive letter or mount point differs.
# ============================================================================

ANALYSIS_ROOT <- ".."
EXTERNAL_ROOT <- "D:/FIA_NEFIN/data"
APP_DATA      <- "data"
APP_FIGURES   <- "www/figures"

# ============================================================================
# SANITY CHECKS
# ============================================================================

stopifnot(
  "Must run from FIA_NEFIN_explorer/ -- setwd() first" =
    file.exists("app.R"),
  "Analysis repo not found at expected relative path" =
    file.exists(file.path(ANALYSIS_ROOT, "data/processed/fia_complete.csv")),
  "External covariate drive not accessible" =
    dir.exists(file.path(EXTERNAL_ROOT, "covariates/fine_10m_preprocessed"))
)
message("Path checks passed")

# ============================================================================
# DIRECTORY SETUP
# ============================================================================

dirs <- c(
  file.path(APP_DATA, "rasters"),
  file.path(APP_DATA, "rasters/covariates"),
  file.path(APP_DATA, "hex_geojsons"),
  APP_FIGURES
)
for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# GLOBAL CONSTANTS
# ============================================================================

options(tigris_use_cache = TRUE)

STATE_LU <- c(
  "9"  = "CT",
  "23" = "ME",
  "25" = "MA",
  "33" = "NH",
  "36" = "NY",
  "44" = "RI",
  "50" = "VT"
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Clip a raster to Chittenden County and reproject to EPSG:4326.
#
# chitt_sf must be an sf object in EPSG:5070.
# sf::st_transform() is used for vector reprojection because
# terra::project(SpatVector, wkt) fails on Windows when the PROJ
# database cannot resolve the full WKT string. sf uses a different
# lookup path that works reliably across platforms.
#
clip_to_chittenden <- function(src_path, out_path, chitt_sf,
                               method = "bilinear") {
  if (!file.exists(src_path)) {
    warning("Source not found, skipping: ", basename(src_path))
    return(invisible(NULL))
  }
  
  r <- terra::rast(src_path)
  
  # Reproject boundary to match raster CRS using sf (not terra::project)
  # All preprocessed rasters should be EPSG:5070; hardcode as fallback
  epsg_code <- suppressWarnings(terra::crs(r, describe = TRUE)$code)
  target_epsg <- if (!is.na(epsg_code) && nchar(epsg_code) > 0) {
    as.integer(epsg_code)
  } else {
    5070
  }
  chitt_reproj <- sf::st_transform(chitt_sf, target_epsg)
  cv <- terra::vect(chitt_reproj)
  
  r_clip <- terra::crop(r, cv, mask = TRUE)
  r_wgs  <- terra::project(r_clip, "EPSG:4326", method = method)
  terra::writeRaster(r_wgs, out_path, overwrite = TRUE)
  message("  saved: ", basename(out_path))
  invisible(out_path)
}

# Read a CSV safely -- returns NULL with a warning if file does not exist
safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) {
    warning("File not found, skipping: ", path)
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE, ...)
}

# Copy a figure file -- warns but does not stop if source missing
copy_fig <- function(from, to_name) {
  to_path <- file.path(APP_FIGURES, to_name)
  if (file.exists(from)) {
    file.copy(from, to_path, overwrite = TRUE)
    message("  copied: ", to_name)
  } else {
    warning("Figure not found, skipping: ", basename(from))
  }
}

# ============================================================================
# SECTION A: Chittenden County boundary
# ============================================================================

message("\n-- A: Chittenden County boundary --")

chittenden_5070 <- tigris::counties(
  state = "VT", year = 2020, progress_bar = FALSE
) |>
  dplyr::filter(NAME == "Chittenden") |>
  sf::st_transform(5070)

# Keep chittenden_5070 as an sf object throughout.
# terra::vect() conversion happens inside clip_to_chittenden()
# AFTER reprojection to avoid the Windows PROJ lookup error.

chittenden_4326 <- sf::st_transform(chittenden_5070, 4326)
sf::st_write(
  chittenden_4326,
  file.path(APP_DATA, "rasters/chittenden_boundary.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)
message("Chittenden boundary saved")

# ============================================================================
# SECTION B: Clip biomass prediction rasters
# ============================================================================

message("\n-- B: Clip biomass prediction rasters --")

PRED_SRC <- file.path(
  ANALYSIS_ROOT, "data/predictions/phase4/scenario_comparison"
)

pred_map <- list(
  "biomass_10m_fia_only.tif"   = "biomass_10m_fia_only_chittenden.tif",
  "biomass_10m_nefin_only.tif" = "biomass_10m_nefin_only_chittenden.tif",
  "biomass_10m_pooled.tif"     = "biomass_10m_pooled_chittenden.tif"
)

for (src_name in names(pred_map)) {
  clip_to_chittenden(
    src_path = file.path(PRED_SRC, src_name),
    out_path = file.path(APP_DATA, "rasters", pred_map[[src_name]]),
    chitt_sf = chittenden_5070
  )
}
message("Prediction rasters complete")

# ============================================================================
# SECTION C: Clip fine-scale (10m) covariate rasters
# ============================================================================

message("\n-- C: Clip fine-scale (10m) covariate rasters --")

FINE_SRC <- file.path(EXTERNAL_ROOT, "covariates/fine_10m_preprocessed")
COV_OUT  <- file.path(APP_DATA, "rasters/covariates")

fine_map <- list(
  "canopy_height_10m_2020_NE.tif" = "fine_canopy_height_chittenden.tif",
  "Elevation10m.tif"              = "fine_elevation_chittenden.tif",
  "Slope10m.tif"                  = "fine_slope_chittenden.tif",
  "Aspect10m.tif"                 = "fine_aspect_chittenden.tif",
  "S2_NDVI_10m_2020_2024.tif"     = "fine_ndvi_chittenden.tif",
  "S2_NDWI_10m_2020_2024.tif"     = "fine_ndwi_chittenden.tif",
  "S2_EVI_10m_2020_2024.tif"      = "fine_evi_chittenden.tif",
  "S2_NBR_10m_2020_2024.tif"      = "fine_nbr_chittenden.tif",
  "S2_B2_10m_2020_2024.tif"       = "fine_b2_chittenden.tif",
  "S2_B3_10m_2020_2024.tif"       = "fine_b3_chittenden.tif",
  "S2_B4_10m_2020_2024.tif"       = "fine_b4_chittenden.tif",
  "tmean.tif"                     = "fine_tmean_chittenden.tif",
  "tmin.tif"                      = "fine_tmin_chittenden.tif",
  "tmax.tif"                      = "fine_tmax_chittenden.tif",
  "ppt.tif"                       = "fine_ppt_chittenden.tif"
)

for (src_name in names(fine_map)) {
  clip_to_chittenden(
    src_path = file.path(FINE_SRC, src_name),
    out_path = file.path(COV_OUT, fine_map[[src_name]]),
    chitt_sf = chittenden_5070
  )
}
message("Fine-scale covariate rasters complete")

# ============================================================================
# SECTION D: Clip coarse-scale (250m) covariate rasters
# ============================================================================

message("\n-- D: Clip coarse-scale (250m) covariate rasters --")

COARSE_SRC <- file.path(EXTERNAL_ROOT, "covariates/coarse_250m_preprocessed")

coarse_map <- list(
  "canopy_height_250m_2020_NE.tif"   = "coarse_canopy_height_chittenden.tif",
  "elevation_250m_NE.tif"            = "coarse_elevation_chittenden.tif",
  "slope_250m_NE.tif"                = "coarse_slope_chittenden.tif",
  "aspect_250m_NE.tif"               = "coarse_aspect_chittenden.tif",
  "MODIS_NDVI_250m_2020_2024_NE.tif" = "coarse_ndvi_chittenden.tif",
  "MODIS_EVI_250m_2020_2024_NE.tif"  = "coarse_evi_chittenden.tif",
  "MODIS_NBR_250m_2020_2024_NE.tif"  = "coarse_nbr_chittenden.tif",
  "MODIS_NDWI_250m_2020_2024_NE.tif" = "coarse_ndwi_chittenden.tif",
  "MODIS_BLUE_250m_2020_2024_NE.tif" = "coarse_blue_chittenden.tif",
  "MODIS_GREEN_250m_2020_2024_NE.tif"= "coarse_green_chittenden.tif",
  "MODIS_RED_250m_2020_2024_NE.tif"  = "coarse_red_chittenden.tif",
  "MODIS_NIR_250m_2020_2024_NE.tif"  = "coarse_nir_chittenden.tif",
  "MODIS_SWIR1_250m_2020_2024_NE.tif"= "coarse_swir1_chittenden.tif",
  "tmean.tif"                        = "coarse_tmean_chittenden.tif",
  "tmin.tif"                         = "coarse_tmin_chittenden.tif",
  "tmax.tif"                         = "coarse_tmax_chittenden.tif",
  "ppt.tif"                          = "coarse_ppt_chittenden.tif"
)

for (src_name in names(coarse_map)) {
  clip_to_chittenden(
    src_path = file.path(COARSE_SRC, src_name),
    out_path = file.path(COV_OUT, coarse_map[[src_name]]),
    chitt_sf = chittenden_5070
  )
}
message("Coarse-scale covariate rasters complete")

# ============================================================================
# SECTION E: Process and copy hex GeoJSONs
# ============================================================================

message("\n-- E: Process hex GeoJSONs --")

HEX_SRC <- file.path(ANALYSIS_ROOT, "data/processed/hex_geojson_with_stats")
HEX_OUT <- file.path(APP_DATA, "hex_geojsons")

hex_map <- list(
  "hex_100ha_complete.geojson"  = "hex_100ha.geojson",
  "hex_500ha_complete.geojson"  = "hex_500ha.geojson",
  "hex_1kha_complete.geojson"   = "hex_1kha.geojson",
  "hex_2_4kha_complete.geojson" = "hex_2_4kha.geojson",
  "hex_5kha_complete.geojson"   = "hex_5kha.geojson",
  "hex_10kha_complete.geojson"  = "hex_10kha.geojson",
  "hex_50kha_complete.geojson"  = "hex_50kha.geojson",
  "hex_64kha_complete.geojson"  = "hex_64kha.geojson",
  "hex_100kha_complete.geojson" = "hex_100kha.geojson"
)

for (src_name in names(hex_map)) {
  src_path <- file.path(HEX_SRC, src_name)
  out_path <- file.path(HEX_OUT, hex_map[[src_name]])
  if (!file.exists(src_path)) {
    warning("GeoJSON not found, skipping: ", src_name)
    next
  }
  sf::st_read(src_path, quiet = TRUE) |>
    sf::st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
    sf::st_transform(4326) |>
    sf::st_write(out_path, delete_dsn = TRUE, quiet = TRUE)
  message("  saved: ", hex_map[[src_name]])
}
message("Hex GeoJSONs complete")

# ============================================================================
# SECTION F: Build tabular RDS files
# ============================================================================

# -- F1: plot_data.rds --------------------------------------------------------
message("\n-- F1: plot_data.rds --")

# baseline_with_covariates.csv contains FIA plots with extracted covariates.
# Column names confirmed from data inspection -- update if they differ.
fia_plots <- safe_read_csv(
  file.path(ANALYSIS_ROOT, "data/processed/baseline_with_covariates.csv")
)

if (!is.null(fia_plots)) {
  # Report actual column names so mismatches are obvious
  message("  baseline_with_covariates.csv columns: ",
          paste(names(fia_plots), collapse = ", "))
  
  fia_plots <- fia_plots |>
    dplyr::filter(dataset == "FIA") |>
    dplyr::mutate(
      plot_id = as.character(PLOT),
      state   = STATE_LU[as.character(STATECD)]
    ) |>
    dplyr::select(
      CN, plot_id, dataset, state, COUNTYCD, MEASYEAR,
      lat, lon, biomass,
      ndvi_s2       = dplyr::any_of(c("ndvi_s2_10m", "ndvi_s2",
                                      "S2_NDVI", "ndvi_sentinel")),
      ndvi_modis    = dplyr::any_of(c("ndvi_modis_250m", "ndvi_modis",
                                      "MODIS_NDVI", "ndvi_modis_mean")),
      temp_mean     = dplyr::any_of(c("tmean_10m", "tmean", "temp_mean",
                                      "tmean_daymet")),
      precip_annual = dplyr::any_of(c("ppt_10m", "ppt", "precip_annual",
                                      "ppt_daymet")),
      canopy_height = dplyr::any_of(c("canopy_height_10m", "canopy_height",
                                      "ETH_canopy_height")),
      elevation     = dplyr::any_of(c("elevation_10m", "elevation",
                                      "Elevation10m")),
      n_trees
    )
}

nefin_raw <- safe_read_csv(
  file.path(ANALYSIS_ROOT, "data/processed/nefin_complete.csv")
)

if (!is.null(nefin_raw)) {
  message("  nefin_complete.csv columns: ",
          paste(names(nefin_raw), collapse = ", "))
  
  nefin_plots <- nefin_raw |>
    dplyr::mutate(
      plot_id       = as.character(dplyr::coalesce(
        as.character(CN), as.character(PLOT))),
      state         = STATE_LU[as.character(STATECD)],
      COUNTYCD      = NA_integer_,
      n_trees       = NA_integer_,
      ndvi_s2       = NA_real_,
      ndvi_modis    = NA_real_,
      temp_mean     = NA_real_,
      precip_annual = NA_real_,
      canopy_height = NA_real_,
      elevation     = NA_real_
    ) |>
    dplyr::select(
      CN, plot_id, dataset, state, COUNTYCD, MEASYEAR,
      lat, lon, biomass,
      ndvi_s2, ndvi_modis, temp_mean, precip_annual,
      canopy_height, elevation, n_trees
    )
}

plot_data <- dplyr::bind_rows(
  if (exists("fia_plots") && !is.null(fia_plots)) fia_plots else NULL,
  if (exists("nefin_plots") && !is.null(nefin_plots)) nefin_plots else NULL
)
saveRDS(plot_data, file.path(APP_DATA, "plot_data.rds"))
message("plot_data.rds saved (", nrow(plot_data), " rows)")

# -- F2: uncertainty_data.rds -------------------------------------------------
message("\n-- F2: uncertainty_data.rds --")

fia_base <- safe_read_csv(
  file.path(ANALYSIS_ROOT, "data/processed/fia_complete.csv")
)
unc <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/monte_carlo/plot_uncertainty.csv")
)

if (!is.null(fia_base) && !is.null(unc)) {
  uncertainty_data <- dplyr::left_join(fia_base, unc, by = "CN") |>
    dplyr::mutate(state = STATE_LU[as.character(STATECD)])
  saveRDS(uncertainty_data, file.path(APP_DATA, "uncertainty_data.rds"))
  message("uncertainty_data.rds saved (", nrow(uncertainty_data), " rows)")
} else {
  warning("Could not build uncertainty_data.rds -- source files missing")
}

# -- F3: species_summary.rds --------------------------------------------------
message("\n-- F3: species_summary.rds --")

species_raw <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/large_tree_analysis/species_summary.csv")
)
tail_enrich <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/edge_case_analysis_species_structure/tables/species_tail_enrichment_ecdf.csv")
)

if (!is.null(species_raw)) {
  species_summary <- if (!is.null(tail_enrich)) {
    # Join if both files exist and share a species_code column
    join_col <- intersect(names(species_raw), names(tail_enrich))
    join_col <- join_col[join_col %in% c("species_code", "SPCD", "spcd")]
    if (length(join_col) > 0) {
      dplyr::left_join(species_raw, tail_enrich, by = join_col[1])
    } else {
      message("  species_code join key not found -- saving species_summary alone")
      species_raw
    }
  } else {
    species_raw
  }
  saveRDS(species_summary, file.path(APP_DATA, "species_summary.rds"))
  message("species_summary.rds saved (", nrow(species_summary), " rows)")
}

# -- F4: tree_data.rds --------------------------------------------------------
message("\n-- F4: tree_data.rds --")

# FIA individual tree records. NEFIN TREE_PLOT_DATA.csv contains
# plot-level aggregates (BAPH, QMD, TPH) not individual trees,
# so tree_data.rds is FIA-only.

tree_raw <- safe_read_csv(
  file.path(ANALYSIS_ROOT, "data/interim/fia/extracted/tree.csv"),
  col_select = dplyr::any_of(c("PLT_CN", "SPCD", "DIA", "STATUSCD",
                               "plot_id", "species_code", "dbh", "status"))
)

if (!is.null(tree_raw)) {
  # Normalise column names -- handle both raw FIA and pre-processed versions
  if ("PLT_CN" %in% names(tree_raw)) {
    tree_raw <- tree_raw |>
      dplyr::rename(plot_id = PLT_CN, species_code = SPCD,
                    dbh = DIA, status = STATUSCD)
  }
  
  # Filter to live trees (STATUSCD == 1 in FIA)
  tree_live <- tree_raw |>
    dplyr::filter(status == 1, !is.na(dbh), dbh > 0)
  
  # Top 40 species by count
  top40 <- tree_live |>
    dplyr::count(species_code, sort = TRUE) |>
    dplyr::slice_head(n = 40) |>
    dplyr::pull(species_code)
  
  tree_data <- tree_live |>
    dplyr::filter(species_code %in% top40) |>
    dplyr::mutate(
      plot_id      = as.character(plot_id),
      dataset      = "FIA",
      species_code = as.character(species_code)
    ) |>
    dplyr::select(plot_id, dataset, species_code, dbh) |>
    dplyr::slice_head(n = 500000)
  
  saveRDS(tree_data, file.path(APP_DATA, "tree_data.rds"))
  message("tree_data.rds saved (", nrow(tree_data),
          " rows, ", length(top40), " species)")
}

# -- F5: hex_data.rds ---------------------------------------------------------
message("\n-- F5: hex_data.rds --")

HEX_AGG <- file.path(ANALYSIS_ROOT, "data/processed/hex_aggregated")

# Augmented files only (no _filtered variants)
hex_files <- list.files(
  HEX_AGG,
  pattern = "^augmented_hex_[^_]+\\.csv$",
  full.names = TRUE
)
hex_files <- hex_files[!grepl("_filtered", hex_files)]

if (length(hex_files) == 0) {
  warning("No hex aggregated files found at: ", HEX_AGG)
} else {
  hex_list <- lapply(hex_files, function(f) {
    scale_label <- sub("^augmented_hex_", "",
                       sub("\\.csv$", "", basename(f)))
    safe_read_csv(f) |>
      dplyr::mutate(scale = scale_label)
  })
  hex_data <- dplyr::bind_rows(hex_list)
  saveRDS(hex_data, file.path(APP_DATA, "hex_data.rds"))
  message("hex_data.rds saved (", nrow(hex_data), " rows, ",
          length(hex_files), " scales)")
}

# -- F6: scale_metrics.rds ----------------------------------------------------
message("\n-- F6: scale_metrics.rds --")

scale_main <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/recommendations/scale_metrics_complete.csv")
)
scale_smd <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/summary_statistics/smd_by_scale.csv")
)
scale_boot <- safe_read_csv(
  file.path(ANALYSIS_ROOT,
            "data/processed/summary_statistics/bootstrap_variance.csv")
)

if (!is.null(scale_main)) {
  scale_metrics <- scale_main
  
  # Attempt joins if companion files exist and share a scale column
  join_candidate <- function(base, extra, files_label) {
    if (is.null(extra)) return(base)
    shared_cols <- intersect(names(base), names(extra))
    scale_col <- shared_cols[shared_cols %in% c("scale", "Scale", "hex_scale")]
    if (length(scale_col) == 0) {
      message("  no shared scale column for ", files_label, " -- skipping join")
      return(base)
    }
    dplyr::left_join(base, extra, by = scale_col[1])
  }
  
  scale_metrics <- join_candidate(scale_metrics, scale_smd, "smd_by_scale")
  scale_metrics <- join_candidate(scale_metrics, scale_boot, "bootstrap_variance")
  
  saveRDS(scale_metrics, file.path(APP_DATA, "scale_metrics.rds"))
  message("scale_metrics.rds saved (", nrow(scale_metrics), " rows)")
}

# -- F7: cv_results.rds -------------------------------------------------------
message("\n-- F7: cv_results.rds --")

CV_DIR  <- file.path(ANALYSIS_ROOT, "data/processed/phase4_cv_results")
DIAG_DIR <- file.path(ANALYSIS_ROOT, "data/processed/phase4_diagnostics")

cv_results <- list(
  summary      = safe_read_csv(file.path(CV_DIR, "cv_summary.csv")),
  folds        = safe_read_csv(file.path(CV_DIR, "fold_results.csv")),
  test_preds   = safe_read_csv(
    file.path(CV_DIR, "test_predictions_all_models.csv")),
  fuzzing      = safe_read_csv(
    file.path(CV_DIR, "fuzzing_impact_summary.csv")),
  significance = safe_read_csv(
    file.path(CV_DIR, "fuzzing_significance_tests.csv")),
  importance   = safe_read_csv(
    file.path(DIAG_DIR, "variable_importance.csv"))
)

n_loaded <- sum(!sapply(cv_results, is.null))
saveRDS(cv_results, file.path(APP_DATA, "cv_results.rds"))
message("cv_results.rds saved (", n_loaded, "/6 components loaded)")
if (n_loaded < 6) {
  missing <- names(cv_results)[sapply(cv_results, is.null)]
  message("  missing components: ", paste(missing, collapse = ", "))
}

# ============================================================================
# SECTION G: Copy manuscript figures
# ============================================================================

message("\n-- G: Copy manuscript figures --")

FIG_ROOT <- file.path(ANALYSIS_ROOT, "manuscript_figures")

copy_fig(file.path(FIG_ROOT, "main/Fig1_Study_Area.png"),
         "Fig1_Study_Area.png")
copy_fig(file.path(FIG_ROOT, "main/Fig3_Monte_Carlo_Uncertainty.png"),
         "Fig3_Monte_Carlo.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/predictions/map_fine_rf_fine_scale_10m_pooled.png"),
         "pred_fine_pooled.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/predictions/map_coarse_rf_coarse_scale_250m_pooled.png"),
         "pred_coarse_pooled.png")
copy_fig(file.path(FIG_ROOT,
                   paste0("phase4/predictions/map_abs_difference_rf_fine_scale_10m",
                          "_pooled_vs_rf_coarse_scale_250m_pooled.png")),
         "pred_abs_diff.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/spatial_cv/Spatial_CV_R2_boxplots.png"),
         "cv_r2_boxplots.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/spatial_cv/Spatial_CV_RMSE_boxplots.png"),
         "cv_rmse_boxplots.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/diagnostics/importance_fine10m.png"),
         "importance_fine.png")
copy_fig(file.path(FIG_ROOT,
                   "phase4/diagnostics/importance_coarse250m.png"),
         "importance_coarse.png")

message("Figure copy complete")

# ============================================================================
# SECTION H: Final summary
# ============================================================================

message("\n==========================================")
message("  prep_app_data.R complete")
message("==========================================")

rds_files <- list.files(APP_DATA, pattern = "\\.rds$", recursive = FALSE)
rasters   <- list.files(file.path(APP_DATA, "rasters"),
                        pattern = "\\.tif$", recursive = TRUE)
geojsons  <- list.files(file.path(APP_DATA, "hex_geojsons"),
                        pattern = "\\.geojson$")
figures   <- list.files(APP_FIGURES, pattern = "\\.png$")

message("RDS files:      ", length(rds_files))
message("Rasters:        ", length(rasters))
message("Hex GeoJSONs:   ", length(geojsons))
message("Figures copied: ", length(figures))

all_app_files <- list.files(
  c(APP_DATA, APP_FIGURES),
  recursive = TRUE, full.names = TRUE
)
total_mb <- sum(file.info(all_app_files)$size, na.rm = TRUE) / 1e6
message("Total app data: ", round(total_mb, 1), " MB")

# Flag anything obviously wrong
expected_rasters <- length(pred_map) + length(fine_map) + length(coarse_map)
if (length(rasters) < expected_rasters) {
  message("WARNING: expected ", expected_rasters,
          " rasters but only found ", length(rasters),
          " -- check warnings above for missing source files")
}
if (length(rds_files) < 7) {
  message("WARNING: expected 7 RDS files but only found ", length(rds_files))
}

message("\nRun shiny::runApp() from FIA_NEFIN_explorer/ to verify the app.")