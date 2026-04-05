# ============================================================================
# FIA_NEFIN Explorer -- App Data Preparation
# ============================================================================
# Run once locally before deploying the Shiny app.
# Reads from the analysis repo and external covariate drive.
# Writes all outputs into FIA_NEFIN_explorer/data/ and www/figures/.
# The app itself never reads from outside FIA_NEFIN_explorer/.
#
# Re-running is safe -- completed outputs are skipped automatically.
# To force a full rebuild set FORCE_REBUILD <- TRUE below.
#
# Run from: FIA_NEFIN/FIA_NEFIN_explorer/
#   setwd("path/to/FIA_NEFIN/FIA_NEFIN_explorer")
#   source("data/prep_app_data.R")
# ============================================================================

# PROJ database conflict fix -- must come before any library() calls.
# PostgreSQL/PostGIS installs a stale proj.db that GDAL finds first,
# causing "cannot get output boundaries" and CRS mismatch errors.
Sys.setenv(PROJ_DATA    = "")
Sys.setenv(PROJ_LIB     = "")
Sys.setenv(PROJ_NETWORK = "OFF")

library(dplyr)
library(readr)
library(sf)
sf::sf_use_s2(FALSE) # GEOS planar geometry -- avoids s2 crossing-edge errors
library(terra)
library(tigris)

# ============================================================================
# CONFIGURATION
# ============================================================================

ANALYSIS_ROOT <- ".."
EXTERNAL_ROOT <- "D:/FIA_NEFIN/data"
APP_DATA      <- "data"
APP_FIGURES   <- "www/figures"

# Set TRUE to overwrite all existing outputs regardless of whether they exist
FORCE_REBUILD <- TRUE

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

# PROJ strings avoid EPSG database lookups that fail with the PostgreSQL
# PROJ conflict. All preprocessed rasters are EPSG:5070 (NAD83/Conus Albers).
WGS84_PROJ  <- "+proj=longlat +datum=WGS84 +no_defs"
ALBERS_PROJ <- paste0(
  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5",
  " +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Clip a raster to Chittenden County and reproject to WGS84.
# Skips if out_path already exists and FORCE_REBUILD is FALSE.
clip_to_chittenden <- function(src_path, out_path, chitt_sf,
                               method = "bilinear") {
  if (file.exists(out_path) && !FORCE_REBUILD) {
    message("  skip (exists): ", basename(out_path))
    return(invisible(out_path))
  }
  if (!file.exists(src_path)) {
    warning("Source not found, skipping: ", basename(src_path))
    return(invisible(NULL))
  }
  r <- terra::rast(src_path)
  if (is.na(terra::crs(r)) || nchar(terra::crs(r)) == 0) {
    message("  CRS missing on ", basename(src_path), " -- assuming EPSG:5070")
    terra::crs(r) <- ALBERS_PROJ
  }
  chitt_reproj <- sf::st_transform(chitt_sf, 5070)
  cv           <- terra::vect(chitt_reproj)
  r_clip       <- terra::crop(r, cv, mask = TRUE)
  r_wgs        <- terra::project(r_clip, WGS84_PROJ, method = method)
  terra::writeRaster(r_wgs, out_path, overwrite = TRUE)
  message("  saved: ", basename(out_path))
  invisible(out_path)
}

# Aggregate a full-res raster to a smaller display version for leaflet.
# Skips if _display.tif already exists and FORCE_REBUILD is FALSE.
write_display_raster <- function(full_res_path, fact = 3, fun = "mean") {
  out_path <- sub("\\.tif$", "_display.tif", full_res_path)
  if (file.exists(out_path) && !FORCE_REBUILD) {
    message("  skip display (exists): ", basename(out_path))
    return(invisible(out_path))
  }
  if (!file.exists(full_res_path)) return(invisible(NULL))
  r <- terra::rast(full_res_path)
  r_agg <- terra::aggregate(r, fact = fact, fun = fun, na.rm = TRUE)
  terra::writeRaster(r_agg, out_path, overwrite = TRUE)
  message("  display saved: ", basename(out_path))
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

# Copy a figure -- warns but continues if source missing.
# Skips if destination exists and FORCE_REBUILD is FALSE.
copy_fig <- function(from, to_name) {
  to_path <- file.path(APP_FIGURES, to_name)
  if (file.exists(to_path) && !FORCE_REBUILD) {
    message("  skip (exists): ", to_name)
    return(invisible(NULL))
  }
  if (file.exists(from)) {
    file.copy(from, to_path, overwrite = TRUE)
    message("  copied: ", to_name)
  } else {
    warning("Figure not found, skipping: ", basename(from))
  }
}

# Build and save an RDS file.
# Skips if out_path already exists and FORCE_REBUILD is FALSE.
# expr is only evaluated when the file needs to be written.
save_rds_if_needed <- function(out_path, label, expr) {
  if (file.exists(out_path) && !FORCE_REBUILD) {
    message(label, " -- skip (exists)")
    return(invisible(out_path))
  }
  result <- tryCatch(
    force(expr),
    error = function(e) {
      warning(label, " failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(result)) {
    saveRDS(result, out_path)
    message(label, " saved")
  }
  invisible(out_path)
}

# ============================================================================
# SECTION A: Chittenden County boundary
# ============================================================================
# chittenden_5070 is always loaded into memory -- sections B, C, D need it
# even when the boundary GeoJSON file already exists on disk.

message("\n-- A: Chittenden County boundary --")

chittenden_5070 <- tigris::counties(
  state = "VT", year = 2020, progress_bar = FALSE
) |>
  dplyr::filter(NAME == "Chittenden") |>
  sf::st_transform(5070)

BOUNDARY_OUT <- file.path(APP_DATA, "rasters/chittenden_boundary.geojson")

if (file.exists(BOUNDARY_OUT) && !FORCE_REBUILD) {
  message("Chittenden boundary -- skip (exists)")
} else {
  chittenden_4326 <- sf::st_transform(chittenden_5070, 4326)
  sf::st_write(chittenden_4326, BOUNDARY_OUT, delete_dsn = TRUE, quiet = TRUE)
  message("Chittenden boundary saved")
}

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

# Generate display-resolution versions for leaflet
message("  Generating display rasters for predictions...")
for (out_name in unlist(pred_map)) {
  write_display_raster(file.path(APP_DATA, "rasters", out_name))
}

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

# Generate display-resolution versions for leaflet (not needed for coarse 250m)
message("  Generating display rasters for fine covariates...")
for (out_name in unlist(fine_map)) {
  agg_fun <- if (grepl("aspect", out_name, ignore.case = TRUE)) "modal" else "mean"
  write_display_raster(file.path(COV_OUT, out_name), fun = agg_fun)
}

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
  
  if (file.exists(out_path) && !FORCE_REBUILD) {
    message("  skip (exists): ", hex_map[[src_name]])
    next
  }
  if (!file.exists(src_path)) {
    warning("GeoJSON not found, skipping: ", src_name)
    next
  }
  sf::st_read(src_path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(5070) |>
    sf::st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
    sf::st_make_valid() |>
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

save_rds_if_needed(
  file.path(APP_DATA, "plot_data.rds"),
  "plot_data.rds",
  {
    # Use augmented_with_covariates.csv for both FIA and NEFIN so that
    # NEFIN rows get real covariate values instead of NA.
    aug_raw <- safe_read_csv(
      file.path(ANALYSIS_ROOT, "data/processed/augmented_with_covariates.csv")
    )
    stopifnot(!is.null(aug_raw))

    message("  augmented columns: ", paste(names(aug_raw), collapse = ", "))

    make_plots <- function(raw, ds_filter) {
      raw |>
        dplyr::filter(dataset == ds_filter) |>
        dplyr::mutate(
          CN      = as.character(CN),
          plot_id = if (ds_filter == "FIA") as.character(PLOT) else as.character(CN),
          state   = STATE_LU[as.character(STATECD)]
        ) |>
        dplyr::select(
          CN, plot_id, dataset, state, COUNTYCD, MEASYEAR, lat, lon, biomass,
          ndvi_s2       = dplyr::any_of(c("ndvi_s2_10m", "ndvi_s2")),
          ndvi_modis    = dplyr::any_of(c("ndvi_modis_250m", "ndvi_modis")),
          temp_mean     = dplyr::any_of(c("tmean_10m", "tmean", "temp_mean")),
          precip_annual = dplyr::any_of(c("ppt_10m", "ppt", "precip_annual")),
          canopy_height = dplyr::any_of(c("canopy_height_10m", "canopy_height")),
          elevation     = dplyr::any_of(c("elevation_10m", "elevation")),
          n_trees
        )
    }

    fia_plots   <- make_plots(aug_raw, "FIA")
    nefin_plots <- make_plots(aug_raw, "NEFIN")
    
    result <- dplyr::bind_rows(fia_plots, nefin_plots)
    message("  ", nrow(result), " rows (FIA + NEFIN)")
    result
  }
)

# -- F2: uncertainty_data.rds -------------------------------------------------
message("\n-- F2: uncertainty_data.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "uncertainty_data.rds"),
  "uncertainty_data.rds",
  {
    fia_base <- safe_read_csv(
      file.path(ANALYSIS_ROOT, "data/processed/fia_complete.csv")
    )
    unc <- safe_read_csv(
      file.path(ANALYSIS_ROOT,
                "data/processed/monte_carlo/plot_uncertainty.csv")
    )
    stopifnot(!is.null(fia_base), !is.null(unc))
    
    result <- dplyr::left_join(
      fia_base |> dplyr::mutate(CN = as.character(CN)),
      unc      |> dplyr::mutate(CN = as.character(CN)),
      by = "CN"
    ) |>
      dplyr::mutate(state = STATE_LU[as.character(STATECD)])
    message("  ", nrow(result), " rows")
    result
  }
)

# -- F3: species_summary.rds --------------------------------------------------
message("\n-- F3: species_summary.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "species_summary.rds"),
  "species_summary.rds",
  {
    sp_raw <- safe_read_csv(
      file.path(ANALYSIS_ROOT,
                "data/processed/large_tree_analysis/species_summary.csv")
    )
    stopifnot(!is.null(sp_raw))
    
    tail_enrich <- safe_read_csv(
      file.path(ANALYSIS_ROOT,
                "data/processed/edge_case_analysis_species_structure/tables/species_tail_enrichment_ecdf.csv")
    )
    
    result <- if (!is.null(tail_enrich)) {
      join_col <- intersect(names(sp_raw), names(tail_enrich))
      join_col <- join_col[join_col %in% c("species_code", "SPCD")]
      if (length(join_col) > 0) {
        dplyr::left_join(sp_raw, tail_enrich, by = join_col[1])
      } else {
        message("  no species_code join key -- saving species_summary alone")
        sp_raw
      }
    } else {
      sp_raw
    }
    message("  ", nrow(result), " species")
    result
  }
)

# -- F4: tree_data.rds --------------------------------------------------------
# NEFIN TREE_PLOT_DATA.csv contains plot-level aggregates not individual trees,
# so tree_data.rds is FIA-only.

message("\n-- F4: tree_data.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "tree_data.rds"),
  "tree_data.rds",
  {
    tree_raw <- safe_read_csv(
      file.path(ANALYSIS_ROOT, "data/interim/fia/extracted/tree.csv"),
      col_select = dplyr::any_of(c("PLT_CN", "SPCD", "DIA", "STATUSCD",
                                   "plot_id", "species_code", "dbh", "status"))
    )
    stopifnot(!is.null(tree_raw))

    if ("PLT_CN" %in% names(tree_raw)) {
      tree_raw <- tree_raw |>
        dplyr::rename(plot_id = PLT_CN, species_code = SPCD,
                      dbh = DIA, status = STATUSCD)
    }

    # Load species crosswalk (SPCD -> latin_name)
    sp_map <- safe_read_csv(
      file.path(ANALYSIS_ROOT, "data/processed/fhm_species_mapping.csv")
    )
    if (!is.null(sp_map)) {
      sp_map <- sp_map |>
        dplyr::mutate(
          species_code = as.character(SPCD),
          species_name = tolower(latin_name)
        ) |>
        dplyr::select(species_code, species_name, common_name)
      message("  species mapping: ", nrow(sp_map), " entries")
    }

    tree_live <- tree_raw |>
      dplyr::filter(status == 1, !is.na(dbh), dbh > 0)

    top40 <- tree_live |>
      dplyr::count(species_code, sort = TRUE) |>
      dplyr::slice_head(n = 40) |>
      dplyr::pull(species_code)

    result <- tree_live |>
      dplyr::filter(species_code %in% top40) |>
      dplyr::mutate(
        plot_id      = as.character(plot_id),
        dataset      = "FIA",
        species_code = as.character(species_code)
      ) |>
      dplyr::select(plot_id, dataset, species_code, dbh) |>
      dplyr::slice_head(n = 500000)

    # Join species names from crosswalk
    if (!is.null(sp_map)) {
      result <- result |>
        dplyr::left_join(sp_map, by = "species_code")
      n_mapped <- sum(!is.na(result$species_name))
      message("  species name mapped: ", n_mapped, "/", nrow(result), " rows")
    }

    message("  ", nrow(result), " rows, ", length(top40), " species")
    result
  }
)

# -- F5: hex_data.rds ---------------------------------------------------------
message("\n-- F5: hex_data.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "hex_data.rds"),
  "hex_data.rds",
  {
    HEX_AGG <- file.path(ANALYSIS_ROOT, "data/processed/hex_aggregated")
    hex_files <- list.files(
      HEX_AGG, pattern = "^augmented_hex_[^_]+\\.csv$", full.names = TRUE
    )
    hex_files <- hex_files[!grepl("_filtered", hex_files)]
    stopifnot(length(hex_files) > 0)
    
    hex_list <- lapply(hex_files, function(f) {
      scale_label <- sub("^augmented_hex_", "",
                         sub("\\.csv$", "", basename(f)))
      safe_read_csv(f) |> dplyr::mutate(scale = scale_label)
    })
    result <- dplyr::bind_rows(hex_list)
    message("  ", nrow(result), " rows across ", length(hex_files), " scales")
    result
  }
)

# -- F6: scale_metrics.rds ----------------------------------------------------
message("\n-- F6: scale_metrics.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "scale_metrics.rds"),
  "scale_metrics.rds",
  {
    scale_main <- safe_read_csv(
      file.path(ANALYSIS_ROOT,
                "data/processed/recommendations/scale_metrics_complete.csv")
    )
    stopifnot(!is.null(scale_main))
    
    join_if_possible <- function(base, path, label) {
      extra <- safe_read_csv(path)
      if (is.null(extra)) return(base)
      scale_col <- intersect(names(base), names(extra))
      scale_col <- scale_col[scale_col %in% c("scale", "Scale", "hex_scale")]
      if (length(scale_col) == 0) {
        message("  no shared scale column for ", label, " -- skipping join")
        return(base)
      }
      dplyr::left_join(base, extra, by = scale_col[1])
    }
    
    result <- scale_main |>
      join_if_possible(
        file.path(ANALYSIS_ROOT,
                  "data/processed/summary_statistics/smd_by_scale.csv"),
        "smd_by_scale"
      ) |>
      join_if_possible(
        file.path(ANALYSIS_ROOT,
                  "data/processed/summary_statistics/bootstrap_variance.csv"),
        "bootstrap_variance"
      )
    message("  ", nrow(result), " rows")
    result
  }
)

# -- F7: cv_results.rds -------------------------------------------------------
message("\n-- F7: cv_results.rds --")

save_rds_if_needed(
  file.path(APP_DATA, "cv_results.rds"),
  "cv_results.rds",
  {
    CV_DIR   <- file.path(ANALYSIS_ROOT, "data/processed/phase4_cv_results")
    DIAG_DIR <- file.path(ANALYSIS_ROOT, "data/processed/phase4_diagnostics")
    
    result <- list(
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
    
    n_loaded <- sum(!sapply(result, is.null))
    message("  ", n_loaded, "/6 components loaded")
    if (n_loaded < 6) {
      missing <- names(result)[sapply(result, is.null)]
      message("  missing: ", paste(missing, collapse = ", "))
    }
    result
  }
)

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
copy_fig(file.path(FIG_ROOT, "phase4/spatial_cv/Spatial_CV_R2_boxplots.png"),
         "cv_r2_boxplots.png")
copy_fig(file.path(FIG_ROOT, "phase4/spatial_cv/Spatial_CV_RMSE_boxplots.png"),
         "cv_rmse_boxplots.png")
copy_fig(file.path(FIG_ROOT, "phase4/diagnostics/importance_fine10m.png"),
         "importance_fine.png")
copy_fig(file.path(FIG_ROOT, "phase4/diagnostics/importance_coarse250m.png"),
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

message("RDS files:    ", length(rds_files), " / 7 expected")
message("Rasters:      ", length(rasters),
        " / ", length(pred_map) + length(fine_map) + length(coarse_map),
        " expected")
message("Hex GeoJSONs: ", length(geojsons), " / 9 expected")
message("Figures:      ", length(figures), " / 9 expected")

all_app_files <- list.files(
  c(APP_DATA, APP_FIGURES), recursive = TRUE, full.names = TRUE
)
total_mb <- sum(file.info(all_app_files)$size, na.rm = TRUE) / 1e6
message("Total size:   ", round(total_mb, 1), " MB")
message("\nTo force a full rebuild: set FORCE_REBUILD <- TRUE and re-run.")
message("Run shiny::runApp() from FIA_NEFIN_explorer/ to verify the app.")