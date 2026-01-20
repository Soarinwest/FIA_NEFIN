# =============================================================================
# Spatial Utility Functions
# =============================================================================
# Functions for coordinate transformations, spatial operations, and hex grids
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

library(sf)
library(dplyr)

#' Transform coordinates to analysis CRS
#'
#' @param df Data frame with lat/lon columns
#' @param lat_col Name of latitude column
#' @param lon_col Name of longitude column
#' @param from_crs Source CRS (default: 4326 WGS84)
#' @param to_crs Target CRS (default: CONFIG$crs_analysis)
#' @return sf object in target CRS
coords_to_sf <- function(df, lat_col = "lat", lon_col = "lon", 
                         from_crs = 4326, to_crs = NULL) {
  
  if (is.null(to_crs)) {
    to_crs <- CONFIG$crs_analysis
  }
  
  # Check columns exist
  if (!lat_col %in% names(df)) stop("Column not found: ", lat_col)
  if (!lon_col %in% names(df)) stop("Column not found: ", lon_col)
  
  # Filter to valid coordinates
  df_valid <- df %>%
    filter(
      !is.na(.data[[lat_col]]),
      !is.na(.data[[lon_col]]),
      is.finite(.data[[lat_col]]),
      is.finite(.data[[lon_col]])
    )
  
  if (nrow(df_valid) < nrow(df)) {
    warning("Removed ", nrow(df) - nrow(df_valid), " rows with invalid coordinates")
  }
  
  # Create sf object
  df_sf <- st_as_sf(
    df_valid,
    coords = c(lon_col, lat_col),
    crs = from_crs,
    remove = FALSE
  )
  
  # Transform if needed
  if (from_crs != to_crs) {
    df_sf <- st_transform(df_sf, crs = to_crs)
  }
  
  df_sf
}

#' Assign plots to hexagons via spatial join
#'
#' @param plots_sf sf object with plot points
#' @param hex_sf sf object with hexagon polygons
#' @param hex_id_col Name of hex ID column (default: "hex_id")
#' @return Data frame with plot data and hex assignments
assign_to_hexagons <- function(plots_sf, hex_sf, hex_id_col = "hex_id") {
  
  # Ensure hex_id column exists
  if (!hex_id_col %in% names(hex_sf)) {
    if ("ID" %in% names(hex_sf)) {
      hex_sf <- hex_sf %>% rename(hex_id = ID)
    } else if ("OBJECTID" %in% names(hex_sf)) {
      hex_sf <- hex_sf %>% rename(hex_id = OBJECTID)
    } else {
      # Create sequential IDs
      hex_sf$hex_id <- seq_len(nrow(hex_sf))
    }
  }
  
  # Ensure CRS match
  if (st_crs(plots_sf) != st_crs(hex_sf)) {
    hex_sf <- st_transform(hex_sf, st_crs(plots_sf))
  }
  
  # Spatial join
  joined <- st_join(
    plots_sf,
    hex_sf %>% select(hex_id = all_of(hex_id_col)),
    join = st_intersects,
    left = TRUE
  )
  
  # Drop geometry and return data frame
  result <- st_drop_geometry(joined)
  
  # Report assignment statistics
  n_assigned <- sum(!is.na(result$hex_id))
  pct_assigned <- 100 * n_assigned / nrow(result)
  
  message(sprintf("  Assigned %d / %d plots (%.1f%%)", 
                  n_assigned, nrow(result), pct_assigned))
  
  result
}

#' Load and prepare hexagon grid
#'
#' @param hex_path Path to hexagon grid file
#' @param target_crs Target CRS (default: CONFIG$crs_analysis)
#' @return sf object with hexagons
load_hex_grid <- function(hex_path, target_crs = NULL) {
  
  if (is.null(target_crs)) {
    target_crs <- CONFIG$crs_analysis
  }
  
  if (!file.exists(hex_path)) {
    stop("Hex grid not found: ", hex_path)
  }
  
  # Load hex grid
  hex_sf <- st_read(hex_path, quiet = TRUE)
  
  # Clean geometry
  hex_sf <- st_make_valid(hex_sf)
  hex_sf <- st_zm(hex_sf, drop = TRUE, what = "ZM")
  
  # Transform to target CRS
  hex_sf <- st_transform(hex_sf, crs = target_crs)
  st_crs(hex_sf) <- target_crs
  
  # Ensure hex_id exists
  if (!"hex_id" %in% names(hex_sf)) {
    if ("ID" %in% names(hex_sf)) {
      hex_sf <- hex_sf %>% rename(hex_id = ID)
    } else if ("OBJECTID" %in% names(hex_sf)) {
      hex_sf <- hex_sf %>% rename(hex_id = OBJECTID)
    } else {
      hex_sf$hex_id <- seq_len(nrow(hex_sf))
    }
  }
  
  # Standardize hex_id as character
  hex_sf$hex_id <- as.character(hex_sf$hex_id)
  
  message("Loaded ", nrow(hex_sf), " hexagons from ", basename(hex_path))
  
  hex_sf
}

#' Calculate hexagon centroids
#'
#' @param hex_sf sf object with hexagons
#' @return Data frame with hex_id, centroid_lon, centroid_lat
hex_centroids <- function(hex_sf) {
  
  centroids <- st_centroid(hex_sf)
  coords <- st_coordinates(centroids)
  
  data.frame(
    hex_id = hex_sf$hex_id,
    centroid_lon = coords[, "X"],
    centroid_lat = coords[, "Y"]
  )
}

#' Validate spatial data
#'
#' @param sf_obj sf object to validate
#' @param name Name for error messages
#' @return TRUE if valid (stops on error)
validate_spatial <- function(sf_obj, name = "spatial object") {
  
  # Check it's an sf object
  if (!"sf" %in% class(sf_obj)) {
    stop(name, " is not an sf object")
  }
  
  # Check CRS is defined
  if (is.na(st_crs(sf_obj))) {
    stop(name, " has undefined CRS")
  }
  
  # Check for invalid geometries
  invalid <- !st_is_valid(sf_obj)
  if (any(invalid)) {
    warning(name, " has ", sum(invalid), " invalid geometries")
  }
  
  # Check for empty geometries
  empty <- st_is_empty(sf_obj)
  if (any(empty)) {
    warning(name, " has ", sum(empty), " empty geometries")
  }
  
  invisible(TRUE)
}

#' Calculate distance matrix between points
#'
#' @param sf_obj sf object with points
#' @param max_dist Maximum distance to compute (m), NULL for all
#' @return Distance matrix (meters)
point_distance_matrix <- function(sf_obj, max_dist = NULL) {
  
  if (!"sf" %in% class(sf_obj)) {
    stop("Input must be sf object")
  }
  
  # Get distance matrix
  dist_mat <- st_distance(sf_obj)
  
  # Convert to numeric matrix (drops units)
  dist_mat <- as.numeric(dist_mat)
  dist_mat <- matrix(dist_mat, nrow = nrow(sf_obj))
  
  # Filter by max distance if specified
  if (!is.null(max_dist)) {
    dist_mat[dist_mat > max_dist] <- NA
  }
  
  dist_mat
}

#' Buffer points by distance
#'
#' @param sf_obj sf object with points
#' @param buffer_m Buffer distance (meters)
#' @return sf object with buffered polygons
buffer_points <- function(sf_obj, buffer_m) {
  
  if (!"sf" %in% class(sf_obj)) {
    stop("Input must be sf object")
  }
  
  # Check units are meters
  crs_units <- st_crs(sf_obj)$units_gdal
  if (!is.null(crs_units) && crs_units != "metre") {
    warning("CRS units are not meters, buffer may be incorrect")
  }
  
  # Create buffer
  buffered <- st_buffer(sf_obj, dist = buffer_m)
  
  buffered
}

#' Check if points fall within polygons
#'
#' @param points_sf sf object with points
#' @param poly_sf sf object with polygons
#' @return Logical vector (TRUE if point in any polygon)
points_in_polygons <- function(points_sf, poly_sf) {
  
  # Ensure CRS match
  if (st_crs(points_sf) != st_crs(poly_sf)) {
    poly_sf <- st_transform(poly_sf, st_crs(points_sf))
  }
  
  # Spatial intersection
  intersects <- st_intersects(points_sf, poly_sf, sparse = FALSE)
  
  # Return TRUE if point intersects any polygon
  apply(intersects, 1, any)
}

#' Extract coordinates from sf object
#'
#' @param sf_obj sf object with point geometry
#' @param crs_out Output CRS (default: 4326 WGS84)
#' @return Data frame with lon, lat columns
extract_coordinates <- function(sf_obj, crs_out = 4326) {
  
  if (!"sf" %in% class(sf_obj)) {
    stop("Input must be sf object")
  }
  
  # Transform if needed
  if (st_crs(sf_obj)$epsg != crs_out) {
    sf_obj <- st_transform(sf_obj, crs = crs_out)
  }
  
  # Extract coordinates
  coords <- st_coordinates(sf_obj)
  
  data.frame(
    lon = coords[, "X"],
    lat = coords[, "Y"]
  )
}

#' Calculate area of polygons
#'
#' @param sf_obj sf object with polygon geometry
#' @param units Output units ("ha", "km2", "m2")
#' @return Vector of areas
calculate_area <- function(sf_obj, units = "ha") {
  
  if (!"sf" %in% class(sf_obj)) {
    stop("Input must be sf object")
  }
  
  # Calculate area (returns units object)
  areas <- st_area(sf_obj)
  
  # Convert units
  areas_numeric <- as.numeric(areas)
  
  if (units == "ha") {
    areas_numeric <- areas_numeric / 10000  # m² to ha
  } else if (units == "km2") {
    areas_numeric <- areas_numeric / 1e6    # m² to km²
  } else if (units != "m2") {
    stop("Unknown units: ", units)
  }
  
  areas_numeric
}

#' Turn off spherical geometry for faster processing
#'
#' @return Previous s2 setting (to restore later)
disable_s2 <- function() {
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  
  if (old_s2) {
    message("Disabled spherical geometry (s2) for faster processing")
  }
  
  invisible(old_s2)
}

#' Restore s2 setting
#'
#' @param old_setting Previous s2 setting from disable_s2()
restore_s2 <- function(old_setting) {
  sf_use_s2(old_setting)
  invisible(NULL)
}
