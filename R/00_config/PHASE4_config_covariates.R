# =============================================================================
# PHASE 4: COVARIATE CONFIGURATION
# =============================================================================
# Central configuration for all covariates used in predictive modeling
# 
# TO ADD NEW COVARIATES:
# 1. Add layer info to COVARIATES list below
# 2. Process raster and save to specified path
# 3. Re-run extraction: PHASE4_02_extract_covariates.R
# 4. Models automatically use new covariates!
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

# =============================================================================
# COVARIATE DEFINITIONS
# =============================================================================

COVARIATES <- list(
  
  # ---------------------------------------------------------------------------
  # SPECTRAL - SENTINEL-2 (10m resolution)
  # ---------------------------------------------------------------------------
  
  ndvi_s2 = list(
    name = "ndvi_s2",
    display_name = "NDVI (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_NDVI_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = TRUE,  # Set to FALSE to exclude from models
    notes = "Enhanced vegetation index, 10m resolution, 2020-2024 mean"
  ),
  
  # PLACEHOLDER - Will activate when data ready
  evi_s2 = list(
    name = "evi_s2",
    display_name = "EVI (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_EVI_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = FALSE,  # Set to TRUE when raster ready
    notes = "Enhanced Vegetation Index, better in dense forests"
  ),
  
  nbr_s2 = list(
    name = "nbr_s2",
    display_name = "NBR (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_NBR_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = FALSE,
    notes = "Normalized Burn Ratio, moisture/structure indicator"
  ),
  
  ndwi_s2 = list(
    name = "ndwi_s2",
    display_name = "NDWI (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_NDWI_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = FALSE,
    notes = "Normalized Difference Water Index"
  ),
  
  red_s2 = list(
    name = "red_s2",
    display_name = "Red Band (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_B4_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = FALSE,
    notes = "Red band (B4), raw reflectance"
  ),
  
  nir_s2 = list(
    name = "nir_s2",
    display_name = "NIR Band (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_B8_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = FALSE,
    notes = "Near-infrared band (B8)"
  ),
  
  swir1_s2 = list(
    name = "swir1_s2",
    display_name = "SWIR1 Band (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_B11_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = FALSE,
    notes = "Short-wave infrared band (B11)"
  ),
  
  ndvi_sd_s2 = list(
    name = "ndvi_sd_s2",
    display_name = "NDVI Variability (Sentinel-2)",
    path = "data/raw/ndvi/s2/S2_NDVI_SD_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "variability",
    scale = "fine",
    active = FALSE,
    notes = "NDVI standard deviation, disturbance/phenology indicator"
  ),
  
  # ---------------------------------------------------------------------------
  # SPECTRAL - MODIS (250m resolution)
  # ---------------------------------------------------------------------------
  
  ndvi_modis = list(
    name = "ndvi_modis",
    display_name = "NDVI (MODIS)",
    path = "data/raw/ndvi/modis/MODIS_NDVI_5yr_blocked_2020_2024.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS NDVI, 250m resolution, 2020-2024 mean"
  ),
  
  evi_modis = list(
    name = "evi_modis",
    display_name = "EVI (MODIS)",
    path = "data/raw/ndvi/modis/MODIS_EVI_250m_2020_2024.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = FALSE,
    notes = "MODIS Enhanced Vegetation Index"
  ),
  
  nbr_modis = list(
    name = "nbr_modis",
    display_name = "NBR (MODIS)",
    path = "data/raw/ndvi/modis/MODIS_NBR_250m_2020_2024.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = FALSE,
    notes = "MODIS Normalized Burn Ratio"
  ),
  
  # ---------------------------------------------------------------------------
  # TOPOGRAPHY (DEM-derived, 10m native)
  # ---------------------------------------------------------------------------
  
  elevation = list(
    name = "elevation",
    display_name = "Elevation",
    path = "data/raw/DEM/Elevation10m.tif",
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,  
    notes = "Elevation above sea level (meters)"
  ),
  
  slope = list(
    name = "slope",
    display_name = "Slope",
    path = "data/raw/DEM/Slope10m.tif",
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,  
    notes = "Slope in degrees"
  ),
  
  aspect = list(
    name = "aspect",
    display_name = "Aspect",
    path = "data/raw/DEM/Aspect10m.tif",
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,  
    notes = "Aspect in degrees (0-360)"
  ),
  
  tpi = list(
    name = "tpi",
    display_name = "Topographic Position Index",
    path = "data/raw/dem/tpi_10m.tif",
    resolution = "10m",
    type = "topography",
    category = "terrain",
    scale = "fine",
    active = FALSE,
    notes = "TPI, ridge vs valley position"
  ),
  
  # ---------------------------------------------------------------------------
  # CLIMATE (PRISM, 4km resolution)
  # ---------------------------------------------------------------------------
  
  tmean = list(
    name = "tmean",
    display_name = "Mean Temperature",
    path = "data/raw/prism/prism_tmean_ne_2020_2024.tif",
    resolution = "4km",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = TRUE,
    notes = "PRISM mean annual temperature, 2020-2024"
  ),
  
  ppt = list(
    name = "ppt",
    display_name = "Precipitation",
    path = "data/raw/prism/prism_ppt_ne_2020_2024.tif",
    resolution = "4km",
    type = "climate",
    category = "precipitation",
    scale = "coarse",
    active = TRUE,
    notes = "PRISM total annual precipitation, 2020-2024"
  ),
  
  # ---------------------------------------------------------------------------
  # LAND SURFACE TEMPERATURE (MODIS, 1km)
  # ---------------------------------------------------------------------------
  
  lst_day = list(
    name = "lst_day",
    display_name = "LST Day",
    path = "data/raw/lst/MODIS_LST_day_1km_2020_2024.tif",
    resolution = "1km",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = FALSE,
    notes = "MODIS daytime land surface temperature"
  ),
  
  lst_night = list(
    name = "lst_night",
    display_name = "LST Night",
    path = "data/raw/lst/MODIS_LST_night_1km_2020_2024.tif",
    resolution = "1km",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = FALSE,
    notes = "MODIS nighttime land surface temperature"
  )
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Get list of active covariates
get_active_covariates <- function() {
  active <- Filter(function(x) x$active, COVARIATES)
  return(active)
}

# Get active covariate names
get_active_names <- function() {
  active <- get_active_covariates()
  return(sapply(active, function(x) x$name))
}

# Get covariates by scale
get_covariates_by_scale <- function(scale = "fine") {
  covs <- Filter(function(x) x$active && x$scale == scale, COVARIATES)
  return(covs)
}

# Get covariates by type
get_covariates_by_type <- function(type = "spectral") {
  covs <- Filter(function(x) x$active && x$type == type, COVARIATES)
  return(covs)
}

# Check which covariates have data available
check_covariate_availability <- function() {
  results <- data.frame(
    name = character(),
    active = logical(),
    exists = logical(),
    path = character(),
    stringsAsFactors = FALSE
  )
  
  for (cov_name in names(COVARIATES)) {
    cov <- COVARIATES[[cov_name]]
    file_exists <- file.exists(cov$path)
    
    results <- rbind(results, data.frame(
      name = cov$name,
      display_name = cov$display_name,
      active = cov$active,
      exists = file_exists,
      path = cov$path,
      resolution = cov$resolution,
      type = cov$type,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

# Print summary of active covariates
print_active_covariates <- function() {
  active <- get_active_covariates()
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("  ACTIVE COVARIATES FOR MODELING\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Group by type
  types <- unique(sapply(active, function(x) x$type))
  
  for (type in types) {
    type_covs <- Filter(function(x) x$type == type, active)
    cat(toupper(type), "covariates:\n")
    
    for (cov in type_covs) {
      status <- if(file.exists(cov$path)) "✓" else "⚠"
      cat(sprintf("  %s %s (%s, %s)\n", 
                  status, cov$display_name, cov$resolution, cov$scale))
    }
    cat("\n")
  }
  
  cat("Total active covariates:", length(active), "\n\n")
}

# =============================================================================
# COVARIATE SETS FOR DIFFERENT SCALES
# =============================================================================

# Fine-scale model (10m Sentinel-2 + topography)
COVARIATE_SET_FINE <- function() {
  c("ndvi_s2", "evi_s2", "nbr_s2", "ndwi_s2", 
    "elevation", "slope", "aspect",
    "tmean", "ppt")
}

# Coarse-scale model (250m MODIS + aggregated topography)
COVARIATE_SET_COARSE <- function() {
  c("ndvi_modis", "evi_modis", "nbr_modis",
    "elevation", "slope", "aspect",
    "tmean", "ppt")
}

# Minimal set (current available data)
COVARIATE_SET_MINIMAL <- function() {
  c("ndvi_s2", "ndvi_modis", "tmean", "ppt")
}

# =============================================================================
# EXPORT
# =============================================================================

cat("\n✓ Covariate configuration loaded\n")
cat("  Total covariates defined:", length(COVARIATES), "\n")
cat("  Currently active:", length(get_active_covariates()), "\n\n")

# Uncomment to see summary on load
# print_active_covariates()
