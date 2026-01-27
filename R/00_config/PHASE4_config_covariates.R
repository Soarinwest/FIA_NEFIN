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
  
  # ===========================================================================
  # FINE SCALE (10m) - All covariates at 10m resolution
  # ===========================================================================
  
  # ---------------------------------------------------------------------------
  # STRUCTURE - Canopy Height (10m)
  # ---------------------------------------------------------------------------
  
  canopy_height_fine = list(
    name = "canopy_height",
    display_name = "Canopy Height",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/canopy_height_10m_2020_NE.tif",
    resolution = "10m",
    type = "structure",
    category = "canopy",
    scale = "fine",
    active = TRUE,
    notes = "ETH Global Canopy Height 2020 - CRITICAL PREDICTOR"
  ),
  
  # ---------------------------------------------------------------------------
  # SPECTRAL - SENTINEL-2 (10m native)
  # ---------------------------------------------------------------------------
  
  ndvi_s2 = list(
    name = "ndvi_s2",
    display_name = "NDVI (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_NDVI_10m_2020_2024.tif",  
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = TRUE,
    notes = "Sentinel-2 NDVI"
  ),
  
  evi_s2 = list(
    name = "evi_s2",
    display_name = "EVI (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_EVI_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = TRUE,  
    notes = "Enhanced Vegetation Index "
  ),
  
  nbr_s2 = list(
    name = "nbr_s2",
    display_name = "NBR (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_NBR_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = TRUE, 
    notes = "Normalized Burn Ratio"
  ),
  
  ndwi_s2 = list(
    name = "ndwi_s2",
    display_name = "NDWI (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_NDWI_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "vegetation_index",
    scale = "fine",
    active = TRUE,  
    notes = "Normalized Difference Water Index"
  ),
  
  # Raw bands
  red_s2 = list(
    name = "red_s2",
    display_name = "Red Band (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_B4_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = TRUE,  # Available but not active
    notes = "Red band (B4)"
  ),
  
  green_s2 = list(
    name = "green_s2",
    display_name = "Green Band (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_B3_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = TRUE,
    notes = "Green band (B3)"
  ),
  
  blue_s2 = list(
    name = "blue_s2",
    display_name = "Blue Band (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_B2_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = TRUE,
    notes = "Blue band (B2)"
  ),
  
  swir1_s2 = list(
    name = "swir1_s2",
    display_name = "SWIR1 Band (Sentinel-2)",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/S2_B11_10m_2020_2024.tif",
    resolution = "10m",
    type = "spectral",
    category = "band",
    scale = "fine",
    active = FALSE,
    notes = "Short-wave infrared band (B11)"
  ),
  
  # ---------------------------------------------------------------------------
  # TOPOGRAPHY (10m native from DEM - need to copy to fine_10m/)
  # ---------------------------------------------------------------------------
  
  elevation_fine = list(
    name = "elevation",
    display_name = "Elevation",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/Elevation10m.tif",  # In raw, will copy
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,
    notes = "Copy from raw/DEM/ to fine_10m/"
  ),
  
  slope_fine = list(
    name = "slope",
    display_name = "Slope",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/Slope10m.tif",  # In raw, will copy
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,
    notes = "Copy from raw/DEM/ to fine_10m/"
  ),
  
  aspect_fine = list(
    name = "aspect",
    display_name = "Aspect",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/Aspect10m.tif",  # In raw, will copy
    resolution = "10m",
    type = "topographic",
    category = "terrain",
    scale = "fine",
    active = TRUE,
    notes = "Copy from raw/DEM/ to fine_10m/"
  ),
  
  # ---------------------------------------------------------------------------
  # CLIMATE (PRISM resampled to 10m - already created!)
  # ---------------------------------------------------------------------------
  
  tmean_fine = list(
    name = "tmean",
    display_name = "Mean Temperature",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/tmean.tif",
    resolution = "10m",
    type = "climate",
    category = "temperature",
    scale = "fine",
    active = TRUE,
    notes = "Daymet V4 resampled to 10m"  # ← Updated note
  ),
  
  # === ADD THESE TWO NEW ENTRIES ===
  tmin_fine = list(
    name = "tmin",
    display_name = "Minimum Temperature",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/tmin.tif",
    resolution = "10m",
    type = "climate",
    category = "temperature",
    scale = "fine",
    active = TRUE,
    notes = "Daymet V4 resampled to 10m"
  ),
  
  tmax_fine = list(
    name = "tmax",
    display_name = "Maximum Temperature",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/tmax.tif",
    resolution = "10m",
    type = "climate",
    category = "temperature",
    scale = "fine",
    active = TRUE,
    notes = "Daymet V4 resampled to 10m"
  ),
  # === END NEW ENTRIES ===
  
  ppt_fine = list(
    name = "ppt",
    display_name = "Precipitation",
    path = "D:/FIA_NEFIN/data/covariates/fine_10m/ppt.tif",
    resolution = "10m",
    type = "climate",
    category = "precipitation",
    scale = "fine",
    active = TRUE,
    notes = "Daymet V4 resampled to 10m"  # ← Updated note
  ),
  
  # ===========================================================================
  # COARSE SCALE (250m) - All covariates at 250m resolution
  # ===========================================================================
  
  # ---------------------------------------------------------------------------
  # STRUCTURE - Canopy Height (250m)
  # ---------------------------------------------------------------------------
  
  canopy_height_coarse = list(
    name = "canopy_height",
    display_name = "Canopy Height",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/canopy_height_250m_2020_NE.tif",
    resolution = "250m",
    type = "structure",
    category = "canopy",
    scale = "coarse",
    active = TRUE,
    notes = "GEDI/Aggregated canopy height 2020 - CRITICAL PREDICTOR"
  ),
  
  # ---------------------------------------------------------------------------
  # SPECTRAL - MODIS (250m native)
  # ---------------------------------------------------------------------------
  
  ndvi_modis = list(
    name = "ndvi_modis",
    display_name = "NDVI (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_NDVI_5yr_blocked_2020_2024.tif",  # In raw, will copy
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = TRUE,
    notes = "Copy to coarse_250m/ when ready"
  ),
  
  evi_modis = list(
    name = "evi_modis",
    display_name = "EVI (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_EVI_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = TRUE,  
    notes = "MODIS Enhanced Vegetation Index - READY"
  ),
  
  nbr_modis = list(
    name = "nbr_modis",
    display_name = "NBR (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_NBR_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = TRUE,  
    notes = "MODIS Normalized Burn Ratio - READY"
  ),
  
  ndwi_modis = list(
    name = "ndwi_modis",
    display_name = "NDWI (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_NDWI_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "vegetation_index",
    scale = "coarse",
    active = TRUE, 
    notes = "MODIS Normalized Difference Water Index"
  ),
  
  # Raw bands
  red_modis = list(
    name = "red_modis",
    display_name = "Red Band (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_RED_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "band",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS Red band"
  ),
  
  nir_modis = list(
    name = "nir_modis",
    display_name = "NIR Band (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_NIR_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "band",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS Near-infrared band"
  ),
  
  blue_modis = list(
    name = "blue_modis",
    display_name = "Blue Band (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_BLUE_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "band",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS Blue band"
  ),
  
  green_modis = list(
    name = "green_modis",
    display_name = "Green Band (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_GREEN_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "band",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS Green band"
  ),
  
  swir1_modis = list(
    name = "swir1_modis",
    display_name = "SWIR1 Band (MODIS)",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/MODIS_SWIR1_250m_2020_2024_NE.tif",
    resolution = "250m",
    type = "spectral",
    category = "band",
    scale = "coarse",
    active = TRUE,
    notes = "MODIS Short-wave infrared band"
  ),
  
  # ---------------------------------------------------------------------------
  # TOPOGRAPHY (aggregated from 10m to 250m - already done!)
  # ---------------------------------------------------------------------------
  
  elevation_coarse = list(
    name = "elevation",
    display_name = "Elevation",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/elevation_250m_NE.tif",
    resolution = "250m",
    type = "topographic",
    category = "terrain",
    scale = "coarse",
    active = TRUE,
    notes = "Aggregated to 250m - READY"
  ),
  
  slope_coarse = list(
    name = "slope",
    display_name = "Slope",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/slope_250m_NE.tif",
    resolution = "250m",
    type = "topographic",
    category = "terrain",
    scale = "coarse",
    active = TRUE,
    notes = "Aggregated to 250m - READY"
  ),
  
  aspect_coarse = list(
    name = "aspect",
    display_name = "Aspect",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/aspect_250m_NE.tif",
    resolution = "250m",
    type = "topographic",
    category = "terrain",
    scale = "coarse",
    active = TRUE,
    notes = "Aggregated to 250m - READY"
  ),
  
  # ---------------------------------------------------------------------------
  # CLIMATE (PRISM aggregated to 250m )
  # ---------------------------------------------------------------------------
  
  tmean_coarse = list(
    name = "tmean",
    display_name = "Mean Temperature",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/tmean.tif",
    resolution = "250m",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = TRUE,
    notes = "Daymet V4 resampled to 250m"  # ← Updated note
  ),
  
  # === ADD THESE TWO NEW ENTRIES ===
  tmin_coarse = list(
    name = "tmin",
    display_name = "Minimum Temperature",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/tmin.tif",
    resolution = "250m",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = TRUE,
    notes = "Daymet V4 resampled to 250m"
  ),
  
  tmax_coarse = list(
    name = "tmax",
    display_name = "Maximum Temperature",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/tmax.tif",
    resolution = "250m",
    type = "climate",
    category = "temperature",
    scale = "coarse",
    active = TRUE,
    notes = "Daymet V4 resampled to 250m"
  ),
  # === END NEW ENTRIES ===
  
  ppt_coarse = list(
    name = "ppt",
    display_name = "Precipitation",
    path = "D:/FIA_NEFIN/data/covariates/coarse_250m/ppt.tif",
    resolution = "250m",
    type = "climate",
    category = "precipitation",
    scale = "coarse",
    active = TRUE,
    notes = "Daymet V4 resampled to 250m"  # ← Updated note
  )  
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

get_active_covariates <- function() {
  active <- Filter(function(x) x$active, COVARIATES)
  return(active)
}

get_active_names <- function() {
  active <- get_active_covariates()
  return(sapply(active, function(x) x$name))
}

get_scale_covariates <- function(scale = "fine") {
  covs <- Filter(function(x) x$active && x$scale == scale, COVARIATES)
  return(sapply(covs, function(x) x$name))
}

get_covariates_by_type <- function(type = "spectral") {
  covs <- Filter(function(x) x$active && x$type == type, COVARIATES)
  return(covs)
}

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
      scale = cov$scale,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

print_active_covariates <- function() {
  active <- get_active_covariates()
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("  ACTIVE COVARIATES FOR MODELING\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  for (scale in c("fine", "coarse")) {
    scale_covs <- Filter(function(x) x$scale == scale, active)
    
    if (length(scale_covs) > 0) {
      cat(sprintf("──────────────────────────────────────────────────────────────\n"))
      cat(sprintf("  %s SCALE (%s)\n", 
                  toupper(scale), 
                  ifelse(scale == "fine", "10m", "250m")))
      cat(sprintf("──────────────────────────────────────────────────────────────\n"))
      
      types <- unique(sapply(scale_covs, function(x) x$type))
      
      for (type in types) {
        type_covs <- Filter(function(x) x$type == type, scale_covs)
        cat(sprintf("  %s:\n", toupper(type)))
        
        for (cov in type_covs) {
          status <- if(file.exists(cov$path)) "✓" else "⚠"
          cat(sprintf("    %s %s\n", status, cov$display_name))
        }
      }
      cat("\n")
    }
  }
  
  cat("Total active covariates:", length(active), "\n")
  cat("  Fine scale (10m):", length(get_scale_covariates("fine")), "\n")
  cat("  Coarse scale (250m):", length(get_scale_covariates("coarse")), "\n\n")
}

# =============================================================================
# EXPORT
# =============================================================================

cat("\n✓ Covariate configuration loaded (MATCHES YOUR FILE STRUCTURE)\n")
cat("  Total covariates defined:", length(COVARIATES), "\n")
cat("  Currently active:", length(get_active_covariates()), "\n")
cat("  Fine scale (10m):", length(get_scale_covariates("fine")), "\n")
cat("  Coarse scale (250m):", length(get_scale_covariates("coarse")), "\n\n")

# Show what's ready vs what needs setup
avail <- check_covariate_availability()
active_avail <- avail[avail$active, ]
missing <- active_avail[!active_avail$exists, ]

if (nrow(missing) > 0) {
  cat("⚠ Active covariates that need file setup:\n")
  for (i in 1:nrow(missing)) {
    cat(sprintf("  • %s (%s)\n", missing$display_name[i], missing$scale[i]))
    cat(sprintf("    Expected: %s\n", missing$path[i]))
  }
  cat("\n")
}
