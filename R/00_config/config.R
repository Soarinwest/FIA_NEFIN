# =============================================================================
# Central Configuration for FIA-NEFIN Comparison Pipeline
# =============================================================================
# This replaces all YAML configs with a single, clean R configuration
# 
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

CONFIG <- list(
  
  # ===========================================================================
  # PROJECT METADATA
  # ===========================================================================
  
  project_name = "FIA-NEFIN Hexagon Comparison",
  version = "2.0",
  
  # ===========================================================================
  # GEOGRAPHIC SCOPE
  # ===========================================================================
  
  states = c("VT", "NH", "ME", "MA", "CT", "RI", "NY"),
  
  state_codes = list(
    VT = 50, NH = 33, ME = 23, MA = 25, 
    CT = 9, RI = 44, NY = 36
  ),
  
  # Coordinate Reference Systems
  crs_wgs84 = 4326,        # WGS84 (input data)
  crs_analysis = 5070,      # Albers Equal Area (analysis)
  
  # ===========================================================================
  # TEMPORAL SCOPE
  # ===========================================================================
  
  # Analysis period
  year_start = 2020,
  year_end = 2024,
  years = 2020:2024,
  
  # Window for temporal aggregation
  window_years = 5,
  
  # ===========================================================================
  # FIA DATA SPECIFICATIONS
  # ===========================================================================
  
  fia = list(
    # Evaluation type
    eval_type = "EXPN",  # Expansion-based estimates
    
    # Forest type filter (forested conditions only)
    # See FIA Database User Guide for FORTYPCD codes
    forested_only = TRUE,
    
    # Biomass field
    biomass_field = "DRYBIO_AG",  # Aboveground dry biomass (pounds)
    
    # Conversion factor: pounds/acre → Mg/ha
    lb_per_acre_to_Mg_per_ha = 0.001121,
    
    # Plot size for expansion
    plot_size_acres = 1,  # FIA subplot = 1/6 acre, but we use TREE-level expansion
    
    # Coordinate fuzzing
    fuzz_distance_km = 1.6,  # FIA fuzzes ±1 mile (1.609 km)
    fuzz_distance_m = 1609.34
  ),
  
  # ===========================================================================
  # MONTE CARLO CONFIGURATION
  # ===========================================================================
  
  monte_carlo = list(
    n_reps = 100,              # Number of jitter replicates
    jitter_radius_m = 1609.34, # Jitter radius (meters)
    seed = 42                  # For reproducibility
  ),
  
  # ===========================================================================
  # HEX GRID SCALES
  # ===========================================================================
  
  hex_scales = list(
    list(name = "100ha",  path = "data/hex/hex_grid_100ha.geojson",  area_ha = 100),
    list(name = "500ha",  path = "data/hex/hex_grid_500ha.geojson",  area_ha = 500),
    list(name = "1kha",   path = "data/hex/hex_grid_1kha.geojson",   area_ha = 1000),
    list(name = "2_4kha", path = "data/hex/hex_grid_2_4kha.geojson", area_ha = 2428),
    list(name = "5kha",   path = "data/hex/hex_grid_5kha.geojson",   area_ha = 5000),
    list(name = "10kha",  path = "data/hex/hex_grid_10kha.geojson",  area_ha = 10000),
    list(name = "50kha",  path = "data/hex/hex_grid_50kha.geojson",  area_ha = 50000),
    list(name = "64kha",  path = "data/hex/hex_grid_64kha.geojson",  area_ha = 64000),
    list(name = "100kha", path = "data/hex/hex_grid_100kha.geojson", area_ha = 100000)
  ),
  
  # ===========================================================================
  # ANALYSIS THRESHOLDS
  # ===========================================================================
  
  thresholds = list(
    min_plots_per_hex = 1,     # Minimum plots for hex estimate
    target_plots_per_hex = 3,  # Target for "good" coverage
    max_biomass_Mg_ha = 500    # Flag extreme outliers
  ),
  
  # ===========================================================================
  # FILE PATHS
  # ===========================================================================
  
  paths = list(
    # Raw data (inputs - never modified)
    raw_fia = "data/raw/fia_sqlite",
    raw_nefin = "data/raw/nefin",
    
    # Interim data (processing steps)
    interim = "data/interim",
    interim_fia = "data/interim/fia",
    interim_nefin = "data/interim/nefin",
    
    # Processed data (final clean datasets)
    processed = "data/processed",
    
    # Spatial data
    hex_grids = "data/hex",
    boundaries = "data/boundaries",
    
    # Covariates
    ndvi = "data/processed/ndvi",
    prism = "data/processed/prism",
    
    # Outputs
    runs = "runs",
    outputs = "outputs",
    figures = "outputs/figures"
  ),
  
  # ===========================================================================
  # DATASET SCHEMAS
  # ===========================================================================
  
  # Standard columns for all plot-level datasets
  standard_columns = c(
    "CN",                  # Plot ID (Condition Number)
    "STATECD",            # State code
    "COUNTYCD",           # County code  
    "PLOT",               # Plot number
    "MEASYEAR",           # Measurement year
    "lat",                # Latitude (WGS84)
    "lon",                # Longitude (WGS84)
    "biomass",            # AGLB (Mg/ha)
    "dataset",            # "FIA" or "NEFIN"
    "coord_source",       # "fuzzed" or "true"
    "lat_for_extraction", # Coordinates to use for covariates
    "lon_for_extraction"
  ),
  
  # ===========================================================================
  # NEFIN CONFIGURATION
  # ===========================================================================
  
  nefin = list(
    main_file = "TREE_PLOT_DATA.csv",
    plots_file = "NEFIN_plots.csv",
    
    # NEFIN-specific settings
    has_true_coords = TRUE,
    coord_precision_m = 10  # NEFIN coordinates accurate to ~10m
  ),
  
  # ===========================================================================
  # PROCESSING OPTIONS
  # ===========================================================================
  
  options = list(
    parallel = FALSE,          # Use parallel processing (future support)
    n_cores = 4,               # Number of cores if parallel
    verbose = TRUE,            # Print progress messages
    validate_outputs = TRUE,   # Run validation checks
    overwrite = FALSE          # Overwrite existing outputs
  )
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get path relative to project root
get_path <- function(path_name) {
  if (path_name %in% names(CONFIG$paths)) {
    return(CONFIG$paths[[path_name]])
  }
  stop("Unknown path: ", path_name)
}

#' Get hex grid configuration
get_hex_scale <- function(scale_name) {
  for (scale in CONFIG$hex_scales) {
    if (scale$name == scale_name) {
      return(scale)
    }
  }
  stop("Unknown hex scale: ", scale_name)
}

#' Print configuration summary
print_config <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  FIA-NEFIN COMPARISON PIPELINE CONFIGURATION\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("GEOGRAPHIC SCOPE:\n")
  cat("  States:", paste(CONFIG$states, collapse = ", "), "\n")
  cat("  CRS Analysis:", CONFIG$crs_analysis, "(Albers Equal Area)\n\n")
  
  cat("TEMPORAL SCOPE:\n")
  cat("  Years:", paste(CONFIG$years, collapse = ", "), "\n")
  cat("  Window:", CONFIG$window_years, "years\n\n")
  
  cat("HEX SCALES:\n")
  for (scale in CONFIG$hex_scales) {
    cat(sprintf("  - %-8s (%,d ha)\n", scale$name, scale$area_ha))
  }
  
  cat("\nMONTE CARLO:\n")
  cat("  Replicates:", CONFIG$monte_carlo$n_reps, "\n")
  cat("  Jitter radius:", CONFIG$monte_carlo$jitter_radius_m, "m\n\n")
  
  cat("DATA PATHS:\n")
  cat("  Raw FIA:", CONFIG$paths$raw_fia, "\n")
  cat("  Raw NEFIN:", CONFIG$paths$raw_nefin, "\n")
  cat("  Processed:", CONFIG$paths$processed, "\n")
  cat("  Outputs:", CONFIG$paths$runs, "\n\n")
  
  cat("═══════════════════════════════════════════════════════════════════\n\n")
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate configuration on load
validate_config <- function() {
  
  # Check required directories exist
  required_dirs <- c("raw_fia", "raw_nefin", "hex_grids")
  
  for (dir_name in required_dirs) {
    path <- CONFIG$paths[[dir_name]]
    if (!dir.exists(path)) {
      warning("Required directory does not exist: ", path)
    }
  }
  
  # Check year range is valid
  if (CONFIG$year_start > CONFIG$year_end) {
    stop("year_start must be <= year_end")
  }
  
  # Check hex scales are unique
  scale_names <- sapply(CONFIG$hex_scales, function(x) x$name)
  if (any(duplicated(scale_names))) {
    stop("Duplicate hex scale names detected")
  }
  
  invisible(TRUE)
}

# Run validation when config is loaded
validate_config()

# Print summary if interactive
if (interactive()) {
  print_config()
}
