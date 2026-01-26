# =============================================================================
# Monte Carlo: Generate Jittered Coordinates (UPDATED - Pipeline Friendly)
# =============================================================================
# UPDATED: If all 100 replicates exist, skip generation instead of quitting
# This allows run_analysis.R to continue to next steps
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: GENERATE JITTERED COORDINATES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(CONFIG$monte_carlo$seed)

# =============================================================================
# PARAMETERS
# =============================================================================

N_REPLICATES <- 100
RADIUS_M <- CONFIG$monte_carlo$jitter_radius_m
USE_CONSTRAINTS <- TRUE

cat("Parameters:\n")
cat("  Replicates per plot:", N_REPLICATES, "\n")
cat("  Jitter radius:", RADIUS_M, "meters\n")
cat("  Use constraints:", USE_CONSTRAINTS, "\n\n")

# =============================================================================
# CHECK IF ALREADY COMPLETE
# =============================================================================

out_dir <- "data/processed/monte_carlo"
replicates_dir <- file.path(out_dir, "replicates")

ensure_dir(out_dir)
ensure_dir(replicates_dir)

# Check for existing replicates
existing_files <- list.files(replicates_dir, pattern = "^rep_\\d{4}\\.csv$")
existing_reps <- if (length(existing_files)) {
  as.integer(gsub("^rep_(\\d{4})\\.csv$", "\\1", existing_files))
} else {
  integer(0)
}

completed <- length(existing_reps)
remaining <- setdiff(1:N_REPLICATES, existing_reps)

# UPDATED: Skip if all replicates complete (don't quit)
if (length(remaining) == 0) {
  cat("✓ All", N_REPLICATES, "replicates already complete!\n")
  cat("  Skipping jitter generation...\n")
  cat("  Location:", replicates_dir, "\n\n")
  cat("  To regenerate, delete the replicates directory and re-run.\n\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  MONTE CARLO JITTER: SKIPPED (ALREADY COMPLETE)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  # Return early - don't stop entire script
  # This allows run_analysis.R to continue
  invisible(NULL)
  
} else {
  
  # Continue with generation if incomplete
  
  if (completed > 0) {
    cat("Found", completed, "existing replicates\n")
    cat("Missing:", length(remaining), "replicates\n")
    cat("Resuming from replicate", min(remaining), "\n\n")
    
    # UPDATED: Auto-resume in pipeline mode, skip prompt
    if (interactive()) {
      response <- readline("Continue from where left off? (yes/no): ")
      if (tolower(response) != "yes") {
        cat("Aborted. To start fresh, delete:", replicates_dir, "\n")
        stop("User aborted")
      }
    } else {
      cat("  → Auto-resuming (non-interactive mode)\n\n")
    }
  } else {
    remaining <- 1:N_REPLICATES
  }
  
  # =============================================================================
  # LOAD FIA DATA
  # =============================================================================
  
  cat("Loading FIA baseline plots...\n")
  
  baseline <- read_csv("data/processed/baseline_with_covariates.csv",
                       show_col_types = FALSE)
  
  # Filter for valid plots
  baseline_valid <- baseline %>%
    filter(is.finite(lat_for_extraction), is.finite(lon_for_extraction))
  
  cat("  Total plots:", nrow(baseline_valid), "\n\n")
  
  # =============================================================================
  # CONVERT TO SPATIAL
  # =============================================================================
  
  cat("Converting plots to spatial points...\n")
  
  # Disable spherical geometry for faster processing
  sf_use_s2(FALSE)
  
  # Convert to sf (WGS84)
  pts_4326 <- st_as_sf(baseline_valid,
                       coords = c("lon_for_extraction", "lat_for_extraction"),
                       crs = 4326,
                       remove = FALSE)
  
  # Transform to Albers Equal Area (5070) for accurate distance
  pts_5070 <- st_transform(pts_4326, crs = CONFIG$crs_analysis)
  
  cat("  Plots converted to CRS:", CONFIG$crs_analysis, "\n\n")
  
  # =============================================================================
  # LOAD HEX GRIDS (for constraints & assignment)
  # =============================================================================
  
  cat("Loading hex grids...\n")
  
  hex_grids_list <- list()
  
  for (scale in CONFIG$hex_scales) {
    hex_path <- file.path("data/processed/hexagons", 
                          paste0("hex_grid_", scale$name, ".gpkg"))
    
    if (file.exists(hex_path)) {
      hex_grids_list[[scale$name]] <- st_read(hex_path, quiet = TRUE)
      cat("  ✓", scale$name, "-", nrow(hex_grids_list[[scale$name]]), "hexagons\n")
    }
  }
  
  cat("\n")
  
  # =============================================================================
  # GENERATE JITTERED COORDINATES
  # =============================================================================
  
  cat("Generating jittered coordinates...\n")
  cat("  Progress: [", paste(rep(".", min(50, length(remaining))), collapse = ""), "]\n")
  cat("            ", sep = "")
  
  progress_interval <- max(1, length(remaining) %/% 50)
  
  for (i in seq_along(remaining)) {
    rep_num <- remaining[i]
    
    # Generate jitter
    angles <- runif(nrow(pts_5070), 0, 2 * pi)
    distances <- sqrt(runif(nrow(pts_5070))) * RADIUS_M  # Square root for uniform spatial distribution
    
    # Calculate offsets
    dx <- distances * cos(angles)
    dy <- distances * sin(angles)
    
    # Apply jitter to coordinates
    jittered_geom <- st_geometry(pts_5070) + c(dx, dy)
    
    # Create jittered sf object
    pts_jittered <- st_sf(
      baseline_valid %>% select(CN),
      geometry = jittered_geom,
      crs = CONFIG$crs_analysis
    )
    
    # Transform back to WGS84
    pts_jittered_4326 <- st_transform(pts_jittered, crs = 4326)
    
    # Extract coordinates
    coords_jittered <- st_coordinates(pts_jittered_4326)
    
    # Create output dataframe
    result <- baseline_valid %>%
      select(CN, lat_original = lat, lon_original = lon) %>%
      mutate(
        rep = rep_num,
        lat_jittered = coords_jittered[, "Y"],
        lon_jittered = coords_jittered[, "X"],
        jitter_radius_m = RADIUS_M
      )
    
    # Assign to hexagons (all scales)
    for (scale_name in names(hex_grids_list)) {
      hex_grid <- hex_grids_list[[scale_name]]
      
      # Spatial join
      joined <- st_join(pts_jittered, hex_grid["hex_id"], left = TRUE)
      result[[paste0("hex_", scale_name)]] <- joined$hex_id
    }
    
    # Save replicate
    output_path <- file.path(replicates_dir, sprintf("rep_%04d.csv", rep_num))
    write_csv(result, output_path)
    
    # Progress indicator
    if (i %% progress_interval == 0) {
      cat("=")
    }
  }
  
  cat("\n\n")
  
  # =============================================================================
  # SUMMARY
  # =============================================================================
  
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  MONTE CARLO GENERATION COMPLETE\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  cat("Generated replicates:", length(remaining), "\n")
  cat("Total replicates:", N_REPLICATES, "\n")
  cat("Plots per replicate:", nrow(baseline_valid), "\n")
  cat("Output directory:", replicates_dir, "\n\n")
  
  cat("Next step: Extract covariates\n")
  cat("  Rscript R/06_analysis/04_monte_carlo_extract_covariates.R\n\n")
}