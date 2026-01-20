# =============================================================================
# Monte Carlo: Generate Jittered Coordinates (Production Version)
# =============================================================================
# Adapted from sophisticated jitter library script
# Features:
# - 100 replicates per plot
# - Multi-scale hex assignment
# - Spatial constraints
# - Resume capability
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(sf)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: GENERATE JITTERED COORDINATES (v2)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

set.seed(CONFIG$monte_carlo$seed)

# =============================================================================
# PARAMETERS
# =============================================================================

N_REPLICATES <- 100  # Changed from 100 to match user request
RADIUS_M <- CONFIG$monte_carlo$jitter_radius_m
USE_CONSTRAINTS <- TRUE  # Keep jitters inside valid areas

cat("Parameters:\n")
cat("  Replicates per plot:", N_REPLICATES, "\n")
cat("  Jitter radius:", RADIUS_M, "meters\n")
cat("  Use constraints:", USE_CONSTRAINTS, "\n\n")

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
# SETUP OUTPUT
# =============================================================================

out_dir <- "data/processed/monte_carlo"
replicates_dir <- file.path(out_dir, "replicates")

ensure_dir(out_dir)
ensure_dir(replicates_dir)

# Check for existing replicates (resume capability)
existing_files <- list.files(replicates_dir, pattern = "^rep_\\d{4}\\.csv$")
existing_reps <- if (length(existing_files)) {
  as.integer(gsub("^rep_(\\d{4})\\.csv$", "\\1", existing_files))
} else {
  integer(0)
}

completed <- length(existing_reps)
remaining <- setdiff(1:N_REPLICATES, existing_reps)

if (completed > 0) {
  cat("Found", completed, "existing replicates\n")
  cat("Resuming from replicate", min(remaining), "\n\n")
  
  response <- readline("Continue from where left off? (yes/no): ")
  if (tolower(response) != "yes") {
    cat("Aborted. To start fresh, delete:", replicates_dir, "\n")
    quit(save = "no")
  }
} else {
  remaining <- 1:N_REPLICATES
}

if (length(remaining) == 0) {
  cat("✓ All", N_REPLICATES, "replicates already complete!\n")
  cat("Delete", replicates_dir, "to regenerate\n\n")
  quit(save = "no")
}

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
  cat("  Loading", scale$name, "...")
  
  if (!file.exists(scale$path)) {
    cat(" ⚠ Not found, skipping\n")
    next
  }
  
  hex_grid <- st_read(scale$path, quiet = TRUE)
  
  # Ensure hex_id column exists
  if (!("hex_id" %in% names(hex_grid))) {
    if ("ID" %in% names(hex_grid)) {
      hex_grid <- rename(hex_grid, hex_id = ID)
    } else {
      hex_grid$hex_id <- seq_len(nrow(hex_grid))
    }
  }
  
  # Clean and transform
  hex_grid$hex_id <- as.character(hex_grid$hex_id)
  hex_grid <- st_make_valid(hex_grid)
  hex_grid_5070 <- st_transform(hex_grid, crs = CONFIG$crs_analysis)
  
  hex_grids_list[[scale$name]] <- hex_grid_5070
  
  cat(" ✓\n")
}

cat("  Loaded", length(hex_grids_list), "hex grids\n\n")

# Create union boundary for constraints (optional)
if (USE_CONSTRAINTS && length(hex_grids_list) > 0) {
  cat("Building constraint boundary from finest hex grid...\n")
  
  # Use first (finest) grid as boundary
  first_grid <- hex_grids_list[[1]]
  boundary_5070 <- st_union(first_grid)
  
  cat("  ✓ Constraint boundary ready\n\n")
} else {
  boundary_5070 <- NULL
}

# =============================================================================
# GENERATE JITTERED REPLICATES
# =============================================================================

cat("Generating", length(remaining), "jittered replicates...\n")
cat("  This will create", nrow(pts_5070) * length(remaining), 
    "jittered locations\n\n")

start_time <- Sys.time()

for (idx in seq_along(remaining)) {
  r <- remaining[idx]
  
  # Progress report
  if (idx %% 10 == 0 || idx == 1) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (idx > 1) {
      rate <- idx / elapsed
      eta <- (length(remaining) - idx) / rate
      cat(sprintf("  Progress: %d/%d (%.1f%%) - ETA: %.1f min\n",
                  completed + idx, N_REPLICATES,
                  100 * (completed + idx) / N_REPLICATES,
                  eta / 60))
    } else {
      cat(sprintf("  Starting replicate %d/%d\n", r, N_REPLICATES))
    }
  }
  
  # Generate random jitter offsets
  u <- runif(nrow(pts_5070))
  rr <- sqrt(u) * RADIUS_M  # Uniform distribution in circle
  theta <- runif(nrow(pts_5070), 0, 2 * pi)
  
  dx <- rr * cos(theta)
  dy <- rr * sin(theta)
  
  # Apply jitter
  pts_j_5070 <- st_set_geometry(pts_5070, 
                                st_geometry(pts_5070) + cbind(dx, dy))
  st_crs(pts_j_5070) <- CONFIG$crs_analysis
  
  # Apply constraints (optional)
  if (!is.null(boundary_5070)) {
    # Check which points fall outside boundary
    inside <- st_within(pts_j_5070, boundary_5070, sparse = FALSE)[,1]
    
    # Re-jitter points that fell outside (up to 20 attempts)
    for (attempt in 1:20) {
      if (all(inside)) break
      
      outside_idx <- which(!inside)
      u2 <- runif(length(outside_idx))
      rr2 <- sqrt(u2) * RADIUS_M
      theta2 <- runif(length(outside_idx), 0, 2 * pi)
      dx2 <- rr2 * cos(theta2)
      dy2 <- rr2 * sin(theta2)
      
      # Re-apply jitter for outside points
      geom_outside <- st_geometry(pts_5070)[outside_idx] + cbind(dx2, dy2)
      st_geometry(pts_j_5070)[outside_idx] <- geom_outside
      
      inside <- st_within(pts_j_5070, boundary_5070, sparse = FALSE)[,1]
    }
  }
  
  # Start building replicate data
  rep_data <- st_drop_geometry(pts_j_5070) %>%
    select(CN, STATECD, COUNTYCD, PLOT, MEASYEAR) %>%
    mutate(replicate_id = r)
  
  # Assign to ALL hex grids
  for (grid_name in names(hex_grids_list)) {
    grid_sf <- hex_grids_list[[grid_name]]
    
    # Spatial join
    joined <- st_join(pts_j_5070, grid_sf["hex_id"], 
                      left = TRUE, join = st_intersects)
    
    # Extract hex_id for this grid
    hex_col_name <- paste0("hex_id_", grid_name)
    rep_data[[hex_col_name]] <- joined$hex_id
  }
  
  # Convert jittered coords back to lat/lon
  pts_j_4326 <- st_transform(pts_j_5070, 4326)
  coords_j <- st_coordinates(pts_j_4326)
  
  rep_data$lon_jittered <- coords_j[, 1]
  rep_data$lat_jittered <- coords_j[, 2]
  
  # Save this replicate
  rep_file <- file.path(replicates_dir, sprintf("rep_%04d.csv", r))
  write_csv(rep_data, rep_file)
}

elapsed_min <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat(sprintf("\n✓ Jittering complete in %.1f minutes\n\n", elapsed_min))

# =============================================================================
# CREATE MANIFEST
# =============================================================================

cat("Creating manifest...\n")

manifest <- list(
  created = as.character(Sys.time()),
  n_replicates = N_REPLICATES,
  n_plots = nrow(baseline_valid),
  radius_m = RADIUS_M,
  constrained = USE_CONSTRAINTS,
  hex_grids = sapply(CONFIG$hex_scales, function(x) x$name),
  replicates_dir = "replicates"
)

saveRDS(manifest, file.path(out_dir, "manifest.rds"))

cat("✓ Saved manifest\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  JITTER LIBRARY SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Replicates:    ", N_REPLICATES, "\n")
cat("Plots per rep: ", nrow(baseline_valid), "\n")
cat("Hex grids:     ", length(hex_grids_list), "\n")
cat("Location:      ", replicates_dir, "\n")
cat("Constrained:   ", USE_CONSTRAINTS, "\n\n")

cat("Total jittered locations:", N_REPLICATES * nrow(baseline_valid), "\n\n")

cat("Next: Extract covariates at all jittered locations\n")
cat("  Rscript R/06_analysis/04_monte_carlo_extract_covariates_v2.R\n\n")