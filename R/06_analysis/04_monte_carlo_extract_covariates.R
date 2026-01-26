# =============================================================================
# Monte Carlo: Extract Covariates (UPDATED - Phase 4 Config + Naming)
# =============================================================================
# UPDATED:
# 1. Uses Phase 4 config for covariate paths
# 2. Creates scale-specific column names (e.g., ndvi_modis_250m)
# 3. Skips if all replicates already extracted (pipeline-friendly)
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: EXTRACT COVARIATES (Phase 4 Config + Naming)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD JITTER LIBRARY
# =============================================================================

replicates_dir <- "data/processed/monte_carlo/replicates"

# Check if replicates exist
jitter_files <- list.files(replicates_dir, pattern = "^rep_\\d{4}\\.csv$",
                           full.names = TRUE)

if (length(jitter_files) == 0) {
  cat("✗ No jittered coordinates found!\n")
  cat("  Run: Rscript R/06_analysis/03_monte_carlo_generate_jitter.R\n\n")
  stop("Jitter library not found")
}

cat("Jitter library:\n")
cat("  Replicates:", length(jitter_files), "\n\n")

# =============================================================================
# CHECK PROGRESS
# =============================================================================

extracted_dir <- "data/processed/monte_carlo/extracted"
dir.create(extracted_dir, showWarnings = FALSE, recursive = TRUE)

extracted_files <- list.files(extracted_dir, pattern = "^rep_\\d{4}_covariates\\.csv$")
extracted_reps <- if (length(extracted_files)) {
  as.integer(gsub("^rep_(\\d{4})_covariates\\.csv$", "\\1", extracted_files))
} else {
  integer(0)
}

all_reps <- as.integer(gsub(".*/rep_(\\d{4})\\.csv$", "\\1", jitter_files))
remaining_reps <- setdiff(all_reps, extracted_reps)

cat("Progress:\n")
cat("  Total replicates:", length(all_reps), "\n")
cat("  Already extracted:", length(extracted_reps), "\n")
cat("  Remaining:", length(remaining_reps), "\n\n")

# UPDATED: Skip if all complete (don't quit)
if (length(remaining_reps) == 0) {
  cat("✓ All replicates already extracted!\n")
  cat("  Skipping covariate extraction...\n")
  cat("  Location:", extracted_dir, "\n\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  MONTE CARLO COVARIATES: SKIPPED (ALREADY COMPLETE)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  invisible(NULL)
  
} else {
  
  # =============================================================================
  # LOAD RASTERS FROM PHASE 4 CONFIG
  # =============================================================================
  
  cat("Loading rasters from Phase 4 config...\n")
  
  # Define which covariates to extract
  covariates_to_extract <- list(
    list(key = "ndvi_modis", col_name = "ndvi_modis_250m"),
    list(key = "ndvi_s2", col_name = "ndvi_s2_10m"),
    list(key = "tmean_coarse", col_name = "tmean_250m"),
    list(key = "ppt_coarse", col_name = "ppt_250m")
  )
  
  # Load rasters
  rasters <- list()
  for (cov_def in covariates_to_extract) {
    cov <- COVARIATES[[cov_def$key]]
    
    if (is.null(cov)) {
      cat("  ⚠ Covariate not found in config:", cov_def$key, "\n")
      next
    }
    
    if (!file.exists(cov$path)) {
      cat("  ✗ File not found:", cov$display_name, "\n")
      cat("    Expected:", cov$path, "\n")
      stop("Missing covariate raster")
    }
    
    rasters[[cov_def$col_name]] <- suppressWarnings(rast(cov$path))
    cat("  ✓", cov$display_name, "→", cov_def$col_name, "\n")
  }
  
  cat("\n")
  
  # =============================================================================
  # SPEED TEST
  # =============================================================================
  
  cat("Running speed test on 10 plots...\n")
  
  # Load first replicate for testing
  test_file <- jitter_files[1]
  test_data <- read_csv(test_file, show_col_types = FALSE, n_max = 10)
  
  # Convert to spatial
  test_sf <- st_as_sf(test_data,
                      coords = c("lon_jittered", "lat_jittered"),
                      crs = 4326)
  test_proj <- st_transform(test_sf, crs = CONFIG$crs_analysis)
  
  # Time extractions
  test_start <- Sys.time()
  suppressWarnings({
    for (col_name in names(rasters)) {
      terra::extract(rasters[[col_name]], vect(test_proj), method = "bilinear")
    }
  })
  test_elapsed <- as.numeric(difftime(Sys.time(), test_start, units = "secs"))
  
  # Calculate rate
  plots_per_sec <- 10 / test_elapsed
  
  # Load one replicate to get n_plots
  first_rep <- read_csv(jitter_files[1], show_col_types = FALSE)
  n_plots <- nrow(first_rep)
  
  # Estimate total time
  total_plots <- length(remaining_reps) * n_plots
  est_seconds <- total_plots / plots_per_sec
  est_hours <- est_seconds / 3600
  
  cat(sprintf("  Speed: ~%.1f plots/sec\n", plots_per_sec))
  cat(sprintf("  Estimated time: %.1f hours for %d replicates\n\n",
              est_hours, length(remaining_reps)))
  
  # =============================================================================
  # START EXTRACTION
  # =============================================================================
  
  cat("Starting extraction...\n")
  if (est_hours > 1) {
    cat("  (This will take ~", round(est_hours, 1), "hours - grab coffee!)n\n")
  } else {
    cat("  (Should complete in ~", round(est_hours * 60), "minutes)\n\n")
  }
  
  start_time <- Sys.time()
  
  for (idx in seq_along(remaining_reps)) {
    rep_id <- remaining_reps[idx]
    
    # Progress every 5 replicates
    show_progress <- (idx %% 5 == 0 || idx == 1)
    
    if (show_progress) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (idx > 1) {
        rate <- idx / elapsed
        eta <- (length(remaining_reps) - idx) / rate
        pct_complete <- 100 * idx / length(remaining_reps)
        
        cat(sprintf("[%s] %d/%d (%.1f%%) | ETA: %.1f hrs | Speed: %.1f rep/min\n",
                    format(Sys.time(), "%H:%M:%S"),
                    idx, length(remaining_reps),
                    pct_complete,
                    eta / 3600,
                    rate * 60))
      } else {
        cat(sprintf("[%s] Starting replicate %04d (1/%d)\n",
                    format(Sys.time(), "%H:%M:%S"),
                    rep_id, length(remaining_reps)))
      }
    }
    
    # Load jittered coordinates
    jitter_file <- file.path(replicates_dir, sprintf("rep_%04d.csv", rep_id))
    jittered <- read_csv(jitter_file, show_col_types = FALSE)
    
    # Convert to spatial
    jittered_sf <- st_as_sf(jittered,
                            coords = c("lon_jittered", "lat_jittered"),
                            crs = 4326)
    jittered_proj <- st_transform(jittered_sf, crs = CONFIG$crs_analysis)
    jittered_vect <- vect(jittered_proj)
    
    # Extract all covariates with scale-specific names
    suppressWarnings({
      for (col_name in names(rasters)) {
        vals <- terra::extract(rasters[[col_name]], jittered_vect, method = "bilinear")
        jittered[[col_name]] <- vals[[2]]
      }
    })
    
    # Save
    output_file <- file.path(extracted_dir,
                             sprintf("rep_%04d_covariates.csv", rep_id))
    write_csv(jittered, output_file)
  }
  
  elapsed <- difftime(Sys.time(), start_time, units = "hours")
  cat(sprintf("\n✓ Extraction complete in %.2f hours\n", elapsed))
  cat(sprintf("  Actual speed: %.1f replicates/hour\n\n", 
              length(remaining_reps) / as.numeric(elapsed)))
  
  # =============================================================================
  # SUMMARY
  # =============================================================================
  
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  EXTRACTION COMPLETE! 🎉\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  all_extracted <- list.files(extracted_dir, pattern = "^rep_\\d{4}_covariates\\.csv$",
                              full.names = TRUE)
  
  cat("Output:\n")
  cat("  Location:", extracted_dir, "\n")
  cat("  Files:", length(all_extracted), "\n")
  cat("  Total size:", round(sum(file.size(all_extracted)) / 1024^2, 1), "MB\n\n")
  
  cat("Column naming:\n")
  cat("  • ndvi_modis_250m (MODIS NDVI)\n")
  cat("  • ndvi_s2_10m (Sentinel-2 NDVI)\n")
  cat("  • tmean_250m (Temperature)\n")
  cat("  • ppt_250m (Precipitation)\n\n")
  
  cat("Next: Analyze uncertainty distributions\n")
  cat("  Rscript R/06_analysis/05_monte_carlo_analyze_uncertainty.R\n\n")
}