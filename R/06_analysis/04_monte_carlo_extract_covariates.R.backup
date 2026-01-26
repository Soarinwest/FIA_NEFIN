# =============================================================================
# Monte Carlo: Extract Covariates - NO CONFIRMATION (v4)
# =============================================================================
# Runs without asking - user is okay with long runtime!
# =============================================================================

source("R/00_config/config.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: EXTRACT COVARIATES (v4 - AUTO-RUN)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# LOAD JITTER LIBRARY
# =============================================================================

replicates_dir <- "data/processed/monte_carlo/replicates"
manifest_file <- "data/processed/monte_carlo/manifest.rds"

if (!file.exists(manifest_file)) {
  stop("Jitter library not found! Run 03_monte_carlo_generate_jitter_v2.R first")
}

manifest <- readRDS(manifest_file)

cat("Jitter library:\n")
cat("  Replicates:", manifest$n_replicates, "\n")
cat("  Plots per replicate:", manifest$n_plots, "\n")
cat("  Total locations:", manifest$n_replicates * manifest$n_plots, "\n\n")

# =============================================================================
# CHECK PROGRESS
# =============================================================================

jitter_files <- list.files(replicates_dir, pattern = "^rep_\\d{4}\\.csv$",
                           full.names = TRUE)

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

if (length(remaining_reps) == 0) {
  cat("✓ All replicates already extracted!\n\n")
  quit(save = "no")
}

# =============================================================================
# SPEED TEST (INFORMATIONAL ONLY)
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

# Load rasters
modis <- suppressWarnings(rast("data/raw/ndvi/modis/MODIS_NDVI_5yr_blocked_2020_2024.tif"))
s2 <- suppressWarnings(rast("data/raw/ndvi/s2/S2_NDVI_10m_2020_2024.tif"))
tmean <- suppressWarnings(rast("data/raw/prism/prism_tmean_ne_2020_2024.tif"))
ppt <- suppressWarnings(rast("data/raw/prism/prism_ppt_ne_2020_2024.tif"))

# Time 10 extractions
test_start <- Sys.time()
suppressWarnings({
  test_modis <- terra::extract(modis, vect(test_proj), method = "bilinear")
  test_s2 <- terra::extract(s2, vect(test_proj), method = "bilinear")
  test_tmean <- terra::extract(tmean, vect(test_proj), method = "bilinear")
  test_ppt <- terra::extract(ppt, vect(test_proj), method = "bilinear")
})
test_elapsed <- as.numeric(difftime(Sys.time(), test_start, units = "secs"))

# Calculate rate
plots_per_sec <- 10 / test_elapsed

# Estimate total time
total_plots <- length(remaining_reps) * manifest$n_plots
est_seconds <- total_plots / plots_per_sec
est_hours <- est_seconds / 3600

cat(sprintf("  Speed: ~%.1f plots/sec\n", plots_per_sec))
cat(sprintf("  Estimated time: %.1f hours for %d replicates\n\n",
            est_hours, length(remaining_reps)))

# =============================================================================
# START EXTRACTION (NO CONFIRMATION)
# =============================================================================

cat("Starting extraction...\n")
cat("  (This will take ~", round(est_hours, 1), "hours - grab coffee!) ☕\n\n")

# Rasters already loaded
cat("Using loaded rasters:\n")
cat("  ✓ MODIS NDVI\n")
cat("  ✓ Sentinel-2 NDVI\n")
cat("  ✓ PRISM temperature\n")
cat("  ✓ PRISM precipitation\n\n")

start_time <- Sys.time()

for (idx in seq_along(remaining_reps)) {
  rep_id <- remaining_reps[idx]
  
  # Progress every 5 replicates OR every 10 minutes
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
  
  # Extract all covariates (suppress CRS warnings)
  suppressWarnings({
    ndvi_modis <- terra::extract(modis, jittered_vect, method = "bilinear")
    ndvi_s2 <- terra::extract(s2, jittered_vect, method = "bilinear")
    tmean_vals <- terra::extract(tmean, jittered_vect, method = "bilinear")
    ppt_vals <- terra::extract(ppt, jittered_vect, method = "bilinear")
  })
  
  # Add to dataframe
  jittered$ndvi_modis <- ndvi_modis[[2]]
  jittered$ndvi_s2 <- ndvi_s2[[2]]
  jittered$tmean <- tmean_vals[[2]]
  jittered$ppt <- ppt_vals[[2]]
  
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

cat("Next: Analyze uncertainty distributions\n")
cat("  Rscript R/06_analysis/05_monte_carlo_analyze_uncertainty_v2.R\n\n")

cat("Great work! Time to see if coordinate precision matters! 🚀\n\n")