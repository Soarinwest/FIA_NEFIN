# =============================================================================
# Monte Carlo: Extract Covariates from Jitter Library (Production Version)
# =============================================================================
# Processes individual replicate files
# Resume capability if extraction crashes
# =============================================================================

source("R/00_config/config.R")

library(sf)
library(terra)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  MONTE CARLO: EXTRACT COVARIATES FROM JITTER LIBRARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# CHECK JITTER LIBRARY EXISTS
# =============================================================================

replicates_dir <- "data/processed/monte_carlo/replicates"
manifest_file <- "data/processed/monte_carlo/manifest.rds"

if (!file.exists(manifest_file)) {
  stop("Jitter library not found! Run 03_monte_carlo_generate_jitter_v2.R first")
}

manifest <- readRDS(manifest_file)

cat("Jitter library info:\n")
cat("  Created:", manifest$created, "\n")
cat("  Replicates:", manifest$n_replicates, "\n")
cat("  Plots per replicate:", manifest$n_plots, "\n")
cat("  Total locations:", manifest$n_replicates * manifest$n_plots, "\n\n")

# =============================================================================
# CHECK WHICH REPLICATES NEED PROCESSING
# =============================================================================

# Find jittered files (input)
jitter_files <- list.files(replicates_dir, pattern = "^rep_\\d{4}\\.csv$",
                           full.names = TRUE)

if (length(jitter_files) == 0) {
  stop("No replicate files found in:", replicates_dir)
}

# Check for already extracted files (output)
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
  cat("✓ All replicates already extracted!\n")
  cat("  Delete", extracted_dir, "to re-extract\n\n")
  quit(save = "no")
}

# =============================================================================
# CONFIRM LONG RUN
# =============================================================================

total_extractions <- length(remaining_reps) * manifest$n_plots
est_hours <- total_extractions / 3600  # ~1 extraction per second

cat("⚠ WARNING: This will take approximately", round(est_hours, 1), "hours\n")
cat("  Processing", length(remaining_reps), "replicates ×",
    manifest$n_plots, "plots =", total_extractions, "extractions\n\n")

response <- readline("Continue? (yes/no): ")
if (tolower(response) != "yes") {
  cat("Aborted.\n")
  quit(save = "no")
}

# =============================================================================
# LOAD RASTERS
# =============================================================================

cat("\nLoading rasters...\n")

modis <- rast("data/raw/ndvi/modis/MODIS_NDVI_5yr_blocked_2020_2024.tif")
cat("  ✓ MODIS NDVI\n")

s2 <- rast("data/raw/ndvi/s2/S2_NDVI_10m_2020_2025.tif")
cat("  ✓ Sentinel-2 NDVI\n")

tmean <- rast("data/raw/prism/prism_tmean_ne_2020_2024.tif")
cat("  ✓ PRISM temperature\n")

ppt <- rast("data/raw/prism/prism_ppt_ne_2020_2024.tif")
cat("  ✓ PRISM precipitation\n\n")

# =============================================================================
# PROCESS REPLICATES
# =============================================================================

cat("Processing replicates...\n\n")

start_time <- Sys.time()

for (idx in seq_along(remaining_reps)) {
  rep_id <- remaining_reps[idx]
  
  # Progress report
  if (idx %% 10 == 0 || idx == 1) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (idx > 1) {
      rate <- idx / elapsed
      eta <- (length(remaining_reps) - idx) / rate
      cat(sprintf("Progress: %d/%d (%.1f%%) - ETA: %.1f hours\n",
                  idx, length(remaining_reps),
                  100 * idx / length(remaining_reps),
                  eta / 3600))
    } else {
      cat(sprintf("Starting replicate %04d (%d/%d)\n", 
                  rep_id, idx, length(remaining_reps)))
    }
  }
  
  # Load jittered coordinates
  jitter_file <- file.path(replicates_dir, sprintf("rep_%04d.csv", rep_id))
  jittered <- read_csv(jitter_file, show_col_types = FALSE)
  
  # Convert to spatial
  jittered_sf <- st_as_sf(jittered,
                          coords = c("lon_jittered", "lat_jittered"),
                          crs = 4326)
  
  # Transform to match raster CRS
  jittered_proj <- st_transform(jittered_sf, crs = CONFIG$crs_analysis)
  
  # Extract covariates
  ndvi_modis <- terra::extract(modis, vect(jittered_proj), method = "bilinear")
  jittered$ndvi_modis <- ndvi_modis[[2]]
  
  ndvi_s2 <- terra::extract(s2, vect(jittered_proj), method = "bilinear")
  jittered$ndvi_s2 <- ndvi_s2[[2]]
  
  tmean_vals <- terra::extract(tmean, vect(jittered_proj), method = "bilinear")
  jittered$tmean <- tmean_vals[[2]]
  
  ppt_vals <- terra::extract(ppt, vect(jittered_proj), method = "bilinear")
  jittered$ppt <- ppt_vals[[2]]
  
  # Save extracted data
  output_file <- file.path(extracted_dir, 
                           sprintf("rep_%04d_covariates.csv", rep_id))
  write_csv(jittered, output_file)
}

elapsed <- difftime(Sys.time(), start_time, units = "hours")
cat(sprintf("\n✓ Extraction complete in %.2f hours\n\n", elapsed))

# =============================================================================
# COMBINE ALL REPLICATES (OPTIONAL)
# =============================================================================

cat("Combining all extracted replicates...\n")

all_extracted <- list.files(extracted_dir, pattern = "^rep_\\d{4}_covariates\\.csv$",
                            full.names = TRUE)

# For memory efficiency, we'll save summary stats per replicate
# rather than one giant file

cat("  Found", length(all_extracted), "extracted files\n")
cat("  Total size:", 
    round(sum(file.size(all_extracted)) / 1024^2, 1), "MB\n\n")

cat("Files saved in:", extracted_dir, "\n\n")

cat("Next: Analyze uncertainty distributions\n")
cat("  Rscript R/06_analysis/05_monte_carlo_analyze_uncertainty_v2.R\n\n")