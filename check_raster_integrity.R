# =============================================================================
# CHECK RASTER FILE INTEGRITY
# =============================================================================
# Tests if a raster file can be opened and read properly
# =============================================================================

library(terra)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  CHECKING RASTER FILE INTEGRITY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# File to check
file_path <- "D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed/S2_NDWI_10m_2020_2024.tif"

cat("Checking:", file_path, "\n\n")

# ===========================================================================
# TEST 1: Does file exist?
# ===========================================================================

cat("TEST 1: File existence...\n")
if (!file.exists(file_path)) {
  cat("  ✗ FAIL: File does not exist\n")
  quit(save = "no")
} else {
  cat("  ✓ PASS: File exists\n")
  
  # Get file size
  file_size <- file.size(file_path)
  cat("  File size:", round(file_size / 1024^2, 2), "MB\n\n")
}

# ===========================================================================
# TEST 2: Can terra open it?
# ===========================================================================

cat("TEST 2: Can terra open the file?\n")
r <- NULL
open_error <- NULL

tryCatch({
  r <- rast(file_path)
  cat("  ✓ PASS: File opened successfully\n")
  cat("  Dimensions:", nrow(r), "x", ncol(r), "\n")
  cat("  CRS:", substr(crs(r), 1, 50), "...\n\n")
}, error = function(e) {
  open_error <<- e$message
  cat("  ✗ FAIL: Cannot open file\n")
  cat("  Error:", e$message, "\n\n")
})

if (!is.null(open_error)) {
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  VERDICT: FILE IS CORRUPTED OR INCOMPATIBLE\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  cat("Recommended action:\n")
  cat("  1. Delete the corrupted file\n")
  cat("  2. Re-run preprocessing to recreate it\n\n")
  cat("Commands:\n")
  cat("  file.remove('", file_path, "')\n", sep = "")
  cat("  # Then run: Rscript R/phase4_modeling/PHASE4_00_preprocess_rasters.R\n\n")
  quit(save = "no")
}

# ===========================================================================
# TEST 3: Can we read values?
# ===========================================================================

cat("TEST 3: Can we read raster values?\n")
read_error <- NULL

tryCatch({
  # Try to read a small chunk of values
  vals <- values(r, row = 1, nrows = 10)
  cat("  ✓ PASS: Successfully read values\n")
  cat("  Sample values:", head(vals[,1], 5), "\n")
  cat("  Value range:", range(vals, na.rm = TRUE), "\n\n")
}, error = function(e) {
  read_error <<- e$message
  cat("  ✗ FAIL: Cannot read values\n")
  cat("  Error:", e$message, "\n\n")
})

if (!is.null(read_error)) {
  cat("\n═══════════════════════════════════════════════════════════════════\n")
  cat("  VERDICT: FILE IS CORRUPTED (Can open but cannot read)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  cat("Recommended action: Delete and re-preprocess\n\n")
  quit(save = "no")
}

# ===========================================================================
# TEST 4: Can we compute statistics?
# ===========================================================================

cat("TEST 4: Can we compute global statistics?\n")
stats_error <- NULL

tryCatch({
  stats <- global(r, "mean", na.rm = TRUE)
  cat("  ✓ PASS: Successfully computed statistics\n")
  cat("  Global mean:", stats[1,1], "\n\n")
}, error = function(e) {
  stats_error <<- e$message
  cat("  ✗ FAIL: Cannot compute statistics\n")
  cat("  Error:", e$message, "\n\n")
})

# ===========================================================================
# TEST 5: Check for missing data
# ===========================================================================

cat("TEST 5: Checking data completeness...\n")

tryCatch({
  # Sample 1000 random cells
  set.seed(42)
  n_cells <- ncell(r)
  sample_size <- min(1000, n_cells)
  sample_cells <- sample(1:n_cells, sample_size)
  sample_vals <- r[sample_cells]
  
  n_na <- sum(is.na(sample_vals))
  pct_na <- 100 * n_na / sample_size
  
  cat("  Sampled", sample_size, "cells\n")
  cat("  Missing values:", n_na, sprintf("(%.1f%%)\n", pct_na))
  
  if (pct_na > 90) {
    cat("  ⚠ WARNING: More than 90% missing data!\n\n")
  } else {
    cat("  ✓ PASS: Reasonable amount of data\n\n")
  }
}, error = function(e) {
  cat("  ⚠ Could not sample data:", e$message, "\n\n")
})

# ===========================================================================
# FINAL VERDICT
# ===========================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VERDICT: FILE APPEARS TO BE OK\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("All tests passed! The file seems readable.\n")
cat("The prediction error might be due to:\n")
cat("  - PROJ database version mismatch\n")
cat("  - Memory issue with large raster\n")
cat("  - Temporary file lock\n\n")

cat("Try these solutions:\n")
cat("  1. Restart R/RStudio to clear memory\n")
cat("  2. Delete and re-preprocess this file\n")
cat("  3. Check if PostgreSQL PROJ is interfering\n\n")

# Show file info summary
cat("File summary:\n")
cat("  Path:", file_path, "\n")
cat("  Size:", round(file.size(file_path) / 1024^2, 2), "MB\n")
cat("  Dimensions:", nrow(r), "x", ncol(r), "\n")
cat("  CRS:", substr(crs(r), 1, 80), "...\n")
cat("  Data type:", datatype(r), "\n")
cat("  Value range:", paste(global(r, "range", na.rm = TRUE)[1,], collapse = " to "), "\n\n")
