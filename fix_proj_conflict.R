# =============================================================================
# DIAGNOSE AND FIX PROJ/GDAL PATH CONFLICTS
# =============================================================================
# PostgreSQL/PostGIS PROJ is interfering with R's terra package
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  DIAGNOSING PROJ/GDAL PATH CONFLICTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# ===========================================================================
# STEP 1: Check current PATH
# ===========================================================================

cat("STEP 1: Checking system PATH...\n\n")

path_var <- Sys.getenv("PATH")
path_dirs <- strsplit(path_var, ";")[[1]]

# Find PROJ/GDAL related directories
postgres_paths <- grep("PostgreSQL|PostGIS", path_dirs, value = TRUE, ignore.case = TRUE)
gdal_paths <- grep("GDAL|OSGeo", path_dirs, value = TRUE, ignore.case = TRUE)
r_paths <- grep("R\\\\|R-", path_dirs, value = TRUE, ignore.case = TRUE)

cat("PostgreSQL/PostGIS paths in system PATH:\n")
if (length(postgres_paths) > 0) {
  for (p in postgres_paths) {
    cat("  ✗", p, "\n")
  }
} else {
  cat("  (none found)\n")
}
cat("\n")

cat("R-related paths in system PATH:\n")
if (length(r_paths) > 0) {
  for (p in r_paths) {
    cat("  ✓", p, "\n")
  }
} else {
  cat("  (none found)\n")
}
cat("\n")

# ===========================================================================
# STEP 2: Check which PROJ database is being used
# ===========================================================================

cat("STEP 2: Checking PROJ database location...\n\n")

library(terra)

# Try to get GDAL info
gdal_info <- try(gdal(warn = 0), silent = TRUE)

if (!inherits(gdal_info, "try-error")) {
  cat("GDAL version:", gdal_info[1], "\n")
  cat("PROJ version:", gdal_info[2], "\n\n")
}

# Check PROJ_DATA environment variable
proj_data <- Sys.getenv("PROJ_DATA")
proj_lib <- Sys.getenv("PROJ_LIB")

cat("PROJ environment variables:\n")
cat("  PROJ_DATA:", ifelse(proj_data == "", "(not set)", proj_data), "\n")
cat("  PROJ_LIB:", ifelse(proj_lib == "", "(not set)", proj_lib), "\n\n")

# ===========================================================================
# STEP 3: Test if we can read a simple raster
# ===========================================================================

cat("STEP 3: Testing raster read capability...\n\n")

test_file <- "D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed/S2_NDWI_10m_2020_2024.tif"

cat("Testing file:", test_file, "\n")

test_result <- tryCatch({
  r <- rast(test_file)
  cat("  ✓ SUCCESS: File opened without errors!\n\n")
  "success"
}, error = function(e) {
  cat("  ✗ FAILED:", e$message, "\n\n")
  "failed"
}, warning = function(w) {
  cat("  ⚠ WARNING:", w$message, "\n\n")
  "warning"
})

# ===========================================================================
# STEP 4: Recommendations
# ===========================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  RECOMMENDATIONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

if (length(postgres_paths) > 0 && test_result == "failed") {
  cat("DIAGNOSIS: PostgreSQL PROJ is interfering with R terra package\n\n")
  
  cat("SOLUTION 1: Set PROJ_DATA in your R session (TEMPORARY FIX)\n")
  cat("  Add this to the start of your prediction script:\n")
  cat('  Sys.setenv(PROJ_DATA = "")  # Clear PostgreSQL PROJ\n')
  cat('  Sys.setenv(PROJ_LIB = "")   # Clear PostgreSQL PROJ\n\n')
  
  cat("SOLUTION 2: Remove PostgreSQL from PATH (PERMANENT FIX)\n")
  cat("  1. Open System Properties > Environment Variables\n")
  cat("  2. Edit PATH variable\n")
  cat("  3. Remove or move down these paths:\n")
  for (p in postgres_paths) {
    cat("     -", p, "\n")
  }
  cat("  4. Restart R/RStudio\n\n")
  
  cat("SOLUTION 3: Prioritize R's GDAL (BEST FIX)\n")
  cat("  Add this to your .Rprofile or script:\n\n")
  
  cat("  # Find R's terra GDAL location\n")
  cat('  terra_path <- system.file("proj", package = "terra")\n')
  cat('  if (terra_path != "") {\n')
  cat('    Sys.setenv(PROJ_DATA = terra_path)\n')
  cat('    cat("Using R terra PROJ:", terra_path, "\\n")\n')
  cat("  }\n\n")
  
  cat("SOLUTION 4: Quick test - Set environment in this session\n")
  cat("  Try running these commands now:\n\n")
  
  cat('  Sys.setenv(PROJ_DATA = "")\n')
  cat('  Sys.setenv(PROJ_LIB = "")\n')
  cat('  library(terra)\n')
  cat('  r <- rast("', test_file, '")\n', sep = "")
  cat("  # If this works, add the Sys.setenv() lines to your scripts\n\n")
  
} else if (test_result == "success") {
  cat("✓ No PROJ conflicts detected - raster reads successfully!\n\n")
  cat("If you're still getting errors, they may be intermittent.\n")
  cat("Try adding these lines to the start of your prediction script:\n\n")
  cat('  Sys.setenv(PROJ_DATA = "")\n')
  cat('  Sys.setenv(PROJ_LIB = "")\n\n')
  
} else {
  cat("Unable to determine the issue.\n")
  cat("Try the temporary fix and see if it helps:\n\n")
  cat('  Sys.setenv(PROJ_DATA = "")\n')
  cat('  Sys.setenv(PROJ_LIB = "")\n\n')
}

# ===========================================================================
# STEP 5: Create a fixed prediction script
# ===========================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  CREATING FIXED PREDICTION SCRIPT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Read original prediction script
pred_script_path <- "R/phase4_modeling/PHASE4_03_predict_biomass.R"

if (file.exists(pred_script_path)) {
  lines <- readLines(pred_script_path, warn = FALSE)
  
  # Check if PROJ fix already exists
  has_proj_fix <- any(grepl("Sys\\.setenv.*PROJ", lines))
  
  if (!has_proj_fix) {
    cat("Adding PROJ fix to prediction script...\n")
    
    # Find where to insert (after source() lines, before library())
    library_line <- which(grepl("^library\\(terra\\)", lines))[1]
    
    if (!is.na(library_line)) {
      # Insert PROJ fix before library(terra)
      proj_fix_lines <- c(
        "",
        "# Fix PostgreSQL PROJ interference",
        'Sys.setenv(PROJ_DATA = "")',
        'Sys.setenv(PROJ_LIB = "")',
        "cat(\"✓ Cleared PostgreSQL PROJ paths\\n\\n\")",
        ""
      )
      
      lines <- c(
        lines[1:(library_line-1)],
        proj_fix_lines,
        lines[library_line:length(lines)]
      )
      
      # Save
      writeLines(lines, pred_script_path)
      cat("  ✓ PROJ fix added to prediction script\n")
      cat("  Location: After source() statements, before library(terra)\n\n")
      
      cat("Now try running:\n")
      cat("  Rscript R/phase4_modeling/PHASE4_03_predict_biomass.R\n\n")
    } else {
      cat("  ⚠ Could not find library(terra) line\n")
      cat("  Add these lines manually at the start:\n\n")
      cat('  Sys.setenv(PROJ_DATA = "")\n')
      cat('  Sys.setenv(PROJ_LIB = "")\n\n')
    }
  } else {
    cat("  ✓ PROJ fix already present in prediction script\n\n")
  }
} else {
  cat("  ⚠ Prediction script not found\n\n")
}

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DIAGNOSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
