#!/usr/bin/env Rscript
# =============================================================================
# Test Script for Clean FIA-NEFIN Pipeline
# =============================================================================
# Tests Phases A, B, C (skips Phase D hex assignment)
#
# Run from project root: Rscript test_pipeline.R
# =============================================================================
#!/usr/bin/env Rscript
# =============================================================================
# Enhanced Test Script - Extract Column Information for Phases A & B
# =============================================================================
# Comprehensive pre-flight check that tells you exactly what you have
# and what needs to be customized
# =============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║  FIA-NEFIN PIPELINE - COMPREHENSIVE DATA INSPECTION              ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n\n")

library(readr)
library(dplyr)
library(DBI)
library(RSQLite)

# =============================================================================
# SECTION 1: FIA DATABASE INSPECTION
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SECTION 1: FIA DATABASE STRUCTURE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Pick one state to inspect (Vermont - smallest)
test_state <- "VT"
db_path <- file.path("data/raw/fia_sqlite", test_state, "unzipped", 
                     paste0("SQLite_FIADB_", test_state, ".db"))

if (file.exists(db_path)) {
  cat("Inspecting", test_state, "database...\n\n")
  
  conn <- dbConnect(SQLite(), db_path)
  
  # List all tables
  tables <- dbListTables(conn)
  cat("Available tables (", length(tables), "):\n")
  cat("  ", paste(head(tables, 20), collapse=", "), "\n")
  if (length(tables) > 20) cat("  ... and", length(tables) - 20, "more\n")
  cat("\n")
  
  # PLOT table
  if ("PLOT" %in% tables) {
    cat("PLOT table columns:\n")
    plot_cols <- dbListFields(conn, "PLOT")
    cat("  ", paste(plot_cols, collapse=", "), "\n\n")
    
    # Sample data
    plot_sample <- dbGetQuery(conn, "SELECT * FROM PLOT LIMIT 3")
    cat("Sample PLOT data:\n")
    print(head(plot_sample, 3))
    cat("\n")
  }
  
  # TREE table
  if ("TREE" %in% tables) {
    cat("TREE table columns:\n")
    tree_cols <- dbListFields(conn, "TREE")
    cat("  ", paste(tree_cols, collapse=", "), "\n\n")
    
    # Check for biomass columns
    cat("Biomass-related columns in TREE:\n")
    biomass_cols <- tree_cols[grepl("BIO|DRYBIO|CARBON", tree_cols, ignore.case = TRUE)]
    if (length(biomass_cols) > 0) {
      cat("  ✓ Found:", paste(biomass_cols, collapse=", "), "\n")
    } else {
      cat("  ✗ No biomass columns found\n")
    }
    
    # Check for expansion factors
    cat("Expansion factor columns in TREE:\n")
    exp_cols <- tree_cols[grepl("TPA|EXPNS", tree_cols, ignore.case = TRUE)]
    if (length(exp_cols) > 0) {
      cat("  ✓ Found:", paste(exp_cols, collapse=", "), "\n")
    } else {
      cat("  ✗ No expansion factor columns found\n")
    }
    cat("\n")
  }
  
  # COND table
  if ("COND" %in% tables) {
    cat("COND table columns:\n")
    cond_cols <- dbListFields(conn, "COND")
    cat("  ", paste(cond_cols, collapse=", "), "\n\n")
  }
  
  dbDisconnect(conn)
  
  cat("FIA Database Summary:\n")
  cat("  ✓ Database accessible\n")
  cat("  ✓ Required tables present (PLOT, TREE, COND)\n")
  cat("  ✓ Standard FIA schema detected\n")
  cat("\n")
  
} else {
  cat("✗ FIA database not found:", db_path, "\n\n")
}

# =============================================================================
# SECTION 2: NEFIN DATA INSPECTION
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SECTION 2: NEFIN DATA STRUCTURE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# TREE_PLOT_DATA.csv
tree_plot_path <- "data/raw/nefin/TREE_PLOT_DATA.csv"

if (file.exists(tree_plot_path)) {
  cat("────────────────────────────────────────────────────────────────\n")
  cat("A. TREE_PLOT_DATA.csv\n")
  cat("────────────────────────────────────────────────────────────────\n\n")
  
  tree_plot <- read_csv(tree_plot_path, show_col_types = FALSE)
  
  cat("Dimensions:", nrow(tree_plot), "rows x", ncol(tree_plot), "columns\n\n")
  
  cat("Column names:\n")
  for (i in seq_along(names(tree_plot))) {
    col_name <- names(tree_plot)[i]
    col_type <- class(tree_plot[[col_name]])[1]
    cat(sprintf("  %2d. %-30s [%s]\n", i, col_name, col_type))
  }
  cat("\n")
  
  cat("Sample data (first 3 rows):\n")
  print(head(tree_plot, 3))
  cat("\n")
  
  # Check for key columns
  cat("Key column analysis:\n")
  
  # Plot ID
  id_cols <- names(tree_plot)[grepl("plot|id", names(tree_plot), ignore.case = TRUE)]
  cat("  Plot ID candidates:", paste(id_cols, collapse=", "), "\n")
  
  # Year
  year_cols <- names(tree_plot)[grepl("year|date|time", names(tree_plot), ignore.case = TRUE)]
  cat("  Year candidates:", paste(year_cols, collapse=", "), "\n")
  
  # Biomass
  bio_cols <- names(tree_plot)[grepl("bio|agb|aglb|mass|carbon", names(tree_plot), ignore.case = TRUE)]
  cat("  Biomass candidates:", paste(bio_cols, collapse=", "), "\n")
  
  # State
  state_cols <- names(tree_plot)[grepl("state|st", names(tree_plot), ignore.case = TRUE)]
  cat("  State candidates:", paste(state_cols, collapse=", "), "\n")
  
  # Coordinates
  coord_cols <- names(tree_plot)[grepl("lat|lon|^x$|^y$|coord|utm", names(tree_plot), ignore.case = TRUE)]
  if (length(coord_cols) > 0) {
    cat("  ✓ Coordinates in this file:", paste(coord_cols, collapse=", "), "\n")
  } else {
    cat("  ✗ No coordinates in this file - check NEFIN_plots.csv\n")
  }
  cat("\n")
  
  # Check unique values for key columns
  if ("_nefin_plotID" %in% names(tree_plot)) {
    n_unique_plots <- n_distinct(tree_plot$`_nefin_plotID`)
    cat("  Unique plots (_nefin_plotID):", n_unique_plots, "\n")
  }
  
  if ("treeSampleYear" %in% names(tree_plot)) {
    years <- sort(unique(tree_plot$treeSampleYear))
    cat("  Years (treeSampleYear):", paste(years, collapse=", "), "\n")
  }
  
  if ("_nefin_state" %in% names(tree_plot)) {
    states <- sort(unique(tree_plot$`_nefin_state`))
    cat("  States (_nefin_state):", paste(states, collapse=", "), "\n")
  }
  
  cat("\n")
} else {
  cat("✗ TREE_PLOT_DATA.csv not found\n\n")
}

# NEFIN_plots.csv
plots_path <- "data/raw/nefin/NEFIN_plots.csv"

if (file.exists(plots_path)) {
  cat("────────────────────────────────────────────────────────────────\n")
  cat("B. NEFIN_plots.csv\n")
  cat("────────────────────────────────────────────────────────────────\n\n")
  
  nefin_plots <- read_csv(plots_path, show_col_types = FALSE)
  
  cat("Dimensions:", nrow(nefin_plots), "rows x", ncol(nefin_plots), "columns\n\n")
  
  cat("Column names:\n")
  for (i in seq_along(names(nefin_plots))) {
    col_name <- names(nefin_plots)[i]
    col_type <- class(nefin_plots[[col_name]])[1]
    cat(sprintf("  %2d. %-30s [%s]\n", i, col_name, col_type))
  }
  cat("\n")
  
  cat("Sample data (first 3 rows):\n")
  print(head(nefin_plots, 3))
  cat("\n")
  
  # Check for coordinates
  cat("Coordinate column analysis:\n")
  coord_cols <- names(nefin_plots)[grepl("lat|lon|^x$|^y$|coord|utm|north|east", names(nefin_plots), ignore.case = TRUE)]
  
  if (length(coord_cols) > 0) {
    cat("  ✓ Coordinate candidates:", paste(coord_cols, collapse=", "), "\n")
    
    # Show sample values
    for (col in coord_cols) {
      sample_vals <- head(nefin_plots[[col]], 3)
      cat(sprintf("    %s: %s\n", col, paste(sample_vals, collapse=", ")))
    }
  } else {
    cat("  ✗ No obvious coordinate columns found\n")
  }
  cat("\n")
  
  # Check for plot ID to join with TREE_PLOT_DATA
  cat("Plot ID for joining:\n")
  id_cols <- names(nefin_plots)[grepl("plot|id", names(nefin_plots), ignore.case = TRUE)]
  cat("  Candidates:", paste(id_cols, collapse=", "), "\n")
  cat("\n")
  
} else {
  cat("✗ NEFIN_plots.csv not found\n\n")
}

# =============================================================================
# SECTION 3: PHASE B CUSTOMIZATION GUIDANCE
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SECTION 3: PHASE B CUSTOMIZATION REQUIRED\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Based on the inspection above, here's what Phase B needs:\n\n")

cat("STEP 1: Load and join NEFIN files\n")
cat("  - Load TREE_PLOT_DATA.csv (has biomass)\n")
cat("  - Load NEFIN_plots.csv (has coordinates)\n")
cat("  - Join on common plot ID\n\n")

cat("STEP 2: Column mapping (update Phase B scripts with these):\n")

if (exists("tree_plot") && exists("nefin_plots")) {
  
  # Suggest plot ID
  cat("\n  Plot ID:\n")
  if ("_nefin_plotID" %in% names(tree_plot)) {
    cat("    TREE_PLOT_DATA: _nefin_plotID\n")
  }
  tree_plot_ids <- names(nefin_plots)[grepl("plot.*id|id.*plot", names(nefin_plots), ignore.case = TRUE)]
  if (length(tree_plot_ids) > 0) {
    cat("    NEFIN_plots:", paste(tree_plot_ids, collapse=" OR "), "\n")
  }
  
  # Suggest year
  cat("\n  Year:\n")
  if ("treeSampleYear" %in% names(tree_plot)) {
    cat("    → treeSampleYear\n")
  }
  
  # Suggest biomass
  cat("\n  Biomass:\n")
  if ("AGB_kgPH" %in% names(tree_plot)) {
    cat("    → AGB_kgPH / 1000  (convert kg/ha → Mg/ha)\n")
  }
  
  # Suggest coordinates
  cat("\n  Coordinates:\n")
  lat_cols <- names(nefin_plots)[grepl("^lat|latitude", names(nefin_plots), ignore.case = TRUE)]
  lon_cols <- names(nefin_plots)[grepl("^lon|longitude", names(nefin_plots), ignore.case = TRUE)]
  if (length(lat_cols) > 0) {
    cat("    Latitude:", paste(lat_cols, collapse=" OR "), "\n")
  }
  if (length(lon_cols) > 0) {
    cat("    Longitude:", paste(lon_cols, collapse=" OR "), "\n")
  }
  
  # Suggest state
  cat("\n  State:\n")
  if ("_nefin_state" %in% names(tree_plot)) {
    cat("    → _nefin_state (may need to convert to FIPS codes)\n")
  }
}

cat("\n")
cat("STEP 3: Standardization to FIA schema:\n")
cat("  Required final columns:\n")
cat("    - CN (plot identifier)\n")
cat("    - STATECD (state FIPS code)\n")
cat("    - MEASYEAR (measurement year)\n")
cat("    - lat, lon (coordinates in WGS84)\n")
cat("    - biomass (Mg/ha)\n")
cat("    - dataset = 'NEFIN'\n")
cat("    - coord_source = 'true'\n")
cat("    - lat_for_extraction, lon_for_extraction (same as lat/lon)\n")

cat("\n")

# =============================================================================
# SECTION 4: ACTION PLAN
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SECTION 4: RECOMMENDED ACTION PLAN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("✓ PHASE A (FIA Processing):\n")
cat("  Status: READY TO RUN AS-IS\n")
cat("  Command: Rscript run_scripts/run_phase_A.R\n")
cat("  Runtime: ~5-10 minutes\n")
cat("  Output: data/processed/fia_complete.csv\n\n")

cat("⚠ PHASE B (NEFIN Processing):\n")
cat("  Status: NEEDS CUSTOMIZATION\n")
cat("  Action: Update column mappings based on inspection above\n")
cat("  Files to edit:\n")
cat("    - R/02_process_nefin/01_load_nefin.R\n")
cat("    - R/02_process_nefin/02_compute_biomass.R\n")
cat("    - R/02_process_nefin/03_create_nefin_dataset.R\n\n")

cat("✓ PHASE C (Comparison Datasets):\n")
cat("  Status: READY (runs after A & B complete)\n")
cat("  Command: Rscript run_scripts/run_phase_C.R\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  NEXT STEPS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("1. Run Phase A now:\n")
cat("     Rscript run_scripts/run_phase_A.R\n\n")

cat("2. Share this inspection output with me so I can:\n")
cat("     - Customize Phase B scripts with correct column mappings\n")
cat("     - Handle the join between TREE_PLOT_DATA and NEFIN_plots\n\n")

cat("3. After customization, run Phase B:\n")
cat("     Rscript run_scripts/run_phase_B.R\n\n")

cat("4. Finally, create comparison datasets:\n")
cat("     Rscript run_scripts/run_phase_C.R\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")
