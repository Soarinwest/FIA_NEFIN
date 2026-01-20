# =============================================================================
# Extract FIA Data from SQLite Databases - FINAL FIX
# =============================================================================
# FIXED: Uses CONFIG$year_start and CONFIG$year_end (not year_range)
# FIXED: Uses CONFIG$states and CONFIG$state_codes (correct structure)
# FIXED: TREE filtered by PLT_CN not MEASYEAR
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/validation_utils.R")

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(readr)
  library(purrr)
})

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  STEP 1: EXTRACT FIA DATA FROM SQLITE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

output_dir <- file.path(CONFIG$paths$interim_fia, "extracted")
ensure_dir(output_dir)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

process_state <- function(state_abbr, base_dir) {
  
  cat("\n───────────────────────────────────────\n")
  cat("Processing:", state_abbr, "\n")
  cat("───────────────────────────────────────\n")
  
  # Get state code
  state_code <- CONFIG$state_codes[[state_abbr]]
  
  # Find database
  db_path <- file.path(base_dir, state_abbr, "unzipped", 
                       sprintf("SQLite_FIADB_%s.db", state_abbr))
  
  if (!file.exists(db_path)) {
    warning("Database not found: ", db_path)
    return(NULL)
  }
  
  # Connect
  conn <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  tables <- list()
  
  # STEP 1: Extract PLOT with year filter
  cat("  Extracting PLOT ... ")
  plot_query <- sprintf(
    "SELECT * FROM PLOT WHERE MEASYEAR >= %d AND MEASYEAR <= %d AND STATECD = %d",
    CONFIG$year_start, CONFIG$year_end, state_code
  )
  plot_df <- dbGetQuery(conn, plot_query)
  cat(nrow(plot_df), "rows\n")
  tables$PLOT <- plot_df
  
  if (nrow(plot_df) == 0) {
    cat("  ⚠ No plots found for this state/year range\n")
    return(tables)
  }
  
  # STEP 2: Extract TREE (filter by PLT_CN, not by year)
  cat("  Extracting TREE ... ")
  plot_cns <- plot_df$CN
  plot_cns_str <- paste0("'", plot_cns, "'", collapse = ",")
  
  tree_query <- sprintf(
    "SELECT * FROM TREE WHERE PLT_CN IN (%s) AND STATECD = %d",
    plot_cns_str, state_code
  )
  tree_df <- dbGetQuery(conn, tree_query)
  cat(nrow(tree_df), "rows\n")
  tables$TREE <- tree_df
  
  # STEP 3: Extract COND
  cat("  Extracting COND ... ")
  cond_query <- sprintf(
    "SELECT * FROM COND WHERE PLT_CN IN (%s) AND STATECD = %d",
    plot_cns_str, state_code
  )
  cond_df <- dbGetQuery(conn, cond_query)
  
  # Filter to forested
  if ("FORTYPCD" %in% names(cond_df) && CONFIG$fia$forested_only) {
    cond_df <- cond_df %>% filter(FORTYPCD > 0)
  }
  
  cat(nrow(cond_df), "rows\n")
  tables$COND <- cond_df
  
  tables
}

# =============================================================================
# MAIN PROCESSING
# =============================================================================

cat("Processing states:", paste(CONFIG$states, collapse = ", "), "\n")
cat("Year range:", CONFIG$year_start, "-", CONFIG$year_end, "\n")

all_tables <- map(CONFIG$states, ~process_state(.x, CONFIG$paths$raw_fia))

# Combine
cat("\n═══════════════════════════════════════\n")
cat("Combining States\n")
cat("═══════════════════════════════════════\n\n")

combined_tables <- list()
for (table_name in c("PLOT", "TREE", "COND")) {
  cat("Combining", table_name, "...")
  state_tables <- compact(map(all_tables, ~.x[[table_name]]))
  if (length(state_tables) > 0) {
    combined <- bind_rows(state_tables)
    combined_tables[[table_name]] <- combined
    cat(" ", nrow(combined), "rows\n")
  }
}

# Save
cat("\n═══════════════════════════════════════\n")
cat("Saving\n")
cat("═══════════════════════════════════════\n\n")

for (table_name in names(combined_tables)) {
  df <- combined_tables[[table_name]]
  output_path <- file.path(output_dir, paste0(tolower(table_name), ".csv"))
  write_csv(df, output_path)
  cat("✓ Saved:", basename(output_path), "-", nrow(df), "rows\n")
}

# Summary
cat("\n═══════════════════════════════════════\n")
cat("Summary\n")
cat("═══════════════════════════════════════\n\n")

if (length(combined_tables) > 0) {
  plot_df <- combined_tables$PLOT
  tree_df <- combined_tables$TREE
  
  cat("PLOT:\n")
  cat("  Total plots:", nrow(plot_df), "\n")
  cat("  States:", paste(sort(unique(plot_df$STATECD)), collapse = ", "), "\n")
  cat("  Years:", paste(sort(unique(plot_df$MEASYEAR)), collapse = ", "), "\n\n")
  
  cat("TREE:\n")
  cat("  Total trees:", nrow(tree_df), "\n")
  cat("  Live trees:", sum(tree_df$STATUSCD == 1, na.rm = TRUE), "\n\n")
}

cat("✓ Extraction complete!\n")
cat("Next: R/01_process_fia/02_compute_biomass.R\n\n")