# =============================================================================
# Extract FIA Data from SQLite Databases
# =============================================================================
# Reads state-level FIA SQLite databases and extracts required tables
#
# INPUTS:
#   - data/raw/fia_sqlite/*/SQLite_FIADB_*.db
#
# OUTPUTS:
#   - data/interim/fia/extracted/plot.csv
#   - data/interim/fia/extracted/tree.csv
#   - data/interim/fia/extracted/cond.csv
#   - data/interim/fia/extracted/extraction_summary.txt
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

# Load configuration and utilities
source("R/00_config/config.R")
source("R/utils/file_utils.R")
source("R/utils/validation_utils.R")

# Load required packages
suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(readr)
  library(purrr)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  STEP 1: EXTRACT FIA DATA FROM SQLITE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Output directory
output_dir <- file.path(CONFIG$paths$interim_fia, "extracted")
ensure_dir(output_dir)

# Tables to extract
tables_to_extract <- c("PLOT", "TREE", "COND")

# Optional tables (may not exist in all databases)
optional_tables <- c("SEEDLING", "POP_STRATUM", "POP_EVAL", "POP_EVAL_TYP")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Connect to FIA SQLite database
#'
#' @param db_path Path to SQLite database
#' @return Database connection
connect_fia_db <- function(db_path) {
  
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path)
  }
  
  conn <- dbConnect(SQLite(), db_path)
  conn
}

#' Extract table from FIA database
#'
#' @param conn Database connection
#' @param table_name Table to extract
#' @param state_abbr State abbreviation for filtering
#' @return Data frame with table contents
extract_table <- function(conn, table_name, state_abbr = NULL) {
  
  # Check if table exists
  tables <- dbListTables(conn)
  if (!table_name %in% tables) {
    warning("Table ", table_name, " not found in database")
    return(NULL)
  }
  
  # Build query with year and state filters
  query <- sprintf("SELECT * FROM %s WHERE 1=1", table_name)
  
  # Add year filter
  query <- paste(query, 
                sprintf("AND MEASYEAR >= %d AND MEASYEAR <= %d",
                       CONFIG$year_start, CONFIG$year_end))
  
  # Add state filter if provided
  if (!is.null(state_abbr) && state_abbr %in% names(CONFIG$state_codes)) {
    state_code <- CONFIG$state_codes[[state_abbr]]
    query <- paste(query, sprintf("AND STATECD = %d", state_code))
  }
  
  # Execute query
  df <- dbGetQuery(conn, query)
  
  # Additional filtering for COND table (forested only)
  if (table_name == "COND" && CONFIG$fia$forested_only) {
    # FORTYPCD > 0 indicates forested condition
    if ("FORTYPCD" %in% names(df)) {
      df <- df %>% filter(FORTYPCD > 0)
    }
  }
  
  df
}

#' Process single state database
#'
#' @param state_abbr State abbreviation
#' @param base_dir Base directory for SQLite databases
#' @return List of extracted tables
process_state <- function(state_abbr, base_dir) {
  
  cat("\n")
  cat("───────────────────────────────────────\n")
  cat("Processing:", state_abbr, "\n")
  cat("───────────────────────────────────────\n")
  
  # Find database file
  db_pattern <- sprintf("SQLite_FIADB_%s.db", state_abbr)
  db_path <- file.path(base_dir, state_abbr, "unzipped", db_pattern)
  
  if (!file.exists(db_path)) {
    warning("Database not found: ", db_path)
    return(NULL)
  }
  
  # Connect to database
  conn <- connect_fia_db(db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  # Extract tables
  tables <- list()
  
  for (table_name in tables_to_extract) {
    cat("  Extracting", table_name, "... ")
    
    df <- extract_table(conn, table_name, state_abbr)
    
    if (!is.null(df) && nrow(df) > 0) {
      tables[[table_name]] <- df
      cat(nrow(df), "rows\n")
    } else {
      cat("no data\n")
    }
  }
  
  tables
}

# =============================================================================
# MAIN PROCESSING
# =============================================================================

cat("Processing states:", paste(CONFIG$states, collapse = ", "), "\n")
cat("Year range:", CONFIG$year_start, "-", CONFIG$year_end, "\n\n")

# Process all states
all_tables <- map(CONFIG$states, ~process_state(.x, CONFIG$paths$raw_fia))

# Combine states for each table
cat("\n")
cat("═══════════════════════════════════════\n")
cat("Combining States\n")
cat("═══════════════════════════════════════\n\n")

combined_tables <- list()

for (table_name in tables_to_extract) {
  cat("Combining", table_name, "...")
  
  # Extract this table from all states
  state_tables <- map(all_tables, ~.x[[table_name]])
  state_tables <- compact(state_tables)  # Remove NULLs
  
  if (length(state_tables) > 0) {
    # Combine
    combined <- bind_rows(state_tables)
    combined_tables[[table_name]] <- combined
    cat(" ", nrow(combined), "rows\n")
  } else {
    cat(" no data\n")
  }
}

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════\n")
cat("Saving Combined Tables\n")
cat("═══════════════════════════════════════\n\n")

output_files <- list()

for (table_name in names(combined_tables)) {
  df <- combined_tables[[table_name]]
  
  # Output path
  output_path <- file.path(output_dir, paste0(tolower(table_name), ".csv"))
  
  # Save
  write_csv(df, output_path)
  output_files[[table_name]] <- output_path
  
  cat("✓ Saved:", basename(output_path), "-", nrow(df), "rows\n")
}

# =============================================================================
# VALIDATION
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════\n")
cat("Validation\n")
cat("═══════════════════════════════════════\n\n")

# Check required tables exist
for (table_name in tables_to_extract) {
  if (table_name %in% names(combined_tables)) {
    cat("✓", table_name, "extracted successfully\n")
  } else {
    stop("✗ Required table missing: ", table_name)
  }
}

# Check key columns exist
plot_df <- combined_tables$PLOT
tree_df <- combined_tables$TREE
cond_df <- combined_tables$COND

required_plot_cols <- c("CN", "LAT", "LON", "MEASYEAR", "STATECD")
required_tree_cols <- c("CN", "PLT_CN", "DRYBIO_AG", "TPA_UNADJ", "DIA", "STATUSCD")
required_cond_cols <- c("CN", "PLT_CN", "CONDID", "FORTYPCD")

validate_columns(plot_df, required_plot_cols, "PLOT")
validate_columns(tree_df, required_tree_cols, "TREE")
validate_columns(cond_df, required_cond_cols, "COND")

# =============================================================================
# SUMMARY REPORT
# =============================================================================

summary_path <- file.path(output_dir, "extraction_summary.txt")

sink(summary_path)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  FIA DATA EXTRACTION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Run time:", as.character(Sys.time()), "\n")
cat("States processed:", paste(CONFIG$states, collapse = ", "), "\n")
cat("Year range:", CONFIG$year_start, "-", CONFIG$year_end, "\n")
cat("Forested only:", CONFIG$fia$forested_only, "\n\n")

cat("TABLES EXTRACTED:\n\n")

for (table_name in names(combined_tables)) {
  df <- combined_tables[[table_name]]
  cat(sprintf("%-10s: %7d rows, %2d columns\n", 
              table_name, nrow(df), ncol(df)))
}

cat("\n")
cat("PLOT DISTRIBUTION BY STATE:\n\n")
plot_by_state <- plot_df %>%
  count(STATECD) %>%
  arrange(desc(n))

for (i in 1:nrow(plot_by_state)) {
  state_code <- plot_by_state$STATECD[i]
  state_name <- names(CONFIG$state_codes)[match(state_code, CONFIG$state_codes)]
  cat(sprintf("  %s (%2d): %5d plots\n", 
              state_name, state_code, plot_by_state$n[i]))
}

cat("\n")
cat("PLOT DISTRIBUTION BY YEAR:\n\n")
plot_by_year <- plot_df %>%
  count(MEASYEAR) %>%
  arrange(MEASYEAR)

for (i in 1:nrow(plot_by_year)) {
  cat(sprintf("  %d: %5d plots\n", 
              plot_by_year$MEASYEAR[i], plot_by_year$n[i]))
}

cat("\n")
cat("TREE STATISTICS:\n\n")
cat(sprintf("  Total trees:      %7d\n", nrow(tree_df)))
cat(sprintf("  Live trees:       %7d\n", sum(tree_df$STATUSCD == 1, na.rm = TRUE)))
cat(sprintf("  Dead trees:       %7d\n", sum(tree_df$STATUSCD == 2, na.rm = TRUE)))
cat(sprintf("  Trees with DBH:   %7d\n", sum(!is.na(tree_df$DIA))))
cat(sprintf("  Trees with biomass: %7d\n", sum(!is.na(tree_df$DRYBIO_AG))))

cat("\n")
cat("OUTPUT FILES:\n\n")
for (path in output_files) {
  cat("  ", path, "\n", sep = "")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")

sink()

cat("\n✓ Summary written:", summary_path, "\n")

# =============================================================================
# COMPLETE
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  EXTRACTION COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Next step: Run R/01_process_fia/02_compute_biomass.R\n\n")
