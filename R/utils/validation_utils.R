# =============================================================================
# Validation Utility Functions
# =============================================================================
# Functions for data quality checking and validation
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

library(dplyr)
library(readr)

#' Validate required columns exist
#'
#' @param df Data frame to check
#' @param required_cols Vector of required column names
#' @param dataset_name Name for error messages
#' @return TRUE if valid (stops on error)
validate_columns <- function(df, required_cols, dataset_name = "dataset") {
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(dataset_name, " missing required columns: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  message("✓ ", dataset_name, " has all required columns")
  invisible(TRUE)
}

#' Check for missing data
#'
#' @param df Data frame to check
#' @param cols Columns to check (NULL = all)
#' @param max_missing_pct Maximum allowed missing percentage
#' @return Data frame with missing data summary
check_missing_data <- function(df, cols = NULL, max_missing_pct = 10) {
  
  if (is.null(cols)) {
    cols <- names(df)
  }
  
  missing_summary <- data.frame(
    column = cols,
    n_missing = sapply(cols, function(col) sum(is.na(df[[col]]))),
    pct_missing = sapply(cols, function(col) 100 * mean(is.na(df[[col]])))
  ) %>%
    arrange(desc(n_missing))
  
  # Flag columns with high missing rates
  high_missing <- missing_summary %>%
    filter(pct_missing > max_missing_pct)
  
  if (nrow(high_missing) > 0) {
    warning("Columns with >", max_missing_pct, "% missing data:")
    print(high_missing)
  } else {
    message("✓ No columns with excessive missing data")
  }
  
  invisible(missing_summary)
}

#' Validate coordinate values
#'
#' @param df Data frame with lat/lon columns
#' @param lat_col Latitude column name
#' @param lon_col Longitude column name
#' @param bounds List with min/max lat/lon for valid region
#' @return Data frame with validation flags
validate_coordinates <- function(df, 
                                 lat_col = "lat", 
                                 lon_col = "lon",
                                 bounds = list(
                                   lat_min = 40, lat_max = 48,
                                   lon_min = -75, lon_max = -66
                                 )) {
  
  if (!lat_col %in% names(df)) stop("Latitude column not found: ", lat_col)
  if (!lon_col %in% names(df)) stop("Longitude column not found: ", lon_col)
  
  df <- df %>%
    mutate(
      coord_valid = !is.na(.data[[lat_col]]) & 
                   !is.na(.data[[lon_col]]) &
                   is.finite(.data[[lat_col]]) &
                   is.finite(.data[[lon_col]]) &
                   .data[[lat_col]] >= bounds$lat_min &
                   .data[[lat_col]] <= bounds$lat_max &
                   .data[[lon_col]] >= bounds$lon_min &
                   .data[[lon_col]] <= bounds$lon_max,
      
      coord_flag = case_when(
        is.na(.data[[lat_col]]) | is.na(.data[[lon_col]]) ~ "missing",
        !is.finite(.data[[lat_col]]) | !is.finite(.data[[lon_col]]) ~ "non_finite",
        .data[[lat_col]] < bounds$lat_min | .data[[lat_col]] > bounds$lat_max ~ "lat_out_of_bounds",
        .data[[lon_col]] < bounds$lon_min | .data[[lon_col]] > bounds$lon_max ~ "lon_out_of_bounds",
        TRUE ~ "ok"
      )
    )
  
  # Summary
  n_valid <- sum(df$coord_valid)
  pct_valid <- 100 * n_valid / nrow(df)
  
  message(sprintf("Coordinate validation: %d / %d valid (%.1f%%)",
                  n_valid, nrow(df), pct_valid))
  
  # Report issues
  issues <- df %>%
    count(coord_flag) %>%
    filter(coord_flag != "ok") %>%
    arrange(desc(n))
  
  if (nrow(issues) > 0) {
    message("Coordinate issues:")
    print(issues)
  } else {
    message("✓ All coordinates valid")
  }
  
  df
}

#' Check for duplicate records
#'
#' @param df Data frame to check
#' @param id_cols Columns that define unique records
#' @return Data frame with duplicates flagged
check_duplicates <- function(df, id_cols) {
  
  # Check for duplicates
  df <- df %>%
    group_by(across(all_of(id_cols))) %>%
    mutate(
      duplicate_n = n(),
      is_duplicate = duplicate_n > 1
    ) %>%
    ungroup()
  
  n_duplicates <- sum(df$is_duplicate)
  n_unique <- sum(df$duplicate_n == 1)
  
  if (n_duplicates > 0) {
    warning("Found ", n_duplicates, " duplicate records")
    
    # Show example duplicates
    examples <- df %>%
      filter(is_duplicate) %>%
      select(all_of(id_cols), duplicate_n) %>%
      distinct() %>%
      head(5)
    
    message("Example duplicates:")
    print(examples)
  } else {
    message("✓ No duplicate records found")
  }
  
  df
}

#' Validate data ranges
#'
#' Check that numeric columns are within expected ranges
#'
#' @param df Data frame to check
#' @param range_specs List of lists with col, min, max
#' @return Data frame with validation summary
validate_ranges <- function(df, range_specs) {
  
  results <- list()
  
  for (spec in range_specs) {
    col <- spec$col
    min_val <- spec$min
    max_val <- spec$max
    
    if (!col %in% names(df)) {
      warning("Column not found: ", col)
      next
    }
    
    values <- df[[col]]
    n_below <- sum(values < min_val, na.rm = TRUE)
    n_above <- sum(values > max_val, na.rm = TRUE)
    n_valid <- sum(values >= min_val & values <= max_val, na.rm = TRUE)
    
    results[[col]] <- data.frame(
      column = col,
      min_expected = min_val,
      max_expected = max_val,
      n_below = n_below,
      n_above = n_above,
      n_valid = n_valid,
      pct_valid = 100 * n_valid / sum(!is.na(values))
    )
  }
  
  results_df <- bind_rows(results)
  
  # Report issues
  issues <- results_df %>%
    filter(n_below > 0 | n_above > 0)
  
  if (nrow(issues) > 0) {
    message("Range validation issues:")
    print(issues)
  } else {
    message("✓ All values within expected ranges")
  }
  
  invisible(results_df)
}

#' Compare two datasets for consistency
#'
#' @param df1 First dataset
#' @param df2 Second dataset
#' @param name1 Name of first dataset
#' @param name2 Name of second dataset
#' @return Comparison summary
compare_datasets <- function(df1, df2, name1 = "dataset1", name2 = "dataset2") {
  
  comparison <- list(
    n_rows = c(nrow(df1), nrow(df2)),
    n_cols = c(ncol(df1), ncol(df2)),
    common_cols = intersect(names(df1), names(df2)),
    unique_cols_1 = setdiff(names(df1), names(df2)),
    unique_cols_2 = setdiff(names(df2), names(df1))
  )
  
  cat("\n")
  cat("═══════════════════════════════════════\n")
  cat("  DATASET COMPARISON\n")
  cat("═══════════════════════════════════════\n\n")
  
  cat(name1, ":\n")
  cat("  Rows:", comparison$n_rows[1], "\n")
  cat("  Cols:", comparison$n_cols[1], "\n\n")
  
  cat(name2, ":\n")
  cat("  Rows:", comparison$n_rows[2], "\n")
  cat("  Cols:", comparison$n_cols[2], "\n\n")
  
  cat("Common columns:", length(comparison$common_cols), "\n")
  
  if (length(comparison$unique_cols_1) > 0) {
    cat("\nUnique to", name1, ":\n")
    cat(" ", paste(comparison$unique_cols_1, collapse = ", "), "\n")
  }
  
  if (length(comparison$unique_cols_2) > 0) {
    cat("\nUnique to", name2, ":\n")
    cat(" ", paste(comparison$unique_cols_2, collapse = ", "), "\n")
  }
  
  cat("\n═══════════════════════════════════════\n\n")
  
  invisible(comparison)
}

#' Validate file exists and is readable
#'
#' @param file_path Path to file
#' @param file_type Expected file type for message
#' @return TRUE if valid (stops on error)
validate_file_exists <- function(file_path, file_type = "file") {
  
  if (!file.exists(file_path)) {
    stop(file_type, " not found: ", file_path)
  }
  
  # Check if readable
  tryCatch({
    test <- readLines(file_path, n = 1, warn = FALSE)
    message("✓ ", file_type, " found: ", basename(file_path))
  }, error = function(e) {
    stop(file_type, " exists but is not readable: ", file_path)
  })
  
  invisible(TRUE)
}

#' Validate dataset schema matches expected
#'
#' @param df Data frame to validate
#' @param expected_schema List with col names and types
#' @param dataset_name Name for messages
#' @return TRUE if valid (warns on mismatch)
validate_schema <- function(df, expected_schema, dataset_name = "dataset") {
  
  all_valid <- TRUE
  
  for (col_spec in expected_schema) {
    col_name <- col_spec$name
    col_type <- col_spec$type
    
    # Check column exists
    if (!col_name %in% names(df)) {
      warning(dataset_name, " missing column: ", col_name)
      all_valid <- FALSE
      next
    }
    
    # Check type matches
    actual_type <- class(df[[col_name]])[1]
    if (actual_type != col_type) {
      warning(dataset_name, " column ", col_name, 
              " has type ", actual_type, " (expected ", col_type, ")")
      all_valid <- FALSE
    }
  }
  
  if (all_valid) {
    message("✓ ", dataset_name, " schema validated")
  }
  
  invisible(all_valid)
}

#' Run comprehensive validation suite
#'
#' @param df Data frame to validate
#' @param dataset_name Name for messages
#' @param required_cols Required column names
#' @param id_cols Columns defining unique records
#' @return Validation report
run_validation_suite <- function(df, 
                                dataset_name = "dataset",
                                required_cols = NULL,
                                id_cols = NULL) {
  
  cat("\n")
  cat("═══════════════════════════════════════\n")
  cat("  VALIDATION SUITE:", dataset_name, "\n")
  cat("═══════════════════════════════════════\n\n")
  
  report <- list()
  
  # Basic stats
  cat("Basic Statistics:\n")
  cat("  Rows:", nrow(df), "\n")
  cat("  Cols:", ncol(df), "\n\n")
  
  # Column validation
  if (!is.null(required_cols)) {
    cat("Checking required columns...\n")
    report$columns_valid <- tryCatch({
      validate_columns(df, required_cols, dataset_name)
      TRUE
    }, error = function(e) {
      message("✗ ", e$message)
      FALSE
    })
  }
  
  # Missing data
  cat("\nChecking for missing data...\n")
  report$missing_summary <- check_missing_data(df, max_missing_pct = 10)
  
  # Duplicates
  if (!is.null(id_cols)) {
    cat("\nChecking for duplicates...\n")
    df <- check_duplicates(df, id_cols)
    report$n_duplicates <- sum(df$is_duplicate)
  }
  
  # Coordinates (if present)
  if (all(c("lat", "lon") %in% names(df))) {
    cat("\nValidating coordinates...\n")
    df <- validate_coordinates(df)
    report$coord_valid_pct <- 100 * mean(df$coord_valid)
  }
  
  # Biomass (if present)
  if ("biomass" %in% names(df)) {
    cat("\nValidating biomass...\n")
    source("R/utils/biomass_utils.R")
    df <- validate_biomass(df)
    report$biomass_valid_pct <- 100 * mean(df$biomass_valid)
  }
  
  cat("\n═══════════════════════════════════════\n")
  cat("  VALIDATION COMPLETE\n")
  cat("═══════════════════════════════════════\n\n")
  
  invisible(report)
}

#' Write validation report to file
#'
#' @param validation_report Output from validation functions
#' @param output_path Path for output text file
write_validation_report <- function(validation_report, output_path) {
  
  sink(output_path)
  
  cat("═══════════════════════════════════════\n")
  cat("  DATA VALIDATION REPORT\n")
  cat("═══════════════════════════════════════\n\n")
  cat("Generated:", as.character(Sys.time()), "\n\n")
  
  for (item_name in names(validation_report)) {
    cat("\n", item_name, ":\n")
    print(validation_report[[item_name]])
    cat("\n")
  }
  
  sink()
  
  message("Validation report written to: ", output_path)
}
