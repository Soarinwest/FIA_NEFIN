# =============================================================================
# File Utility Functions
# =============================================================================
# Functions for file I/O, path handling, and data management
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

library(readr)
library(fs)

#' Create directory if it doesn't exist
#'
#' @param dir_path Directory path to create
#' @param verbose Print message (default: TRUE)
#' @return Path (invisibly)
ensure_dir <- function(dir_path, verbose = TRUE) {
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    if (verbose) {
      message("Created directory: ", dir_path)
    }
  }
  
  invisible(dir_path)
}

#' Save data frame with metadata
#'
#' @param df Data frame to save
#' @param file_path Output path
#' @param metadata List of metadata to save alongside
#' @param overwrite Allow overwriting existing file
#' @return File path (invisibly)
save_with_metadata <- function(df, file_path, metadata = NULL, overwrite = FALSE) {
  
  # Check if file exists
  if (file.exists(file_path) && !overwrite) {
    stop("File already exists (use overwrite = TRUE): ", file_path)
  }
  
  # Ensure output directory exists
  ensure_dir(dirname(file_path), verbose = FALSE)
  
  # Save main data
  write_csv(df, file_path)
  message("✓ Saved: ", file_path, " (", nrow(df), " rows)")
  
  # Save metadata if provided
  if (!is.null(metadata)) {
    meta_path <- paste0(tools::file_path_sans_ext(file_path), "_metadata.txt")
    
    sink(meta_path)
    cat("═══════════════════════════════════════\n")
    cat("  FILE METADATA\n")
    cat("═══════════════════════════════════════\n\n")
    cat("File:", basename(file_path), "\n")
    cat("Created:", as.character(Sys.time()), "\n")
    cat("Rows:", nrow(df), "\n")
    cat("Columns:", ncol(df), "\n\n")
    
    if (length(metadata) > 0) {
      cat("Additional metadata:\n")
      for (key in names(metadata)) {
        cat("  ", key, ": ", metadata[[key]], "\n", sep = "")
      }
    }
    sink()
    
    message("✓ Metadata saved: ", meta_path)
  }
  
  invisible(file_path)
}

#' Load CSV with validation
#'
#' @param file_path Path to CSV file
#' @param required_cols Required column names (NULL = no check)
#' @param verbose Print messages
#' @return Data frame
load_csv_validated <- function(file_path, required_cols = NULL, verbose = TRUE) {
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  if (verbose) {
    message("Loading: ", basename(file_path))
  }
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  if (verbose) {
    message("  Loaded ", nrow(df), " rows, ", ncol(df), " columns")
  }
  
  # Validate columns if specified
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    if (verbose) {
      message("  ✓ All required columns present")
    }
  }
  
  df
}

#' Get timestamped output directory
#'
#' Creates directory with timestamp for reproducibility
#'
#' @param base_dir Base directory
#' @param prefix Prefix for directory name
#' @param create Create directory immediately
#' @return Directory path
get_timestamped_dir <- function(base_dir, prefix = "run", create = TRUE) {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir_name <- paste0(prefix, "_", timestamp)
  dir_path <- file.path(base_dir, dir_name)
  
  if (create) {
    ensure_dir(dir_path)
  }
  
  dir_path
}

#' List files matching pattern
#'
#' @param dir_path Directory to search
#' @param pattern Regex pattern to match
#' @param full_names Return full paths (default: TRUE)
#' @return Vector of file paths
list_files_pattern <- function(dir_path, pattern, full_names = TRUE) {
  
  if (!dir.exists(dir_path)) {
    stop("Directory not found: ", dir_path)
  }
  
  files <- list.files(
    dir_path,
    pattern = pattern,
    full.names = full_names,
    recursive = FALSE
  )
  
  if (length(files) == 0) {
    warning("No files found matching pattern: ", pattern)
  }
  
  files
}

#' Copy file with backup
#'
#' If destination exists, creates backup with timestamp
#'
#' @param from Source file
#' @param to Destination file
#' @param backup Create backup if exists
#' @return Destination path (invisibly)
copy_with_backup <- function(from, to, backup = TRUE) {
  
  if (!file.exists(from)) {
    stop("Source file not found: ", from)
  }
  
  # Check if destination exists
  if (file.exists(to) && backup) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_name <- paste0(
      tools::file_path_sans_ext(to),
      "_backup_", timestamp, ".",
      tools::file_ext(to)
    )
    
    file.copy(to, backup_name)
    message("Created backup: ", basename(backup_name))
  }
  
  # Copy file
  file.copy(from, to, overwrite = TRUE)
  message("Copied: ", basename(from), " → ", basename(to))
  
  invisible(to)
}

#' Get file size in human-readable format
#'
#' @param file_path Path to file
#' @return String with file size
get_file_size <- function(file_path) {
  
  if (!file.exists(file_path)) {
    return("File not found")
  }
  
  size_bytes <- file.size(file_path)
  
  if (size_bytes < 1024) {
    return(paste(size_bytes, "bytes"))
  } else if (size_bytes < 1024^2) {
    return(sprintf("%.1f KB", size_bytes / 1024))
  } else if (size_bytes < 1024^3) {
    return(sprintf("%.1f MB", size_bytes / 1024^2))
  } else {
    return(sprintf("%.1f GB", size_bytes / 1024^3))
  }
}

#' Archive old files
#'
#' Move files older than threshold to archive directory
#'
#' @param dir_path Directory to clean
#' @param age_days Files older than this (days)
#' @param archive_dir Archive directory (default: dir_path/archive)
#' @return Number of files archived
archive_old_files <- function(dir_path, age_days = 30, archive_dir = NULL) {
  
  if (!dir.exists(dir_path)) {
    stop("Directory not found: ", dir_path)
  }
  
  if (is.null(archive_dir)) {
    archive_dir <- file.path(dir_path, "archive")
  }
  
  ensure_dir(archive_dir, verbose = FALSE)
  
  # Get all files
  files <- list.files(dir_path, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]  # Exclude directories
  
  # Check age
  cutoff_date <- Sys.time() - (age_days * 24 * 60 * 60)
  old_files <- files[file.mtime(files) < cutoff_date]
  
  if (length(old_files) > 0) {
    for (file in old_files) {
      new_path <- file.path(archive_dir, basename(file))
      file.rename(file, new_path)
    }
    message("Archived ", length(old_files), " files older than ", age_days, " days")
  } else {
    message("No files older than ", age_days, " days")
  }
  
  invisible(length(old_files))
}

#' Write processing log
#'
#' @param log_path Path to log file
#' @param message Message to log
#' @param append Append to existing log (default: TRUE)
write_log <- function(log_path, message, append = TRUE) {
  
  ensure_dir(dirname(log_path), verbose = FALSE)
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", message, "\n")
  
  cat(log_entry, file = log_path, append = append)
}

#' Get config value
#'
#' Safe accessor for config values with defaults
#'
#' @param key Config key (can be nested with $)
#' @param default Default value if not found
#' @return Config value or default
get_config_value <- function(key, default = NULL) {
  
  if (!exists("CONFIG")) {
    stop("CONFIG not found. Source R/00_config/config.R first.")
  }
  
  # Handle nested keys (e.g., "paths$raw_fia")
  keys <- strsplit(key, "\\$")[[1]]
  
  value <- CONFIG
  for (k in keys) {
    if (k %in% names(value)) {
      value <- value[[k]]
    } else {
      return(default)
    }
  }
  
  value
}

#' Create processing manifest
#'
#' Records metadata about processing run
#'
#' @param output_dir Output directory
#' @param script_name Name of script that ran
#' @param inputs List of input files used
#' @param outputs List of output files created
#' @return Manifest path (invisibly)
create_manifest <- function(output_dir, script_name, inputs = NULL, outputs = NULL) {
  
  ensure_dir(output_dir, verbose = FALSE)
  
  manifest_path <- file.path(output_dir, "manifest.txt")
  
  sink(manifest_path)
  
  cat("═══════════════════════════════════════\n")
  cat("  PROCESSING MANIFEST\n")
  cat("═══════════════════════════════════════\n\n")
  
  cat("Script:", script_name, "\n")
  cat("Run time:", as.character(Sys.time()), "\n")
  cat("User:", Sys.info()["user"], "\n")
  cat("R version:", R.version.string, "\n\n")
  
  if (!is.null(inputs)) {
    cat("INPUT FILES:\n")
    for (input in inputs) {
      cat("  - ", input, "\n", sep = "")
    }
    cat("\n")
  }
  
  if (!is.null(outputs)) {
    cat("OUTPUT FILES:\n")
    for (output in outputs) {
      cat("  - ", output, " (", get_file_size(output), ")\n", sep = "")
    }
    cat("\n")
  }
  
  cat("═══════════════════════════════════════\n")
  
  sink()
  
  message("✓ Manifest written: ", manifest_path)
  invisible(manifest_path)
}

#' Check disk space
#'
#' Warn if low on disk space
#'
#' @param path Path to check
#' @param min_gb_free Minimum free space (GB)
#' @return Available space in GB
check_disk_space <- function(path = ".", min_gb_free = 10) {
  
  # This is platform-specific - works on Unix-like systems
  tryCatch({
    df_output <- system(paste("df", shQuote(path)), intern = TRUE)
    space_line <- df_output[2]
    free_kb <- as.numeric(strsplit(space_line, "\\s+")[[1]][4])
    free_gb <- free_kb / (1024^2)
    
    if (free_gb < min_gb_free) {
      warning("Low disk space: ", round(free_gb, 1), " GB available (", 
              round(free_gb / min_gb_free * 100, 0), "% of minimum)")
    }
    
    invisible(free_gb)
  }, error = function(e) {
    # Can't check on this platform
    invisible(NA)
  })
}
