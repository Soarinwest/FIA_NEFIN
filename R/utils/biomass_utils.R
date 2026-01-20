# =============================================================================
# Biomass Utility Functions
# =============================================================================
# Functions for biomass calculations and conversions
#
# Author: Soren Donisvitch
# Updated: January 2026
# =============================================================================

library(dplyr)

#' Convert FIA tree biomass to plot-level Mg/ha
#'
#' FIA stores biomass as DRYBIO_AG (pounds) at tree level with expansion factors.
#' This function aggregates to plot level and converts units.
#'
#' @param tree_df Data frame with tree-level data
#' @param biomass_col Column with biomass in pounds (default: "DRYBIO_AG")
#' @param tpa_col Column with trees per acre expansion factor (default: "TPA_UNADJ")
#' @return Plot-level data frame with biomass in Mg/ha
compute_plot_biomass_fia <- function(tree_df, 
                                     biomass_col = "DRYBIO_AG",
                                     tpa_col = "TPA_UNADJ") {
  
  # Check required columns
  required_cols <- c("CN", biomass_col, tpa_col)
  missing_cols <- setdiff(required_cols, names(tree_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Calculate expanded biomass per tree (pounds per acre)
  tree_df <- tree_df %>%
    mutate(
      biomass_expanded_lb_ac = .data[[biomass_col]] * .data[[tpa_col]]
    )
  
  # Aggregate to plot level
  plot_biomass <- tree_df %>%
    group_by(CN) %>%
    summarise(
      biomass_lb_ac = sum(biomass_expanded_lb_ac, na.rm = TRUE),
      n_trees = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # Convert pounds/acre to Mg/ha
      biomass = biomass_lb_ac * CONFIG$fia$lb_per_acre_to_Mg_per_ha
    ) %>%
    select(CN, biomass, n_trees)
  
  plot_biomass
}

#' Convert tree-level biomass to plot-level
#'
#' Generic function for tree → plot aggregation.
#' Assumes biomass is already in correct units (Mg/ha or needs conversion)
#'
#' @param tree_df Data frame with tree-level data
#' @param plot_id_col Column with plot identifier
#' @param biomass_col Column with tree biomass
#' @param conversion_factor Factor to convert to Mg/ha (default: 1)
#' @return Plot-level biomass data frame
aggregate_tree_to_plot <- function(tree_df, 
                                   plot_id_col = "PLT_CN",
                                   biomass_col = "biomass",
                                   conversion_factor = 1) {
  
  tree_df %>%
    group_by(.data[[plot_id_col]]) %>%
    summarise(
      biomass = sum(.data[[biomass_col]], na.rm = TRUE) * conversion_factor,
      n_trees = n(),
      .groups = "drop"
    ) %>%
    rename(plot_id = all_of(plot_id_col))
}

#' Validate biomass values
#'
#' Check for unrealistic or problematic biomass values
#'
#' @param df Data frame with biomass column
#' @param biomass_col Name of biomass column
#' @param min_valid Minimum valid biomass (Mg/ha)
#' @param max_valid Maximum valid biomass (Mg/ha)
#' @return Data frame with validation columns added
validate_biomass <- function(df, 
                            biomass_col = "biomass",
                            min_valid = 0,
                            max_valid = 500) {
  
  if (!biomass_col %in% names(df)) {
    stop("Biomass column not found: ", biomass_col)
  }
  
  df <- df %>%
    mutate(
      biomass_valid = !is.na(.data[[biomass_col]]) & 
                     is.finite(.data[[biomass_col]]) &
                     .data[[biomass_col]] >= min_valid &
                     .data[[biomass_col]] <= max_valid,
      
      biomass_flag = case_when(
        is.na(.data[[biomass_col]]) ~ "missing",
        !is.finite(.data[[biomass_col]]) ~ "non_finite",
        .data[[biomass_col]] < min_valid ~ "too_low",
        .data[[biomass_col]] > max_valid ~ "too_high",
        TRUE ~ "ok"
      )
    )
  
  # Report validation results
  n_total <- nrow(df)
  n_valid <- sum(df$biomass_valid)
  
  message(sprintf("Biomass validation: %d / %d valid (%.1f%%)",
                  n_valid, n_total, 100 * n_valid / n_total))
  
  # Report issues
  flag_summary <- df %>%
    count(biomass_flag) %>%
    arrange(desc(n))
  
  if (any(flag_summary$biomass_flag != "ok")) {
    message("Issues found:")
    print(flag_summary %>% filter(biomass_flag != "ok"))
  }
  
  df
}

#' Calculate biomass summary statistics
#'
#' @param df Data frame with biomass column
#' @param biomass_col Name of biomass column
#' @param group_by Optional grouping variable(s)
#' @return Summary statistics data frame
summarize_biomass <- function(df, biomass_col = "biomass", group_by = NULL) {
  
  if (!biomass_col %in% names(df)) {
    stop("Biomass column not found: ", biomass_col)
  }
  
  if (!is.null(group_by)) {
    df_grouped <- df %>% group_by(across(all_of(group_by)))
  } else {
    df_grouped <- df
  }
  
  summary_stats <- df_grouped %>%
    summarise(
      n = n(),
      mean = mean(.data[[biomass_col]], na.rm = TRUE),
      median = median(.data[[biomass_col]], na.rm = TRUE),
      sd = sd(.data[[biomass_col]], na.rm = TRUE),
      min = min(.data[[biomass_col]], na.rm = TRUE),
      max = max(.data[[biomass_col]], na.rm = TRUE),
      q25 = quantile(.data[[biomass_col]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[biomass_col]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_stats
}

#' Convert between biomass units
#'
#' @param value Biomass value
#' @param from_units Source units
#' @param to_units Target units
#' @return Converted value
convert_biomass_units <- function(value, from_units, to_units) {
  
  # Define conversion factors (all relative to Mg/ha)
  to_Mg_ha <- list(
    "Mg/ha" = 1,
    "Mg_per_ha" = 1,
    "ton/ha" = 1,
    "kg/ha" = 0.001,
    "g/m2" = 0.01,
    "lb/acre" = 0.001121,
    "ton/acre" = 2.242
  )
  
  if (!from_units %in% names(to_Mg_ha)) {
    stop("Unknown from_units: ", from_units)
  }
  if (!to_units %in% names(to_Mg_ha)) {
    stop("Unknown to_units: ", to_units)
  }
  
  # Convert to Mg/ha then to target units
  value_Mg_ha <- value * to_Mg_ha[[from_units]]
  value_out <- value_Mg_ha / to_Mg_ha[[to_units]]
  
  value_out
}

#' Calculate plot-level statistics from tree data
#'
#' @param tree_df Tree-level data frame
#' @param plot_id_col Plot identifier column
#' @return Plot-level statistics
calculate_plot_stats <- function(tree_df, plot_id_col = "CN") {
  
  tree_df %>%
    group_by(.data[[plot_id_col]]) %>%
    summarise(
      n_trees = n(),
      n_live = sum(STATUSCD == 1, na.rm = TRUE),
      n_dead = sum(STATUSCD == 2, na.rm = TRUE),
      mean_dbh = mean(DIA, na.rm = TRUE),
      max_dbh = max(DIA, na.rm = TRUE),
      basal_area = sum(pi * (DIA / 2)^2, na.rm = TRUE),  # sq inches
      .groups = "drop"
    ) %>%
    rename(plot_id = all_of(plot_id_col))
}

#' Flag extreme biomass values
#'
#' Identifies plots with unusually high or low biomass
#'
#' @param df Data frame with biomass
#' @param biomass_col Biomass column name
#' @param threshold Number of standard deviations for outlier
#' @return Data frame with outlier flag
flag_biomass_outliers <- function(df, 
                                  biomass_col = "biomass",
                                  threshold = 3) {
  
  biomass_mean <- mean(df[[biomass_col]], na.rm = TRUE)
  biomass_sd <- sd(df[[biomass_col]], na.rm = TRUE)
  
  df <- df %>%
    mutate(
      biomass_zscore = (.data[[biomass_col]] - biomass_mean) / biomass_sd,
      biomass_outlier = abs(biomass_zscore) > threshold
    )
  
  n_outliers <- sum(df$biomass_outlier, na.rm = TRUE)
  if (n_outliers > 0) {
    message("Flagged ", n_outliers, " biomass outliers (>", threshold, " SD)")
  }
  
  df
}

#' Apply expansion factors to tree biomass
#'
#' For FIA data with TPA (trees per acre) expansion factors
#'
#' @param tree_df Tree data with biomass and TPA
#' @param biomass_col Biomass column (per tree)
#' @param tpa_col Trees per acre expansion factor
#' @return Data frame with expanded biomass
apply_expansion_factors <- function(tree_df,
                                   biomass_col = "DRYBIO_AG",
                                   tpa_col = "TPA_UNADJ") {
  
  tree_df %>%
    mutate(
      biomass_expanded = .data[[biomass_col]] * .data[[tpa_col]]
    )
}

#' Calculate carbon from biomass
#'
#' Using standard conversion factor (0.5 for temperate forests)
#'
#' @param biomass Biomass in Mg/ha
#' @param carbon_fraction Carbon content (default: 0.5)
#' @return Carbon in Mg C/ha
biomass_to_carbon <- function(biomass, carbon_fraction = 0.5) {
  biomass * carbon_fraction
}

#' Biomass per unit area calculation
#'
#' For plot-level data with explicit plot size
#'
#' @param total_biomass Total biomass on plot
#' @param plot_area_m2 Plot area in square meters
#' @return Biomass density in Mg/ha
biomass_per_area <- function(total_biomass, plot_area_m2) {
  # Convert to per-hectare
  (total_biomass / plot_area_m2) * 10000
}

#' Print biomass summary
#'
#' @param df Data frame with biomass
#' @param biomass_col Biomass column name
print_biomass_summary <- function(df, biomass_col = "biomass") {
  
  stats <- summarize_biomass(df, biomass_col)
  
  cat("\n")
  cat("═══════════════════════════════════════\n")
  cat("  BIOMASS SUMMARY\n")
  cat("═══════════════════════════════════════\n\n")
  
  cat("Sample size:", stats$n, "plots\n\n")
  cat("Mean:   ", sprintf("%.2f Mg/ha\n", stats$mean))
  cat("Median: ", sprintf("%.2f Mg/ha\n", stats$median))
  cat("SD:     ", sprintf("%.2f Mg/ha\n", stats$sd))
  cat("\n")
  cat("Range:  ", sprintf("%.2f - %.2f Mg/ha\n", stats$min, stats$max))
  cat("IQR:    ", sprintf("%.2f - %.2f Mg/ha\n", stats$q25, stats$q75))
  cat("\n")
  cat("═══════════════════════════════════════\n\n")
  
  invisible(stats)
}
