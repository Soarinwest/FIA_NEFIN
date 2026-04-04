#!/usr/bin/env Rscript
# Summary & quick-view utilities for processed datasets

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(dplyr)
library(readr)
library(sf)
library(ggplot2)

dir.create(CONFIG$paths$outputs, showWarnings = FALSE, recursive = TRUE)
fig_dir <- file.path(CONFIG$paths$outputs, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

cat("SUMMARY & VIEW: processed datasets\n")

# Files
baseline_path <- "data/processed/baseline_with_covariates.csv"
aug_cov_path <- "data/processed/augmented_covariates.csv"
aug_hex_path <- "data/processed/augmented_hex_assignments.csv"

check_file <- function(path) {
  if (!file.exists(path)) {
    cat("  ⚠ Missing:", path, "\n")
    return(FALSE)
  }
  return(TRUE)
}

have_baseline <- check_file(baseline_path)
have_aug_cov <- check_file(aug_cov_path)
have_aug_hex <- check_file(aug_hex_path)

if (!have_baseline && !have_aug_cov) {
  stop("No input processed datasets found; run pipeline steps first.")
}

# Load datasets when present
if (have_baseline) baseline <- read_csv(baseline_path, show_col_types = FALSE)
if (have_aug_cov) augmented <- read_csv(aug_cov_path, show_col_types = FALSE)
if (have_aug_hex) aug_hex <- read_csv(aug_hex_path, show_col_types = FALSE)

# Basic summaries
summaries <- list()
if (have_baseline) {
  summaries$baseline <- tibble(
    dataset = "baseline",
    n_plots = nrow(baseline),
    mean_biomass = mean(baseline$biomass, na.rm = TRUE),
    sd_biomass = sd(baseline$biomass, na.rm = TRUE),
    pct_complete_covariates = 100 * mean(!is.na(baseline$ndvi_modis) & !is.na(baseline$ndvi_s2) & !is.na(baseline$tmean) & !is.na(baseline$ppt))
  )
}

if (have_aug_cov) {
  summaries$augmented <- tibble(
    dataset = "augmented",
    n_plots = nrow(augmented),
    mean_biomass = mean(augmented$biomass, na.rm = TRUE),
    sd_biomass = sd(augmented$biomass, na.rm = TRUE),
    pct_complete_covariates = 100 * mean(!is.na(augmented$ndvi_modis) & !is.na(augmented$ndvi_s2) & !is.na(augmented$tmean) & !is.na(augmented$ppt))
  )
}

summary_df <- bind_rows(summaries)
summary_out <- file.path(CONFIG$paths$outputs, "summary_dataset_overview.csv")
write_csv(summary_df, summary_out)
cat("Saved summary:", summary_out, "\n")

# Plot: spatial overview (both datasets)
points <- list()
if (have_baseline) {
  points$baseline <- st_as_sf(baseline, coords = c("lon_for_extraction", "lat_for_extraction"), crs = 4326)
}
if (have_aug_cov) {
  points$augmented <- st_as_sf(augmented, coords = c("lon_for_extraction", "lat_for_extraction"), crs = 4326)
}

if (length(points) > 0) {
  # combine with source column and preserve STATECD when available
  pts_combined <- bind_rows(
    lapply(names(points), function(nm) {
      df <- points[[nm]]
      coords <- as.data.frame(st_coordinates(df)) %>% rename(lon = X, lat = Y)
      state_col <- if ("STATECD" %in% names(df)) as.character(df$STATECD) else NA_character_
      data.frame(lon = coords$lon, lat = coords$lat, dataset = nm, state = state_col, idx = seq_len(nrow(coords)), stringsAsFactors = FALSE)
    })
  )

  # Try ggplot2 first; fall back to base plotting if ggplot2 fails (package mismatches)
  map_file <- file.path(fig_dir, "plots_spatial_overview.png")
  tryCatch({
    p_map <- ggplot(pts_combined, aes(x = lon, y = lat, color = dataset)) +
      geom_point(alpha = 0.6, size = 0.8) +
      coord_equal() +
      theme_minimal() +
      labs(title = "Plot locations (baseline vs augmented)")
    ggsave(map_file, p_map, width = 8, height = 6, dpi = 150)
    cat("Saved map:", map_file, "\n")
  }, error = function(e) {
    cat("  ⚠ ggplot2 plotting failed:", conditionMessage(e), "\n")
    cat("  Falling back to base R plotting. Consider updating ggplot2/rlang/vctrs packages.\n")
    png(map_file, width = 8, height = 6, units = "in", res = 150)
    cols <- as.integer(as.factor(pts_combined$dataset))
    plot(pts_combined$lon, pts_combined$lat, col = cols, pch = 20, cex = 0.6,
         xlab = "lon", ylab = "lat", main = "Plot locations (baseline vs augmented)")
    legend("topright", legend = unique(pts_combined$dataset), col = unique(cols), pch = 20)
    dev.off()
    cat("Saved fallback map:", map_file, "\n")
  })

  # If STATECD present, also create a state-colored map
  if (any(!is.na(pts_combined$state))) {
    state_map_file <- file.path(fig_dir, "plots_spatial_by_state.png")
    tryCatch({
      p_state <- ggplot(pts_combined, aes(x = lon, y = lat, color = state)) +
        geom_point(alpha = 0.7, size = 0.8) +
        coord_equal() +
        theme_minimal() +
        labs(title = "Plot locations colored by STATECD")
      ggsave(state_map_file, p_state, width = 8, height = 6, dpi = 150)
      cat("Saved state-colored map:", state_map_file, "\n")
    }, error = function(e) {
      cat("  ⚠ ggplot2 state map failed:", conditionMessage(e), "\n")
      cat("  Falling back to base R state-colored map.\n")
      png(state_map_file, width = 8, height = 6, units = "in", res = 150)
      cols <- as.integer(as.factor(pts_combined$state))
      plot(pts_combined$lon, pts_combined$lat, col = cols, pch = 20, cex = 0.6,
           xlab = "lon", ylab = "lat", main = "Plot locations by STATECD")
      legend("topright", legend = unique(pts_combined$state), col = unique(cols), pch = 20, cex = 0.8)
      dev.off()
      cat("Saved fallback state-colored map:", state_map_file, "\n")
    })
  }
}

# Plot: biomass distribution
if (have_baseline || have_aug_cov) {
  df_plot <- bind_rows(
    if (have_baseline) baseline %>% select(biomass) %>% mutate(dataset = "baseline") else NULL,
    if (have_aug_cov) augmented %>% select(biomass) %>% mutate(dataset = "augmented") else NULL
  )

  hist_file <- file.path(fig_dir, "biomass_distribution.png")
  tryCatch({
    p_hist <- ggplot(df_plot, aes(x = biomass, fill = dataset)) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
      theme_minimal() +
      labs(title = "Biomass distribution", x = "Biomass (Mg/ha)")
    ggsave(hist_file, p_hist, width = 8, height = 5, dpi = 150)
    cat("Saved histogram:", hist_file, "\n")
  }, error = function(e) {
    cat("  ⚠ ggplot2 histogram failed:", conditionMessage(e), "\n")
    cat("  Falling back to base R histogram.\n")
    png(hist_file, width = 8, height = 5, units = "in", res = 150)
    hist(df_plot$biomass, breaks = 50, col = "grey", main = "Biomass distribution", xlab = "Biomass (Mg/ha)")
    dev.off()
    cat("Saved fallback histogram:", hist_file, "\n")
  })
}

# Scatter: biomass vs NDVI (MODIS)
if ((have_baseline && "ndvi_modis" %in% names(baseline)) || (have_aug_cov && "ndvi_modis" %in% names(augmented))) {
  df_sc <- bind_rows(
    if (have_baseline) baseline %>% select(biomass, ndvi_modis) %>% mutate(dataset = "baseline") else NULL,
    if (have_aug_cov) augmented %>% select(biomass, ndvi_modis) %>% mutate(dataset = "augmented") else NULL
  ) %>% filter(!is.na(biomass), !is.na(ndvi_modis))

  sc_file <- file.path(fig_dir, "biomass_vs_ndvi_modis.png")
  tryCatch({
    p_sc <- ggplot(df_sc, aes(x = ndvi_modis, y = biomass, color = dataset)) +
      geom_point(alpha = 0.5, size = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "Biomass vs MODIS NDVI", x = "NDVI (MODIS)", y = "Biomass (Mg/ha)")
    ggsave(sc_file, p_sc, width = 7, height = 5, dpi = 150)
    cat("Saved scatter:", sc_file, "\n")
  }, error = function(e) {
    cat("  ⚠ ggplot2 scatter failed:", conditionMessage(e), "\n")
    cat("  Falling back to base R scatter plot.\n")
    png(sc_file, width = 7, height = 5, units = "in", res = 150)
    cols <- as.integer(as.factor(df_sc$dataset))
    plot(df_sc$ndvi_modis, df_sc$biomass, col = cols, pch = 20, cex = 0.6,
         xlab = "NDVI (MODIS)", ylab = "Biomass (Mg/ha)", main = "Biomass vs MODIS NDVI")
    abline(lm(biomass ~ ndvi_modis, data = df_sc), col = "black")
    legend("topright", legend = unique(df_sc$dataset), col = unique(cols), pch = 20)
    dev.off()
    cat("Saved fallback scatter:", sc_file, "\n")
  })
}

cat("All outputs are in:", CONFIG$paths$outputs, "(figures in outputs/figures)\n")

if (interactive()) {
  try(View(summary_df), silent = TRUE)
}
