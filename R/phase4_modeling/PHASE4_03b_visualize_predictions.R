# =============================================================================
# PHASE 4: VISUALIZE BIOMASS PREDICTIONS
# =============================================================================
# Creates quick preview maps of predicted biomass rasters
# 
# Usage:
#   Rscript R/phase4_modeling/PHASE4_03b_visualize_predictions.R
# =============================================================================

source("R/00_config/PHASE4_config.R")

library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4: VISUALIZE PREDICTIONS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Create output directory for figures
fig_dir <- file.path(PHASE4_CONFIG$output$dir_figures, "predictions")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD PREDICTION RASTERS
# =============================================================================

cat("Loading prediction rasters...\n")

pred_dir <- PHASE4_CONFIG$prediction$output$dir
pred_files <- list.files(pred_dir, pattern = "biomass_.*\\.tif$", full.names = TRUE)

if (length(pred_files) == 0) {
  stop("No prediction rasters found. Run PHASE4_03_predict_biomass.R first.")
}

cat("  Found", length(pred_files), "rasters\n\n")

# =============================================================================
# CREATE INDIVIDUAL MAPS
# =============================================================================

cat("Creating individual maps...\n")

for (file in pred_files) {
  
  model_name <- gsub("biomass_|\\.tif", "", basename(file))
  cat("  ", model_name, "...\n")
  
  r <- rast(file)
  
  # Create plot
  p <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_viridis_c(
      name = "Biomass\n(Mg/ha)",
      option = "viridis",
      na.value = "transparent",
      limits = c(0, 400)
    ) +
    labs(
      title = paste("Predicted Aboveground Biomass:", model_name),
      subtitle = paste(PHASE4_CONFIG$prediction$extent$type)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  
  # Save
  ggsave(
    file.path(fig_dir, paste0("map_", model_name, ".png")),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300
  )
}

cat("  ✓ Individual maps saved\n\n")

# =============================================================================
# CREATE COMPARISON MAPS (FIA vs NEFIN)
# =============================================================================

cat("Creating comparison maps...\n")

for (scale in c("fine", "coarse")) {
  
  # Find matching files
  fia_file <- grep(paste0(scale, "_fia_only"), pred_files, value = TRUE)
  nefin_file <- grep(paste0(scale, "_nefin_only"), pred_files, value = TRUE)
  diff_file <- list.files(pred_dir, 
                          pattern = paste0("difference_", scale, "_nefin_minus_fia.tif"),
                          full.names = TRUE)
  
  if (length(fia_file) == 1 && length(nefin_file) == 1) {
    
    cat("  Scale:", scale, "\n")
    
    # Load rasters
    fia <- rast(fia_file)
    nefin <- rast(nefin_file)
    
    # Create plots
    p1 <- ggplot() +
      geom_spatraster(data = fia) +
      scale_fill_viridis_c(
        name = "Biomass\n(Mg/ha)",
        option = "viridis",
        limits = c(0, 400)
      ) +
      labs(title = "FIA Only (Fuzzed Coordinates)") +
      theme_minimal() +
      theme(legend.position = "right")
    
    p2 <- ggplot() +
      geom_spatraster(data = nefin) +
      scale_fill_viridis_c(
        name = "Biomass\n(Mg/ha)",
        option = "viridis",
        limits = c(0, 400)
      ) +
      labs(title = "NEFIN Only (Precise Coordinates)") +
      theme_minimal() +
      theme(legend.position = "right")
    
    # Combine
    p_combined <- p1 + p2 +
      plot_annotation(
        title = paste("Biomass Predictions Comparison:", 
                      ifelse(scale == "fine", "Fine Scale (10m)", "Coarse Scale (250m)")),
        theme = theme(plot.title = element_text(face = "bold", size = 16))
      )
    
    ggsave(
      file.path(fig_dir, paste0("comparison_", scale, "_fia_vs_nefin.png")),
      plot = p_combined,
      width = 16,
      height = 8,
      dpi = 300
    )
    
    # If difference map exists, plot it
    if (length(diff_file) == 1) {
      
      diff <- rast(diff_file)
      
      # Get symmetric limits
      max_abs <- max(abs(minmax(diff)), na.rm = TRUE)
      
      p_diff <- ggplot() +
        geom_spatraster(data = diff) +
        scale_fill_gradient2(
          name = "Difference\n(Mg/ha)",
          low = "blue",
          mid = "white",
          high = "red",
          midpoint = 0,
          limits = c(-max_abs, max_abs)
        ) +
        labs(
          title = paste("Difference Map: NEFIN - FIA", 
                        ifelse(scale == "fine", "(10m)", "(250m)")),
          subtitle = "Positive = NEFIN predicts higher, Negative = FIA predicts higher"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "right"
        )
      
      ggsave(
        file.path(fig_dir, paste0("difference_", scale, "_map.png")),
        plot = p_diff,
        width = 10,
        height = 8,
        dpi = 300
      )
    }
  }
}

cat("  ✓ Comparison maps saved\n\n")

# =============================================================================
# CREATE SUMMARY STATISTICS
# =============================================================================

cat("Calculating summary statistics...\n")

stats_list <- list()

for (file in pred_files) {
  
  model_name <- gsub("biomass_|\\.tif", "", basename(file))
  r <- rast(file)
  
  stats_list[[model_name]] <- data.frame(
    model = model_name,
    mean = global(r, "mean", na.rm = TRUE)[1,1],
    sd = global(r, "sd", na.rm = TRUE)[1,1],
    min = global(r, "min", na.rm = TRUE)[1,1],
    max = global(r, "max", na.rm = TRUE)[1,1],
    median = global(r, "median", na.rm = TRUE)[1,1]
  )
}

stats_df <- do.call(rbind, stats_list)
rownames(stats_df) <- NULL

# Save statistics
write.csv(stats_df, 
          file.path(pred_dir, "prediction_statistics.csv"),
          row.names = FALSE)

cat("\n  Prediction Statistics:\n")
print(stats_df, digits = 2)
cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VISUALIZATION COMPLETE!\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Figures saved to:", fig_dir, "\n")
cat("  - Individual maps:", length(pred_files), "files\n")
cat("  - Comparison maps: FIA vs NEFIN\n")
cat("  - Difference maps: NEFIN - FIA\n\n")

cat("Statistics saved to:", file.path(pred_dir, "prediction_statistics.csv"), "\n\n")
