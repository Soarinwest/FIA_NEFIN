# =============================================================================
# VISUALIZE 10M SCENARIO COMPARISON - WITH AERIAL REFERENCE PANEL
# =============================================================================
# Adds Esri World Imagery satellite panel alongside the three prediction maps.
# Layout: 2x2 grid
#   [A] Satellite reference    [B] FIA Only
#   [C] NEFIN Only             [D] Pooled
#
# CRITICAL: Sys.setenv MUST be before any library() calls or PROJ fix won't work.
# =============================================================================

Sys.setenv(PROJ_DATA    = "")
Sys.setenv(PROJ_LIB     = "")
Sys.setenv(PROJ_NETWORK = "OFF")

library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(viridis)
library(png)

# =============================================================================
# SETTINGS
# =============================================================================

pred_dir <- "data/predictions/phase4/scenario_comparison"
fig_dir  <- file.path(pred_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=================================================================\n")
cat("  VISUALIZING 10M SCENARIO COMPARISON + AERIAL REFERENCE\n")
cat("=================================================================\n\n")

# =============================================================================
# LOAD PREDICTIONS AND FIX CRS  (unchanged from working script)
# =============================================================================

cat("Loading predictions and fixing CRS...\n")

predictions <- list()

if (file.exists(file.path(pred_dir, "biomass_10m_fia_only.tif"))) {
  predictions$fia <- rast(file.path(pred_dir, "biomass_10m_fia_only.tif"))
  cat("  OK FIA only\n")
}
if (file.exists(file.path(pred_dir, "biomass_10m_nefin_only.tif"))) {
  predictions$nefin <- rast(file.path(pred_dir, "biomass_10m_nefin_only.tif"))
  cat("  OK NEFIN only\n")
}
if (file.exists(file.path(pred_dir, "biomass_10m_pooled.tif"))) {
  predictions$pooled <- rast(file.path(pred_dir, "biomass_10m_pooled.tif"))
  cat("  OK Pooled\n")
}

if (length(predictions) == 0) stop("No predictions found!")

# Mask non-forest pixels
for (nm in names(predictions)) {
  predictions[[nm]] <- ifel(predictions[[nm]] <= 0, NA, predictions[[nm]])
}
cat("  OK Non-forest masked\n\n")



# =============================================================================
# COMMON THEME  (unchanged)
# =============================================================================

theme_map <- theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom",
    axis.text     = element_text(size = 7),
    panel.grid    = element_line(color = "gray90", linewidth = 0.3)
  )

# Consistent biomass colour scale across all three prediction panels
biomass_scale <- scale_fill_viridis_c(
  option   = "mako",
  name     = "Biomass\n(Mg/ha)",
  na.value = "transparent",
  limits   = c(0, 300)
)

# =============================================================================
# PANEL A: AERIAL / SATELLITE REFERENCE
# =============================================================================

# PANELS B, C, D: BIOMASS PREDICTIONS  (same as original, labels updated)
# =============================================================================

panel_fia <- ggplot() +
  geom_spatraster(data = predictions$fia) +
  biomass_scale +
  labs(title    = "(B)  FIA Only",
       subtitle = "Fuzzed coordinates | Full landscape coverage",
       x = NULL, y = NULL) +
  theme_void()

# Extract the exact geographic limits from the FIA panel so the aerial panel
# uses identical coordinate ranges -- ensures equal sizing with no PROJ calls.
pb       <- ggplot_build(panel_fia)
pp       <- pb$layout$panel_params[[1]]
map_xmin <- pp$x_range[1];  map_xmax <- pp$x_range[2]
map_ymin <- pp$y_range[1];  map_ymax <- pp$y_range[2]
cat(sprintf("  Map extent: lon %.3f to %.3f | lat %.3f to %.3f\n",
            map_xmin, map_xmax, map_ymin, map_ymax))

panel_nefin <- ggplot() +
  geom_spatraster(data = predictions$nefin) +
  biomass_scale +
  labs(title    = "(C)  NEFIN Only",
       subtitle = "True GPS coordinates | Voids in low-biomass areas",
       x = NULL, y = NULL) +
  theme_void()

panel_pooled <- ggplot() +
  geom_spatraster(data = predictions$pooled) +
  biomass_scale +
  labs(title    = "(D)  Pooled (FIA + NEFIN)",
       subtitle = "Combined training | Coverage restored",
       x = NULL, y = NULL) +
  theme_void()

# =============================================================================
# COMBINE: 2x2 LAYOUT WITH SHARED LEGEND
# =============================================================================

cat("Building aerial reference panel...\n")

# Load local aerial PNG -- place file at manuscript_figures/interpretability/AOI.png
aoi_path <- "manuscript_figures/interpretability/AOI.png"

if (file.exists(aoi_path)) {
  tile_img <- png::readPNG(aoi_path)
  cat("  OK Local AOI.png loaded\n")
  panel_aerial <- ggplot() +
    annotation_raster(tile_img,
                      xmin = map_xmin, xmax = map_xmax,
                      ymin = map_ymin, ymax = map_ymax,
                      interpolate = TRUE) +
    xlim(map_xmin, map_xmax) + ylim(map_ymin, map_ymax) +
    labs(x = NULL, y = NULL) +
    theme_void()
  cat("  OK Aerial panel built\n\n")
} else {
  cat("  AOI.png not found at", aoi_path, "-- using blank placeholder\n")
  panel_aerial <- ggplot() +
    annotate("text", x = (map_xmin + map_xmax) / 2, y = (map_ymin + map_ymax) / 2,
             label = paste0("Place aerial image at:\n", aoi_path),
             size = 4, colour = "grey50", hjust = 0.5, vjust = 0.5) +
    xlim(map_xmin, map_xmax) + ylim(map_ymin, map_ymax) +
    labs(x = NULL, y = NULL) +
    theme_void()
  cat("  OK Fallback aerial panel built\n\n")
}

# =============================================================================
cat("Assembling 4-panel figure via magick...\n")
library(magick)

# Pre-render each panel to a temp PNG at identical dimensions.
# This completely avoids patchwork coordinate alignment issues.
PANEL_W <- 7;  PANEL_H <- 6;  PANEL_DPI <- 300

panel_theme_clean <- theme(
  legend.position  = "none",
  axis.text        = element_blank(),
  axis.ticks       = element_blank(),
  axis.title       = element_blank(),
  panel.grid       = element_blank(),
  plot.title       = element_blank(),   # magick add_label() handles panel labels
  plot.subtitle    = element_blank()
)

tmp_A <- tempfile(fileext = ".png")
tmp_B <- tempfile(fileext = ".png")
tmp_C <- tempfile(fileext = ".png")
tmp_D <- tempfile(fileext = ".png")
tmp_leg <- tempfile(fileext = ".png")

# Panel A: aerial (already a clean ggplot with no axes)
ggsave(tmp_A, panel_aerial + panel_theme_clean,
       width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")

# Panel B: FIA (suppress legend, axes)
ggsave(tmp_B, panel_fia + panel_theme_clean,
       width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")

# Panel C: NEFIN
ggsave(tmp_C, panel_nefin + panel_theme_clean,
       width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")

# Panel D: Pooled
ggsave(tmp_D, panel_pooled + panel_theme_clean,
       width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")

# Legend strip (extract from panel_pooled with legend)
p_leg_only <- panel_pooled +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(
    title.position = "top", title.hjust = 0.5,
    barwidth = 20, barheight = 0.8
  ))
ggsave(tmp_leg, p_leg_only, width = 14, height = 2, dpi = PANEL_DPI, bg = "white")

# Stitch with magick
img_A <- image_read(tmp_A);  img_B <- image_read(tmp_B)
img_C <- image_read(tmp_C);  img_D <- image_read(tmp_D)
img_leg <- image_read(tmp_leg)

# Add panel labels as image annotations
add_label <- function(img, label, subtitle = NULL) {
  lbl <- image_blank(image_info(img)$width, 60, color = "white")
  lbl <- image_annotate(lbl, label,   size = 36, font = "sans",
                        weight = 700, location = "+10+5",  color = "black")
  if (!is.null(subtitle))
    lbl <- image_annotate(lbl, subtitle, size = 26, font = "sans",
                          location = "+10+38", color = "grey40")
  image_append(c(lbl, img), stack = TRUE)
}

img_A <- add_label(img_A, "(A)  Chittenden County, VT",
                   "Esri World Imagery satellite reference")
img_B <- add_label(img_B, "(B)  FIA Only",
                   "Fuzzed coordinates | Full landscape coverage")
img_C <- add_label(img_C, "(C)  NEFIN Only",
                   "True GPS coordinates | Voids in low-biomass areas")
img_D <- add_label(img_D, "(D)  Pooled (FIA + NEFIN)",
                   "Combined training | Coverage restored")

# 2x2 grid
top_row    <- image_append(c(img_A, img_B))
bottom_row <- image_append(c(img_C, img_D))
grid_img   <- image_append(c(top_row, bottom_row), stack = TRUE)

# Add title bar
title_bar <- image_blank(image_info(grid_img)$width, 110, color = "white")
title_bar <- image_annotate(title_bar,
                            "Figure 5.  Chittenden County: satellite reference and biomass predictions",
                            size = 44, font = "sans", weight = 700, location = "+20+10", color = "black")
title_bar <- image_annotate(title_bar,
                            paste0("Random Forest 10 m predictions. (C) NEFIN-only voids = absence of low-biomass training plots. ",
                                   "Spatial gradient in (B) vs (C) is ecologically structured."),
                            size = 30, font = "sans", location = "+20+65", color = "grey35")

# Crop legend strip to just the legend area (bottom ~40% of that figure)
leg_h  <- image_info(img_leg)$height
leg_crop <- image_crop(img_leg,
                       paste0(image_info(img_leg)$width, "x", round(leg_h * 0.55),
                              "+0+", round(leg_h * 0.45)))

final_img <- image_append(c(title_bar, grid_img, leg_crop), stack = TRUE)

image_write(final_img,
            path = file.path(fig_dir, "fig5_scenario_comparison_4panel.png"),
            format = "png", quality = 95)
cat("  OK fig5_scenario_comparison_4panel.png\n\n")

# =============================================================================
# ALSO KEEP THE ORIGINAL 3-PANEL (unchanged from working script)
# =============================================================================

cat("Also saving original 3-panel...\n")

p_3panel <- (panel_fia + panel_nefin + panel_pooled) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

p_3panel <- p_3panel +
  plot_annotation(
    title    = "Fine Scale (10m) Biomass: Dataset Comparison",
    subtitle = "Chittenden County, Vermont",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  filename = file.path(fig_dir, "scenario_comparison_3panel.png"),
  plot     = p_3panel,
  width    = 18,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)
cat("  OK scenario_comparison_3panel.png\n\n")

# =============================================================================
# REMAINING PLOTS (unchanged from working script)
# =============================================================================

cat("Saving distribution comparison...\n")

dfs <- list()
for (nm in names(predictions)) {
  df <- as.data.frame(predictions[[nm]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "biomass"
  df$scenario  <- switch(nm, fia = "FIA Only", nefin = "NEFIN Only", pooled = "Pooled")
  dfs[[nm]] <- df
}
combined_df <- do.call(rbind, dfs)

p_dist <- ggplot(combined_df, aes(x = biomass, fill = scenario)) +
  geom_density(alpha = 0.6, linewidth = 1) +
  scale_fill_manual(
    values = c("FIA Only" = "#D32F2F", "NEFIN Only" = "#1976D2", "Pooled" = "#388E3C"),
    name   = "Dataset"
  ) +
  labs(title    = "Biomass Distribution by Training Dataset",
       subtitle = "All predictions at 10m resolution",
       x = "Predicted Biomass (Mg/ha)", y = "Density") +
  theme_minimal() +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        legend.position = "top")

ggsave(file.path(fig_dir, "scenario_distribution_comparison.png"),
       p_dist, width = 10, height = 6, dpi = 300, bg = "white")
cat("  OK scenario_distribution_comparison.png\n")

if ("fia" %in% names(predictions) && "nefin" %in% names(predictions)) {
  cat("Saving difference map...\n")
  diff_rast <- predictions$fia - predictions$nefin
  
  p_diff <- ggplot() +
    geom_spatraster(data = diff_rast) +
    scale_fill_distiller(
      palette  = "RdBu",
      name     = "Difference\n(Mg/ha)\nFIA - NEFIN",
      na.value = "transparent",
      limits   = c(-100, 100)
    ) +
    labs(title    = "FIA vs NEFIN: Difference in Biomass Predictions",
         subtitle = "Positive (red) = FIA predicts more | Negative (blue) = NEFIN predicts more",
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11),
          legend.position = "right",
          panel.grid = element_line(color = "gray90", linewidth = 0.3))
  
  ggsave(file.path(fig_dir, "difference_fia_vs_nefin.png"),
         p_diff, width = 10, height = 8, dpi = 300, bg = "white")
  cat("  OK difference_fia_vs_nefin.png\n")
  
  # 2-panel: aerial reference + difference map side by side
  if (!is.null(tiles)) {
    cat("Saving aerial + difference 2-panel...\n")
    # Save aerial at same dimensions as prediction panels
    tmp_aerial_sq <- tempfile(fileext = ".png")
    ggsave(tmp_aerial_sq, panel_aerial,
           width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
    
    # Save diff map standalone (geom_spatraster works fine in isolation)
    tmp_diff <- tempfile(fileext = ".png")
    ggsave(tmp_diff,
           p_diff + theme_void() +
             theme(legend.position = "right",
                   legend.title = element_text(size = 9),
                   legend.text  = element_text(size = 8)),
           width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
    
    img_aerial_2 <- add_label(image_read(tmp_aerial_sq),
                              "(A)  Chittenden County, VT",
                              "Esri World Imagery satellite reference")
    img_diff_2   <- add_label(image_read(tmp_diff),
                              "FIA vs NEFIN: Difference in Biomass Predictions",
                              "Red = FIA predicts more | Blue = NEFIN predicts more")
    
    combined_2p <- image_append(c(img_aerial_2, img_diff_2))
    title2 <- image_blank(image_info(combined_2p)$width, 110, color = "white")
    title2 <- image_annotate(title2,
                             "Figure 5b.  Satellite reference and FIA vs NEFIN biomass difference",
                             size = 44, font = "sans", weight = 700, location = "+20+10", color = "black")
    title2 <- image_annotate(title2,
                             paste0("Left: Esri World Imagery. Right: FIA minus NEFIN AGB (Mg/ha). ",
                                    "Red = FIA higher (Champlain Valley); Blue = NEFIN higher (Green Mtns). ",
                                    "Gradient is ecologically structured, not spatially random."),
                             size = 28, font = "sans", location = "+20+62", color = "grey35")
    
    final_2p <- image_append(c(title2, combined_2p), stack = TRUE)
    image_write(final_2p,
                path = file.path(fig_dir, "fig5b_aerial_difference_2panel.png"),
                format = "png", quality = 95)
    cat("  OK fig5b_aerial_difference_2panel.png\n")
  }
}

cat("\n=================================================================\n")
cat("  DONE\n")
cat("=================================================================\n")
cat("Figures saved to:", fig_dir, "\n")
cat("  fig5_scenario_comparison_4panel.png  <- NEW main figure\n")
cat("  scenario_comparison_3panel.png\n")
cat("  scenario_distribution_comparison.png\n")
cat("  difference_fia_vs_nefin.png\n\n")