# =============================================================================
# VISUALIZE 10M SCENARIO COMPARISON WITH AERIAL REFERENCE
# =============================================================================
# Layout:  [A] Satellite reference   [B] FIA Only
#          [C] NEFIN Only            [D] Pooled
#
# Also produces:  fig5b_aerial_difference_2panel.png
#                 aerial_size_test.png  (always -- inspect to confirm aerial image dimensions)
#
# NOTE: Sys.setenv must be before library() calls to clear PostgreSQL PROJ conflict
# =============================================================================

Sys.setenv(PROJ_DATA = "", PROJ_LIB = "", PROJ_NETWORK = "OFF")

library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(viridis)
library(magick)

# =============================================================================
# SETTINGS -- adjust these
# =============================================================================

pred_dir <- "data/predictions/phase4/scenario_comparison"
fig_dir  <- file.path(pred_dir, "figures")
aoi_path <- "manuscript_figures/interpretability/AOI.png"

# Aerial image dimensions (pixels) -- check aerial_size_test.png and adjust
AERIAL_W_4PANEL <- 2000   # pixels wide  for panel A in the 4-panel figure
AERIAL_H_4PANEL <- 1700   # pixels tall  for panel A in the 4-panel figure

AERIAL_H_2PANEL <- 1700   # height for aerial in the 2-panel figure
# (width scales automatically to preserve aspect ratio)

AERIAL_BORDER_TOP    <- 80    # white border top    (pixels) -- 0 to remove
AERIAL_BORDER_BOTTOM <- 110    # white border bottom (pixels)
AERIAL_BORDER_LEFT   <- 80    # white border left   (pixels)
AERIAL_BORDER_RIGHT  <- 80    # white border right  (pixels)

AERIAL_BORDER_2PANEL_TOP    <- 150    # white border top    (pixels) -- 0 to remove
AERIAL_BORDER_2PANEL_BOTTOM <- 140    # white border bottom (pixels)
AERIAL_BORDER_2PANEL_LEFT   <- 40   # white border left   (pixels)
AERIAL_BORDER_2PANEL_RIGHT  <- 40    # white border right  (pixels)


# Text size multiplier -- increase to make all labels bigger, decrease to shrink
# 1.0 = default; 1.3 = 30% bigger; 0.8 = 20% smaller
TEXT_SCALE <- 1.3

# Prediction panel render dimensions
PANEL_W   <- 7
PANEL_H   <- 6
PANEL_DPI <- 300

# =============================================================================
# HELPERS
# =============================================================================

add_border_sides <- function(img, top = 0, bottom = 0, left = 0, right = 0) {
  if (top == 0 && bottom == 0 && left == 0 && right == 0) return(img)
  inf <- image_info(img)
  canvas <- image_blank(inf$width + left + right, inf$height + top + bottom,
                        color = "white")
  image_composite(canvas, img, operator = "over",
                  offset = paste0("+", left, "+", top))
}

add_label <- function(img, label, subtitle = NULL) {
  w        <- image_info(img)$width
  bar_h    <- round(w * 0.07 * TEXT_SCALE)
  title_sz <- round(w / 40  * TEXT_SCALE)
  sub_sz   <- round(w / 58  * TEXT_SCALE)
  lbl <- image_blank(w, bar_h, color = "white")
  lbl <- image_annotate(lbl, label, size = title_sz, font = "sans", weight = 700,
                        location = paste0("+", round(w * 0.01), "+", round(bar_h * 0.12)),
                        color = "black")
  if (!is.null(subtitle))
    lbl <- image_annotate(lbl, subtitle, size = sub_sz, font = "sans",
                          location = paste0("+", round(w * 0.01), "+", round(bar_h * 0.56)),
                          color = "grey40")
  image_append(c(lbl, img), stack = TRUE)
}

make_title_bar <- function(width, title, subtitle) {
  h   <- round(width * 0.038 * TEXT_SCALE)
  bar <- image_blank(width, h, color = "white")
  bar <- image_annotate(bar, title,
                        size = round(width / 75 * TEXT_SCALE), font = "sans", weight = 700,
                        location = paste0("+", round(width * 0.005), "+", round(h * 0.08)),
                        color = "black")
  bar <- image_annotate(bar, subtitle,
                        size = round(width / 110 * TEXT_SCALE), font = "sans",
                        location = paste0("+", round(width * 0.005), "+", round(h * 0.55)),
                        color = "grey35")
  bar
}

fit_aerial <- function(raw, w, h) {
  fit <- image_resize(raw, paste0(w, "x", h))
  image_composite(
    image_blank(w, h, color = "white"), fit, operator = "over",
    offset = paste0("+", floor((w - image_info(fit)$width)  / 2),
                    "+", floor((h - image_info(fit)$height) / 2))
  )
}

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("manuscript_figures/interpretability", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD PREDICTIONS
# =============================================================================

cat("Loading predictions...\n")
predictions <- list(
  fia    = rast(file.path(pred_dir, "biomass_10m_fia_only.tif")),
  nefin  = rast(file.path(pred_dir, "biomass_10m_nefin_only.tif")),
  pooled = rast(file.path(pred_dir, "biomass_10m_pooled.tif"))
)
predictions <- lapply(predictions, function(r) ifel(r <= 0, NA, r))
predictions$nefin <- subst(predictions$nefin, NA, 0)
cat("  OK\n\n")

# =============================================================================
# BUILD PREDICTION PANELS
# =============================================================================

biomass_scale <- scale_fill_viridis_c(
  option = "mako", name = "Biomass\n(Mg/ha)",
  na.value = "grey92", limits = c(0, 300)
)

panel_theme_clean <- theme(
  legend.position = "none",
  axis.text = element_blank(), axis.ticks = element_blank(),
  axis.title = element_blank(), panel.grid = element_blank(),
  plot.title = element_blank(), plot.subtitle = element_blank()
)

panel_fia <- ggplot() +
  geom_spatraster(data = predictions$fia) + biomass_scale + theme_void() +
  labs(title = "(B)  FIA Only", subtitle = "Fuzzed coordinates | Full landscape coverage")

panel_nefin <- ggplot() +
  geom_spatraster(data = predictions$nefin) + biomass_scale + theme_void() +
  labs(title = "(C)  NEFIN Only", subtitle = "True GPS coordinates | Voids in low-biomass areas")

panel_pooled <- ggplot() +
  geom_spatraster(data = predictions$pooled) + biomass_scale + theme_void() +
  labs(title = "(D)  Pooled (FIA + NEFIN)", subtitle = "Combined training | Coverage restored")

# =============================================================================
# LOAD AERIAL
# =============================================================================

cat("Loading aerial image...\n")
aerial_raw <- image_read(aoi_path)
cat("  Native size:", image_info(aerial_raw)$width, "x",
    image_info(aerial_raw)$height, "px\n\n")

# =============================================================================
# AERIAL SIZE-TEST PNG  (always produced -- inspect before full figures)
# =============================================================================

cat("Saving size-test PNG...\n")

tmp_ref <- tempfile(fileext = ".png")
ggsave(tmp_ref, panel_fia + panel_theme_clean,
       width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
ref_img <- image_read(tmp_ref)

# Row 1: 4-panel aerial box (red border) | FIA panel for comparison
box_4p <- add_border_sides(fit_aerial(aerial_raw, AERIAL_W_4PANEL, AERIAL_H_4PANEL),
                           AERIAL_BORDER_TOP, AERIAL_BORDER_BOTTOM,
                           AERIAL_BORDER_LEFT, AERIAL_BORDER_RIGHT)
box_4p <- image_border(box_4p, "red", "4x4")
box_4p <- image_annotate(box_4p,
                         paste0("4-panel  W=", AERIAL_W_4PANEL, "  H=", AERIAL_H_4PANEL,
                                "  border T=", AERIAL_BORDER_TOP, " B=", AERIAL_BORDER_BOTTOM,
                                " L=", AERIAL_BORDER_LEFT, " R=", AERIAL_BORDER_RIGHT),
                         size = round(AERIAL_W_4PANEL / 45), color = "red", location = "+10+10")
row1 <- image_append(c(box_4p,
                       image_resize(ref_img, paste0("x", image_info(box_4p)$height))))
row1 <- image_annotate(row1, "4-PANEL: aerial (red) | FIA panel",
                       size = round(image_info(row1)$width / 85),
                       color = "black", location = "+10+8")

# Row 2: 2-panel aerial (blue border) | FIA panel for comparison
box_2p <- add_border_sides(image_resize(aerial_raw, paste0("x", AERIAL_H_2PANEL)),
                           AERIAL_BORDER_TOP, AERIAL_BORDER_BOTTOM,
                           AERIAL_BORDER_LEFT, AERIAL_BORDER_RIGHT)
box_2p <- image_border(box_2p, "blue", "4x4")
box_2p <- image_annotate(box_2p, paste0("2-panel  H=", AERIAL_H_2PANEL),
                         size = round(image_info(box_2p)$width / 45),
                         color = "blue", location = "+10+10")
row2 <- image_append(c(box_2p,
                       image_resize(ref_img, paste0("x", image_info(box_2p)$height))))
row2 <- image_annotate(row2, "2-PANEL: aerial (blue) | FIA panel",
                       size = round(image_info(row2)$width / 85),
                       color = "black", location = "+10+8")

# Pad to equal width and stack
wmax <- max(image_info(row1)$width, image_info(row2)$width)
pad_w <- function(img) {
  w <- image_info(img)$width
  if (w == wmax) return(img)
  image_composite(image_blank(wmax, image_info(img)$height, color = "white"),
                  img, operator = "over", offset = "+0+0")
}
test_path <- file.path("manuscript_figures/interpretability", "aerial_size_test.png")
image_write(image_append(c(pad_w(row1), pad_w(row2)), stack = TRUE), test_path)
cat("  OK", test_path, "\n\n")

# =============================================================================
# RENDER PREDICTION PANELS
# =============================================================================

cat("Rendering prediction panels...\n")
tmp_B   <- tempfile(fileext = ".png")
tmp_C   <- tempfile(fileext = ".png")
tmp_D   <- tempfile(fileext = ".png")
tmp_leg <- tempfile(fileext = ".png")

ggsave(tmp_B, panel_fia    + panel_theme_clean, width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
ggsave(tmp_C, panel_nefin  + panel_theme_clean, width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
ggsave(tmp_D, panel_pooled + panel_theme_clean, width = PANEL_W, height = PANEL_H, dpi = PANEL_DPI, bg = "white")
ggsave(tmp_leg,
       panel_pooled + theme(legend.position = "bottom") +
         guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                      barwidth = 20, barheight = 0.8)),
       width = 14, height = 2, dpi = PANEL_DPI, bg = "white")
cat("  OK\n\n")

# =============================================================================
# ASSEMBLE 4-PANEL FIGURE
# =============================================================================

cat("Assembling 4-panel figure...\n")
tmp_A <- tempfile(fileext = ".png")
image_write(add_border_sides(fit_aerial(aerial_raw, AERIAL_W_4PANEL, AERIAL_H_4PANEL),
                             AERIAL_BORDER_TOP, AERIAL_BORDER_BOTTOM,
                             AERIAL_BORDER_LEFT, AERIAL_BORDER_RIGHT), tmp_A)

img_A <- add_label(image_read(tmp_A), "(A)  Chittenden County, VT",  "Esri World Imagery satellite reference")
img_B <- add_label(image_read(tmp_B), "(B)  FIA Only",               "Fuzzed coordinates | Full landscape coverage")
img_C <- add_label(image_read(tmp_C), "(C)  NEFIN Only",             "True GPS coordinates | Voids in low-biomass areas")
img_D <- add_label(image_read(tmp_D), "(D)  Pooled (FIA + NEFIN)",   "Combined training | Coverage restored")

grid_img <- image_append(c(image_append(c(img_A, img_B)),
                           image_append(c(img_C, img_D))), stack = TRUE)

leg      <- image_read(tmp_leg)
leg_h    <- image_info(leg)$height
leg_crop <- image_crop(leg, paste0(image_info(leg)$width, "x", round(leg_h * 0.55),
                                   "+0+", round(leg_h * 0.45)))

out_4p <- file.path(fig_dir, "fig5_scenario_comparison_4panel.png")
image_write(
  image_append(c(
    make_title_bar(image_info(grid_img)$width,
                   "Figure 5. Chittenden County: satellite reference and biomass predictions",
                   paste0("Random Forest 10 m predictions. (C) NEFIN-only voids = absence of ",
                          "low-biomass training plots. Spatial gradient in (B) vs (C) is ecologically structured.")),
    grid_img, leg_crop), stack = TRUE),
  out_4p, format = "png", quality = 95)
cat("  OK", out_4p, "\n\n")

# =============================================================================
# DIFFERENCE MAP
# =============================================================================

cat("Building difference map...\n")
p_diff <- ggplot() +
  geom_spatraster(data = predictions$fia - predictions$nefin) +
  scale_fill_distiller(palette = "RdBu",
                       name = "Difference\n(Mg/ha)\nFIA - NEFIN",
                       na.value = "grey92", limits = c(-100, 100)) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 9))

tmp_diff <- tempfile(fileext = ".png")
ggsave(tmp_diff, p_diff, width = 8, height = 8, dpi = PANEL_DPI, bg = "white")

out_diff <- file.path(fig_dir, "difference_fia_vs_nefin.png")
ggsave(out_diff,
       p_diff + labs(title    = "FIA vs NEFIN: Difference in Biomass Predictions",
                     subtitle = "Positive (red) = FIA predicts more | Negative (blue) = NEFIN predicts more") +
         theme(plot.title    = element_text(face = "bold", size = 14),
               plot.subtitle = element_text(size = 11)),
       width = 10, height = 8, dpi = 300, bg = "white")
cat("  OK", out_diff, "\n")

# =============================================================================
# ASSEMBLE 2-PANEL FIGURE
# =============================================================================

cat("Assembling 2-panel figure...\n")
aerial_2p <- add_border_sides(image_resize(aerial_raw, paste0("x", AERIAL_H_2PANEL)),
                              AERIAL_BORDER_2PANEL_TOP, AERIAL_BORDER_2PANEL_BOTTOM,
                              AERIAL_BORDER_2PANEL_LEFT, AERIAL_BORDER_2PANEL_RIGHT)
# Match diff height to aerial's actual height AFTER border (not AERIAL_H_2PANEL)
aerial_2p_h <- image_info(aerial_2p)$height
diff_2p     <- image_resize(image_read(tmp_diff), paste0("x", aerial_2p_h))

combined_2p <- image_append(c(
  add_label(aerial_2p, "(A)  Chittenden County, VT",    "Esri World Imagery satellite reference"),
  add_label(diff_2p,   "(B)  FIA vs NEFIN difference (Mg/ha)", "Red = FIA higher (valley) | Blue = NEFIN higher (mountains)")
))

out_2p <- file.path(fig_dir, "fig5b_aerial_difference_2panel.png")
image_write(
  image_append(c(
    make_title_bar(image_info(combined_2p)$width,
                   "Figure 5b. Satellite reference and FIA vs NEFIN biomass difference",
                   paste0("Left: Esri World Imagery. Right: FIA minus NEFIN AGB (Mg/ha). ",
                          "Red = FIA higher (Champlain Valley); Blue = NEFIN higher (Green Mtns).")),
    combined_2p), stack = TRUE),
  out_2p, format = "png", quality = 95)
cat("  OK", out_2p, "\n\n")

# =============================================================================

cat("=================================================================\n")
cat("Output directory:", fig_dir, "\n")
cat("  aerial_size_test.png           <- check this first\n")
cat("  fig5_scenario_comparison_4panel.png\n")
cat("  fig5b_aerial_difference_2panel.png\n")
cat("  difference_fia_vs_nefin.png\n")
cat("=================================================================\n")