# =============================================================================
# Create Essential Spatial Maps (SIMPLIFIED)
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(readr)
})

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  CREATING SPATIAL CONTEXT MAPS (SIMPLIFIED)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Create output directory
dir.create("data/processed/figures/spatial_context", 
           showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Step 1: Loading plot data...\n")

# Try multiple file locations for FIA
fia_files <- c(
  "data/processed/fia_complete.csv",
  "data/interim/fia/biomass/fia_plot_biomass.csv",
  "data/processed/baseline_with_covariates.csv"
)
fia_file <- fia_files[file.exists(fia_files)][1]
if (is.na(fia_file)) {
  cat("  ⚠ FIA file not found, tried:\n")
  for (f in fia_files) cat("    ", f, "\n")
  stop("FIA plot file not found!")
}

fia_plots <- read_csv(fia_file, show_col_types = FALSE)
cat("  ✓ FIA loaded:", nrow(fia_plots), "plots from", basename(fia_file), "\n")

# Try multiple file locations for NEFIN
nefin_files <- c(
  "data/processed/nefin_complete.csv",
  "data/interim/nefin/biomass/nefin_plot_biomass.csv",
  "data/raw/nefin/TREE_PLOT_DATA.csv",
  "data/raw/nefin/NEFIN_plots.csv",
  "data/processed/augmented_with_covariates.csv"
)
nefin_file <- nefin_files[file.exists(nefin_files)][1]
if (is.na(nefin_file)) {
  cat("  ⚠ NEFIN file not found, tried:\n")
  for (f in nefin_files) cat("    ", f, "\n")
  stop("NEFIN plot file not found!")
}

nefin_plots <- read_csv(nefin_file, show_col_types = FALSE)
cat("  ✓ NEFIN loaded:", nrow(nefin_plots), "plots from", basename(nefin_file), "\n\n")

# =============================================================================
# IDENTIFY COORDINATE COLUMNS
# =============================================================================

cat("Step 2: Identifying coordinate columns...\n")

# Find lon/lat columns in FIA
fia_lon_col <- intersect(names(fia_plots), c("lon", "LON", "longitude", "x", "X"))[1]
fia_lat_col <- intersect(names(fia_plots), c("lat", "LAT", "latitude", "y", "Y"))[1]

if (is.na(fia_lon_col) || is.na(fia_lat_col)) {
  cat("  Available FIA columns:\n")
  print(names(fia_plots))
  stop("Could not find lon/lat columns in FIA data")
}

cat("  FIA coordinates: lon =", fia_lon_col, ", lat =", fia_lat_col, "\n")

# Find lon/lat columns in NEFIN
nefin_lon_col <- intersect(names(nefin_plots), c("lon", "LON", "longitude", "x", "X"))[1]
nefin_lat_col <- intersect(names(nefin_plots), c("lat", "LAT", "latitude", "y", "Y"))[1]

if (is.na(nefin_lon_col) || is.na(nefin_lat_col)) {
  cat("  Available NEFIN columns:\n")
  print(names(nefin_plots))
  stop("Could not find lon/lat columns in NEFIN data")
}

cat("  NEFIN coordinates: lon =", nefin_lon_col, ", lat =", nefin_lat_col, "\n\n")

# =============================================================================
# PREPARE SPATIAL DATA
# =============================================================================

cat("Step 3: Converting to spatial objects...\n")

# Prepare FIA spatial data
fia_spatial <- fia_plots %>%
  select(lon = !!sym(fia_lon_col), lat = !!sym(fia_lat_col)) %>%
  filter(!is.na(lon), !is.na(lat), is.finite(lon), is.finite(lat)) %>%
  mutate(dataset = "FIA")

# Prepare NEFIN spatial data  
nefin_spatial <- nefin_plots %>%
  select(lon = !!sym(nefin_lon_col), lat = !!sym(nefin_lat_col)) %>%
  filter(!is.na(lon), !is.na(lat), is.finite(lon), is.finite(lat)) %>%
  mutate(dataset = "NEFIN")

# Combine
all_plots <- bind_rows(fia_spatial, nefin_spatial)

cat("  ✓ Combined:", nrow(all_plots), "plots\n")
cat("    FIA:", sum(all_plots$dataset == "FIA"), "\n")
cat("    NEFIN:", sum(all_plots$dataset == "NEFIN"), "\n\n")

# =============================================================================
# FIGURE 1: STUDY AREA MAP (SIMPLE VERSION)
# =============================================================================

cat("Step 4: Creating study area map...\n")

# Simple version without state boundaries (in case download fails)
fig1 <- ggplot(all_plots, aes(x = lon, y = lat, color = dataset)) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_manual(
    values = c("FIA" = "#d62728", "NEFIN" = "#1f77b4"),
    labels = c("FIA (fuzzed ±1.6 km)", "NEFIN (true location)")
  ) +
  coord_fixed(ratio = 1.3) +
  labs(
    title = "Study Area: FIA vs NEFIN Forest Inventory Plots",
    subtitle = paste0("Northeastern United States: FIA n=", 
                      sum(all_plots$dataset == "FIA"), 
                      ", NEFIN n=", sum(all_plots$dataset == "NEFIN")),
    x = "Longitude",
    y = "Latitude",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid = element_line(color = "gray90", size = 0.2)
  )

output_file <- "data/processed/figures/spatial_context/fig1_study_area_simple.png"
ggsave(output_file, fig1, width = 10, height = 8, dpi = 300)

cat("  ✓ Saved:", output_file, "\n\n")

# =============================================================================
# FIGURE 2: HEXAGON DISTRIBUTION (if hex data available)
# =============================================================================

cat("Step 5: Creating hexagon map (if available)...\n")

# Try to load 1kha hex data
hex_files <- c(
  "data/processed/hex_aggregated/baseline_hex_1kha.csv",
  "data/processed/hex_aggregated/augmented_hex_1kha.csv"
)

hex_file <- hex_files[file.exists(hex_files)][1]

if (!is.na(hex_file)) {
  hex_data <- read_csv(hex_file, show_col_types = FALSE) %>%
    filter(n_plots > 0)
  
  # Check for centroid columns
  has_centroids <- all(c("centroid_lon", "centroid_lat") %in% names(hex_data))
  
  if (has_centroids) {
    fig2 <- ggplot(hex_data, aes(x = centroid_lon, y = centroid_lat)) +
      geom_point(aes(fill = biomass_mean, size = n_plots), 
                 shape = 21, alpha = 0.7) +
      scale_fill_viridis_c(option = "viridis", name = "Biomass\n(Mg/ha)") +
      scale_size_continuous(range = c(0.5, 3), name = "Plots") +
      coord_fixed(ratio = 1.3) +
      labs(
        title = "Hexagonal Aggregation: 1,000 ha Scale",
        subtitle = paste0(nrow(hex_data), " hexagons with plots"),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    output_file2 <- "data/processed/figures/spatial_context/fig2_hex_1kha.png"
    ggsave(output_file2, fig2, width = 10, height = 8, dpi = 300)
    
    cat("  ✓ Saved:", output_file2, "\n\n")
  } else {
    cat("  ⚠ Centroid columns not found, skipping hexagon map\n\n")
  }
} else {
  cat("  ⚠ Hex data not found, skipping hexagon map\n\n")
}

# =============================================================================
# FIGURE 3: FUZZING CONCEPTUAL DIAGRAM
# =============================================================================

cat("Step 6: Creating fuzzing visualization...\n")

# Conceptual diagram (doesn't need real data)
set.seed(42)
example_lon <- -72.5
example_lat <- 44.0
fuzzing_radius_km <- 1.6
fuzzing_radius_deg <- fuzzing_radius_km / 111

# Create circle
theta <- seq(0, 2*pi, length.out = 100)
circle_x <- example_lon + fuzzing_radius_deg * cos(theta)
circle_y <- example_lat + fuzzing_radius_deg * sin(theta) * 1.3

circle_df <- data.frame(lon = circle_x, lat = circle_y)

fig3 <- ggplot() +
  geom_polygon(data = circle_df, aes(x = lon, y = lat), 
               fill = "red", alpha = 0.2, color = "red", size = 1) +
  geom_point(aes(x = example_lon, y = example_lat), 
             color = "blue", size = 4, shape = 3) +
  geom_point(aes(x = example_lon + 0.01, y = example_lat + 0.008), 
             color = "red", size = 3, shape = 16) +
  annotate("text", x = example_lon, y = example_lat - 0.02, 
           label = "True Location\n(precise)", color = "blue", size = 5) +
  annotate("text", x = example_lon + 0.01, y = example_lat + 0.008 + 0.015, 
           label = "Fuzzed Location\n(±1.6 km uncertainty)", 
           color = "red", size = 4) +
  coord_fixed(ratio = 1.3, xlim = c(example_lon - 0.03, example_lon + 0.03),
              ylim = c(example_lat - 0.025, example_lat + 0.025)) +
  labs(
    title = "FIA Coordinate Fuzzing Explained",
    subtitle = "Privacy protection introduces spatial uncertainty",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

output_file3 <- "data/processed/figures/spatial_context/fig3_fuzzing_concept.png"
ggsave(output_file3, fig3, width = 8, height = 8, dpi = 300)

cat("  ✓ Saved:", output_file3, "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Maps created:\n")
cat("  1. fig1_study_area_simple.png - Plot locations\n")
if (!is.na(hex_file) && has_centroids) {
  cat("  2. fig2_hex_1kha.png - Hexagonal aggregation\n")
}
cat("  3. fig3_fuzzing_concept.png - Fuzzing visualization\n\n")

cat("Location: data/processed/figures/spatial_context/\n\n")

cat("These figures provide essential spatial context!\n")
cat("More detailed maps with state boundaries can be added later.\n\n")

cat("Spatial mapping complete!\n\n")