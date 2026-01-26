# =============================================================================
# CREATE SPATIAL CONTEXT MAPS (UPDATED - Reads Hex from GeoJSON)
# =============================================================================
# UPDATED: Reads hexagon centroids from GeoJSON files instead of CSV
# =============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(sf)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  CREATING SPATIAL CONTEXT MAPS (UPDATED)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Create output directory
out_dir <- "data/processed/figures/spatial_context"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# STEP 1: LOAD PLOT DATA
# =============================================================================

cat("Step 1: Loading plot data...\n")

# Try both locations for FIA data
if (file.exists("data/processed/fia_complete.csv")) {
  fia <- read_csv("data/processed/fia_complete.csv", show_col_types = FALSE)
  cat("  ✓ FIA loaded:", nrow(fia), "plots from fia_complete.csv\n")
} else if (file.exists("data/processed/baseline.csv")) {
  fia <- read_csv("data/processed/baseline.csv", show_col_types = FALSE)
  cat("  ✓ FIA loaded:", nrow(fia), "plots from baseline.csv\n")
} else {
  stop("FIA data not found!")
}

# Try both locations for NEFIN data
if (file.exists("data/processed/nefin_complete.csv")) {
  nefin <- read_csv("data/processed/nefin_complete.csv", show_col_types = FALSE)
  cat("  ✓ NEFIN loaded:", nrow(nefin), "plots from nefin_complete.csv\n")
} else if (file.exists("data/processed/nefin.csv")) {
  nefin <- read_csv("data/processed/nefin.csv", show_col_types = FALSE) %>%
    group_by(CN) %>%
    filter(MEASYEAR == max(MEASYEAR)) %>%
    ungroup()
  cat("  ✓ NEFIN loaded:", nrow(nefin), "plots from nefin.csv (deduplicated)\n")
} else {
  cat("  ⚠ NEFIN data not found, using FIA only\n")
  nefin <- data.frame()
}

cat("\n")

# =============================================================================
# STEP 2: COMBINE AND CONVERT TO SPATIAL
# =============================================================================

cat("Step 2: Converting to spatial objects...\n")

# Detect coordinate columns
detect_coords <- function(df) {
  lon_col <- names(df)[grepl("^lon", names(df), ignore.case = TRUE)][1]
  lat_col <- names(df)[grepl("^lat", names(df), ignore.case = TRUE)][1]
  list(lon = lon_col, lat = lat_col)
}

fia_coords <- detect_coords(fia)
fia_sf <- st_as_sf(fia, 
                   coords = c(fia_coords$lon, fia_coords$lat),
                   crs = 4326,
                   remove = FALSE) %>%
  mutate(dataset = "FIA")

if (nrow(nefin) > 0) {
  nefin_coords <- detect_coords(nefin)
  nefin_sf <- st_as_sf(nefin,
                       coords = c(nefin_coords$lon, nefin_coords$lat),
                       crs = 4326,
                       remove = FALSE) %>%
    mutate(dataset = "NEFIN")
  
  combined_sf <- rbind(
    fia_sf %>% select(dataset, geometry),
    nefin_sf %>% select(dataset, geometry)
  )
} else {
  combined_sf <- fia_sf %>% select(dataset, geometry)
}

cat("  ✓ Combined:", nrow(combined_sf), "plots\n")
cat("    FIA:", sum(combined_sf$dataset == "FIA"), "\n")
if (nrow(nefin) > 0) {
  cat("    NEFIN:", sum(combined_sf$dataset == "NEFIN"), "\n")
}
cat("\n")

# =============================================================================
# STEP 3: STUDY AREA MAP
# =============================================================================

cat("Step 3: Creating study area map...\n")

# Get plot coordinates for map
plot_coords <- st_coordinates(combined_sf)
combined_df <- combined_sf %>%
  st_drop_geometry() %>%
  mutate(lon = plot_coords[, 1],
         lat = plot_coords[, 2])

p1 <- ggplot() +
  geom_point(data = combined_df,
             aes(x = lon, y = lat, color = dataset),
             size = 1, alpha = 0.4) +
  scale_color_manual(values = c("FIA" = "#D32F2F", "NEFIN" = "#1976D2"),
                     name = "Dataset") +
  coord_sf(crs = 4326) +
  labs(
    title = "Study Area: Forest Inventory Plot Locations",
    subtitle = "FIA (fuzzed coordinates) and NEFIN (precise coordinates) in Northeastern US",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(
  file.path(out_dir, "fig1_study_area.png"),
  p1,
  width = 10,
  height = 8,
  dpi = 300
)

cat("  ✓ Saved: fig1_study_area.png\n\n")

# =============================================================================
# STEP 4: HEXAGON MAP (from GeoJSON)
# =============================================================================

cat("Step 4: Creating hexagon map from GeoJSON...\n")

# Look for hexagon GeoJSON files
hex_dir <- "data/hex"

if (!dir.exists(hex_dir)) {
  cat("  ⚠ Hexagon directory not found, skipping hexagon map\n\n")
} else {
  
  hex_files <- list.files(hex_dir, pattern = "^hex_grid_.*\\.geojson$", full.names = TRUE)
  
  if (length(hex_files) == 0) {
    cat("  ⚠ No hexagon GeoJSON files found, skipping hexagon map\n\n")
  } else {
    
    # Use a medium-sized hex grid (e.g., 1kha or 2_4kha)
    preferred_scales <- c("1kha", "2_4kha", "5kha", "500ha", "100ha")
    
    hex_file <- NULL
    for (scale in preferred_scales) {
      candidate <- file.path(hex_dir, paste0("hex_grid_", scale, ".geojson"))
      if (file.exists(candidate)) {
        hex_file <- candidate
        hex_scale_name <- scale
        break
      }
    }
    
    if (is.null(hex_file)) {
      # Just use first available
      hex_file <- hex_files[1]
      hex_scale_name <- gsub(".*hex_grid_(.*)\\.geojson", "\\1", hex_file)
    }
    
    cat("  Using hexagon scale:", hex_scale_name, "\n")
    
    # Load hexagons
    hexagons <- st_read(hex_file, quiet = TRUE)
    
    # Calculate centroids
    hex_centroids <- st_centroid(hexagons)
    hex_coords <- st_coordinates(hex_centroids)
    
    hex_df <- hex_centroids %>%
      st_drop_geometry() %>%
      mutate(
        lon = hex_coords[, 1],
        lat = hex_coords[, 2]
      )
    
    cat("  Loaded:", nrow(hexagons), "hexagons\n")
    
    # Create hexagon map
    p2 <- ggplot() +
      # Hexagon boundaries (light)
      geom_sf(data = hexagons, fill = NA, color = "gray70", linewidth = 0.3, alpha = 0.5) +
      # Plot locations
      geom_point(data = combined_df,
                 aes(x = lon, y = lat, color = dataset),
                 size = 0.8, alpha = 0.5) +
      scale_color_manual(values = c("FIA" = "#D32F2F", "NEFIN" = "#1976D2"),
                         name = "Dataset") +
      coord_sf(crs = 4326) +
      labs(
        title = paste("Hexagonal Grid Analysis (", hex_scale_name, ")", sep = ""),
        subtitle = "Plots aggregated to hexagons for spatial analysis",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
      )
    
    ggsave(
      file.path(out_dir, "fig2_hexagon_grid.png"),
      p2,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    cat("  ✓ Saved: fig2_hexagon_grid.png\n\n")
  }
}

# =============================================================================
# STEP 5: FUZZING VISUALIZATION
# =============================================================================

cat("Step 5: Creating fuzzing visualization...\n")

# Select a few example plots
set.seed(42)
if (nrow(nefin) > 0) {
  example_nefin <- nefin_sf %>%
    sample_n(min(5, nrow(nefin)))
  
  # Get coordinates
  nefin_coords_ex <- st_coordinates(example_nefin)
  
  # Create fuzzing circles (1 mile = 1609.34 meters)
  nefin_circles <- example_nefin %>%
    st_buffer(dist = 1609.34)
  
  p3 <- ggplot() +
    # Fuzzing circles
    geom_sf(data = nefin_circles, fill = "#FFCDD2", color = "#D32F2F", 
            alpha = 0.3, linewidth = 0.5) +
    # True locations (NEFIN)
    geom_point(data = data.frame(lon = nefin_coords_ex[, 1],
                                 lat = nefin_coords_ex[, 2]),
               aes(x = lon, y = lat),
               color = "#1976D2", size = 3, shape = 19) +
    # Labels
    annotate("text", x = -Inf, y = Inf, 
             label = "Blue = Precise NEFIN coordinates\nRed circles = FIA fuzzing radius (~1 mile)",
             hjust = 0, vjust = 1, size = 3.5, color = "gray20") +
    coord_sf(crs = 4326) +
    labs(
      title = "FIA Coordinate Fuzzing Concept",
      subtitle = "FIA adds random displacement within ~1 mile radius to protect landowner privacy",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggsave(
    file.path(out_dir, "fig3_fuzzing_concept.png"),
    p3,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  cat("  ✓ Saved: fig3_fuzzing_concept.png\n\n")
} else {
  cat("  ⚠ NEFIN data not available, skipping fuzzing visualization\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Maps created:\n")
cat("  1. fig1_study_area.png - Plot locations\n")

if (dir.exists(hex_dir) && length(list.files(hex_dir, pattern = "\\.geojson$")) > 0) {
  cat("  2. fig2_hexagon_grid.png - Hexagonal analysis grid\n")
}

if (nrow(nefin) > 0) {
  cat("  3. fig3_fuzzing_concept.png - Fuzzing visualization\n")
}

cat("\nLocation:", out_dir, "\n\n")

cat("These figures provide essential spatial context for your manuscript!\n\n")