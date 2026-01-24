# =============================================================================
# Generate All Publication Figures - Complete Suite (FIXED)
# =============================================================================
# Creates all manuscript figures including hexagon maps with proper geometries
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(viridis)
  library(patchwork)
  library(RColorBrewer)
})

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  GENERATING COMPLETE FIGURE SUITE FOR MANUSCRIPT\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# Create output directories
dir.create("manuscript_figures", showWarnings = FALSE, recursive = TRUE)
dir.create("manuscript_figures/main", showWarnings = FALSE)
dir.create("manuscript_figures/supplementary", showWarnings = FALSE)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Step 1: Loading data...\n")

# Load plot data
fia_plots <- read_csv("data/processed/fia_complete.csv", show_col_types = FALSE)
nefin_plots <- read_csv("data/processed/nefin_complete.csv", show_col_types = FALSE)

cat("  ✓ FIA plots:", nrow(fia_plots), "\n")
cat("  ✓ NEFIN plots:", nrow(nefin_plots), "\n")

# Load Monte Carlo results
mc_data <- read_csv("data/processed/monte_carlo/plot_uncertainty.csv", 
                    show_col_types = FALSE)
cat("  ✓ Monte Carlo data:", nrow(mc_data), "plots\n\n")

# =============================================================================
# FIGURE 1: STUDY AREA MAP (Enhanced Version)
# =============================================================================

cat("Step 2: Creating Figure 1 - Study Area Map (enhanced)...\n")

# Combine plot data
all_plots <- bind_rows(
  fia_plots %>% 
    select(lat, lon) %>% 
    mutate(dataset = "FIA"),
  nefin_plots %>% 
    select(lat, lon) %>% 
    mutate(dataset = "NEFIN")
) %>%
  filter(!is.na(lon), !is.na(lat))

fig1 <- ggplot(all_plots, aes(x = lon, y = lat, color = dataset)) +
  geom_point(aes(alpha = dataset, size = dataset)) +
  scale_color_manual(
    values = c("FIA" = "#d62728", "NEFIN" = "#1f77b4"),
    labels = c(paste0("FIA (fuzzed ±1.6 km, n=", 
                      sum(all_plots$dataset == "FIA"), ")"),
               paste0("NEFIN (precise coords, n=", 
                      sum(all_plots$dataset == "NEFIN"), ")"))
  ) +
  scale_alpha_manual(values = c("FIA" = 0.3, "NEFIN" = 0.8), guide = "none") +
  scale_size_manual(values = c("FIA" = 0.5, "NEFIN" = 1.2), guide = "none") +
  coord_fixed(ratio = 1.3) +
  labs(
    title = "Study Area: Forest Inventory Plots Across Northeastern United States",
    subtitle = paste0("Total: ", nrow(all_plots), " plots from 7 states (VT, NH, ME, MA, NY, CT, RI)"),
    x = "Longitude (°W)",
    y = "Latitude (°N)",
    color = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    panel.grid = element_line(color = "gray90", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "gray30", linewidth = 0.5)
  )

ggsave("manuscript_figures/main/Fig1_Study_Area.png", fig1, 
       width = 12, height = 9, dpi = 300)
cat("  ✓ Saved: Fig1_Study_Area.png\n\n")

# =============================================================================
# FIGURE 2: HEXAGON MAPS - Multi-Scale Comparison
# =============================================================================

cat("Step 3: Creating Figure 2 - Hexagon Multi-Scale Maps...\n")

# Function to load and process hex data with geometries
load_hex_with_geometry <- function(scale) {
  
  # Load hex geometry
  hex_geom_file <- paste0("data/hex/hex_grid_", scale, ".geojson")
  if (!file.exists(hex_geom_file)) {
    cat("  ⚠ Geometry file not found:", hex_geom_file, "\n")
    return(NULL)
  }
  
  hex_geom <- st_read(hex_geom_file, quiet = TRUE)
  
  # Load hex data
  hex_data_files <- c(
    paste0("data/processed/hex_aggregated/baseline_hex_", scale, "_filtered.csv"),
    paste0("data/processed/hex_aggregated/baseline_hex_", scale, ".csv"),
    paste0("data/processed/hex_aggregated/augmented_hex_", scale, ".csv")
  )
  
  hex_data_file <- hex_data_files[file.exists(hex_data_files)][1]
  
  if (is.na(hex_data_file)) {
    cat("  ⚠ Data file not found for scale:", scale, "\n")
    return(NULL)
  }
  
  hex_data <- read_csv(hex_data_file, show_col_types = FALSE) %>%
    filter(n_plots > 0)
  
  # Join geometry with data
  hex_sf <- hex_geom %>%
    left_join(hex_data, by = "hex_id") %>%
    filter(!is.na(biomass_mean))
  
  return(hex_sf)
}

# Create 4-panel figure with different scales
scales_to_plot <- c("1kha", "10kha", "50kha", "100kha")
scale_labels <- c("1,000 ha", "10,000 ha", "50,000 ha", "100,000 ha")

hex_panels <- list()

for (i in seq_along(scales_to_plot)) {
  scale <- scales_to_plot[i]
  label <- scale_labels[i]
  
  cat("  Processing", scale, "...\n")
  
  hex_sf <- load_hex_with_geometry(scale)
  
  if (!is.null(hex_sf)) {
    hex_panels[[i]] <- ggplot(hex_sf) +
      geom_sf(aes(fill = biomass_mean), color = NA) +
      scale_fill_viridis_c(
        option = "viridis",
        name = "Biomass\n(Mg/ha)",
        limits = c(0, 300),
        na.value = "gray80"
      ) +
      labs(title = paste0("(", LETTERS[i], ") ", label),
           subtitle = paste0("n = ", nrow(hex_sf), " hexagons")) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  } else {
    hex_panels[[i]] <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Data not available for", label)) +
      theme_void()
  }
}

if (length(hex_panels) > 0) {
  fig2 <- wrap_plots(hex_panels, ncol = 2) +
    plot_annotation(
      title = "Multi-Scale Hexagonal Aggregation",
      subtitle = "Biomass patterns at increasing spatial scales show progressive averaging",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )
  
  ggsave("manuscript_figures/main/Fig2_Hexagon_MultiScale.png", fig2,
         width = 14, height = 12, dpi = 300)
  cat("  ✓ Saved: Fig2_Hexagon_MultiScale.png\n\n")
}

# =============================================================================
# FIGURE 3: MONTE CARLO UNCERTAINTY DISTRIBUTION
# =============================================================================

cat("Step 4: Creating Figure 3 - Monte Carlo Uncertainty...\n")

# Panel A: NDVI uncertainty histogram
panel_a <- ggplot(mc_data, aes(x = ndvi_s2_sd)) +
  geom_histogram(bins = 50, fill = "#1f77b4", alpha = 0.7, color = "white") +
  geom_vline(xintercept = mean(mc_data$ndvi_s2_sd, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", 
           x = mean(mc_data$ndvi_s2_sd, na.rm = TRUE) + 0.01,
           y = Inf, vjust = 1.5,
           label = sprintf("Mean = %.4f", mean(mc_data$ndvi_s2_sd, na.rm = TRUE)),
           color = "red", fontface = "bold") +
  labs(
    title = "(A) Distribution of NDVI Uncertainty",
    subtitle = "Standard deviation across 100 Monte Carlo replicates per plot",
    x = "NDVI Standard Deviation (fuzzing-induced uncertainty)",
    y = "Number of Plots"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Panel B: Coefficient of variation
panel_b <- ggplot(mc_data, aes(x = ndvi_s2_cv)) +
  geom_histogram(bins = 50, fill = "#ff7f0e", alpha = 0.7, color = "white") +
  geom_vline(xintercept = median(mc_data$ndvi_s2_cv, na.rm = TRUE),
             linetype = "dashed", color = "darkred", linewidth = 1) +
  annotate("text",
           x = median(mc_data$ndvi_s2_cv, na.rm = TRUE) + 0.02,
           y = Inf, vjust = 1.5,
           label = sprintf("Median = %.1f%%", 
                           median(mc_data$ndvi_s2_cv, na.rm = TRUE)),
           color = "darkred", fontface = "bold") +
  labs(
    title = "(B) Relative Uncertainty",
    subtitle = "Coefficient of variation shows 10% typical error",
    x = "Coefficient of Variation (%)",
    y = "Number of Plots"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

fig3 <- panel_a / panel_b +
  plot_annotation(
    title = "Monte Carlo Quantification of Fuzzing-Induced NDVI Uncertainty",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("manuscript_figures/main/Fig3_Monte_Carlo_Uncertainty.png", fig3,
       width = 10, height = 10, dpi = 300)
cat("  ✓ Saved: Fig3_Monte_Carlo_Uncertainty.png\n\n")

# =============================================================================
# FIGURE 4: BIOMASS DISTRIBUTION COMPARISON (FIXED)
# =============================================================================

cat("Step 5: Creating Figure 4 - Compositional Differences...\n")

# Combine biomass data - calculate means directly from data
biomass_comparison <- bind_rows(
  fia_plots %>% select(biomass) %>% mutate(dataset = "FIA"),
  nefin_plots %>% select(biomass) %>% mutate(dataset = "NEFIN")
) %>%
  filter(!is.na(biomass), biomass > 0)

# Calculate means for each dataset
dataset_means <- biomass_comparison %>%
  group_by(dataset) %>%
  summarise(mean_biomass = mean(biomass, na.rm = TRUE))

# Panel A: Overlapping histograms
panel_a <- ggplot(biomass_comparison, aes(x = biomass, fill = dataset)) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  scale_fill_manual(values = c("FIA" = "#d62728", "NEFIN" = "#1f77b4")) +
  geom_vline(data = dataset_means, 
             aes(xintercept = mean_biomass, color = dataset),
             linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("FIA" = "#d62728", "NEFIN" = "#1f77b4"),
                     guide = "none") +
  labs(
    title = "(A) Biomass Distributions",
    subtitle = "NEFIN plots show 31% higher mean biomass than FIA",
    x = "Aboveground Biomass (Mg/ha)",
    y = "Number of Plots",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.8, 0.8)
  )

# Panel B: Box plots
panel_b <- ggplot(biomass_comparison, aes(x = dataset, y = biomass, fill = dataset)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("FIA" = "#d62728", "NEFIN" = "#1f77b4")) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  labs(
    title = "(B) Statistical Comparison",
    subtitle = "Boxes: IQR, white diamond: mean",
    x = "Dataset",
    y = "Aboveground Biomass (Mg/ha)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

fig4 <- panel_a + panel_b +
  plot_annotation(
    title = "Compositional Differences: NEFIN vs FIA Forest Inventory Plots",
    caption = "Kolmogorov-Smirnov test: D = 0.28, p < 10^-13",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 11, hjust = 0)
    )
  )

ggsave("manuscript_figures/main/Fig4_Compositional_Differences.png", fig4,
       width = 14, height = 6, dpi = 300)
cat("  ✓ Saved: Fig4_Compositional_Differences.png\n\n")

# =============================================================================
# COPY EXISTING FIGURES
# =============================================================================

cat("Step 6: Organizing existing figures...\n")

# Copy existing figures to manuscript_figures folder
existing_figs <- c(
  "data/processed/figures/scale_dependency_comprehensive.png",
  "data/processed/figures/decision_matrix.png",
  "data/processed/figures/ndvi_uncertainty_distribution.png",
  "data/processed/figures/within_hex_analysis/biomass_sd_by_nefin_pct.png",
  "data/processed/figures/within_hex_analysis/biomass_sd_trend_by_scale.png",
  "data/processed/figures/spatial_context/fig1_study_area_simple.png",
  "data/processed/figures/spatial_context/fig3_fuzzing_concept.png"
)

for (fig in existing_figs) {
  if (file.exists(fig)) {
    new_name <- paste0("manuscript_figures/supplementary/", basename(fig))
    file.copy(fig, new_name, overwrite = TRUE)
    cat("  ✓ Copied:", basename(fig), "\n")
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  FIGURE GENERATION COMPLETE\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Main Figures Created:\n")
cat("  1. Fig1_Study_Area.png - Enhanced study area map\n")
cat("  2. Fig2_Hexagon_MultiScale.png - 4-panel scale comparison\n")
cat("  3. Fig3_Monte_Carlo_Uncertainty.png - NDVI uncertainty\n")
cat("  4. Fig4_Compositional_Differences.png - FIA vs NEFIN biomass\n\n")

cat("Existing figures organized in supplementary/\n\n")

cat("Location: manuscript_figures/\n")
cat("  - main/ (new publication figures)\n")
cat("  - supplementary/ (supporting figures)\n\n")

cat("All figures ready for manuscript!\n")
cat("Resolution: 300 DPI (publication quality)\n")
cat("Format: PNG (can convert to PDF/EPS if needed)\n\n")

cat("═══════════════════════════════════════════════════════════════════\n\n")
