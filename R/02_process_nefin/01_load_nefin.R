# =============================================================================
# Load and Join NEFIN Data - FINAL
# =============================================================================

source("R/00_config/config.R")
source("R/utils/file_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 1: LOAD AND JOIN NEFIN DATA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

output_dir <- file.path(CONFIG$paths$interim_nefin, "cleaned")
ensure_dir(output_dir)

# Load biomass data
cat("Loading", CONFIG$nefin$main_file, "...\n")
biomass_path <- file.path(CONFIG$paths$raw_nefin, CONFIG$nefin$main_file)
tree_plot_data <- read_csv(biomass_path, show_col_types = FALSE)
cat("  ", nrow(tree_plot_data), "rows,", n_distinct(tree_plot_data$`_nefin_plotID`), "unique plots\n\n")

# Load coordinate data
cat("Loading", CONFIG$nefin$plots_file, "...\n")
coord_path <- file.path(CONFIG$paths$raw_nefin, CONFIG$nefin$plots_file)
nefin_plots <- read_csv(coord_path, show_col_types = FALSE)
cat("  ", nrow(nefin_plots), "rows,", n_distinct(nefin_plots$`_nefin_plotID`), "unique plots\n\n")

# Join on _nefin_plotID
cat("Joining on _nefin_plotID...\n")
nefin_joined <- tree_plot_data %>%
  left_join(
    nefin_plots %>% select(`_nefin_plotID`, lat, long, NAME),
    by = "_nefin_plotID"
  )

# Check for missing coordinates
missing <- sum(is.na(nefin_joined$lat) | is.na(nefin_joined$long))
if (missing > 0) {
  cat("  ⚠ ", missing, "rows missing coordinates - removing\n")
  nefin_joined <- nefin_joined %>% filter(!is.na(lat), !is.na(long))
}

cat("  ", nrow(nefin_joined), "rows after join\n\n")

# Filter to analysis years
cat("Filtering to", CONFIG$year_start, "-", CONFIG$year_end, "...\n")
nefin_filtered <- nefin_joined %>%
  filter(treeSampleYear >= CONFIG$year_start, 
         treeSampleYear <= CONFIG$year_end)

cat("  ", nrow(nefin_filtered), "rows\n")
cat("  ", n_distinct(nefin_filtered$`_nefin_plotID`), "unique plots\n\n")

# Rename columns
nefin_clean <- nefin_filtered %>%
  rename(
    plot_id = `_nefin_plotID`,
    state = `_nefin_state`,
    year = treeSampleYear,
    biomass_kg_ha = AGB_kgPH,
    latitude = lat,
    longitude = long
  )

# Save
output_path <- file.path(output_dir, "nefin_joined.csv")
write_csv(nefin_clean, output_path)

cat("✓ Saved:", output_path, "\n")
cat("  Rows:", nrow(nefin_clean), "\n")
cat("  States:", paste(sort(unique(nefin_clean$state)), collapse=", "), "\n")
cat("  Year range:", min(nefin_clean$year), "-", max(nefin_clean$year), "\n\n")

cat("Next: R/02_process_nefin/02_compute_biomass.R\n\n")