# =============================================================================
# ADD WATER AND URBAN TRAINING POINTS
# =============================================================================
# Samples water/urban locations within the study AOI and extracts covariates
# using the same config-driven pipeline as PHASE4_extract_covariates.R.
#
# Water points are clipped to the AOI shapefile (same boundary used to clip
# all rasters in PHASE4_00_preprocess_rasters.R) to ensure every point falls
# within raster coverage. Urban points are sampled from NDVI raster pixels,
# which are already within coverage by definition.
#
# Points with incomplete covariate extraction are dropped (not imputed).
# =============================================================================

# Fix PostgreSQL PROJ interference (must be before loading terra)
Sys.setenv(PROJ_DATA = "")
Sys.setenv(PROJ_LIB = "")

source("R/00_config/config.R")
source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

# Load preprocessed config if available (overrides file paths)
preprocessed_config <- "R/00_config/PHASE4_config_covariates_PREPROCESSED.R"
if (file.exists(preprocessed_config)) {
  source(preprocessed_config)
  cat("✓ Loaded PREPROCESSED covariate config\n")
}

library(terra)
library(sf)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  ADD WATER & URBAN TRAINING POINTS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

data_dir <- "data/processed/phase4_modeling"

N_WATER_POINTS <- 500
N_URBAN_POINTS <- 500
TEST_PROPORTION <- 0.30
RANDOM_SEED <- 42

water_file <- "data/hex/Waterbodies_FeaturesToJSON.geojson"

# AOI shapefile — same boundary all rasters were clipped to
AOI_SHAPEFILE <- "D:/FIA_NEFIN/data/aoi/region.shp"

# =============================================================================
# STEP 0: LOAD AOI AND COVARIATE CONFIG
# =============================================================================

cat("Step 0: Loading AOI and covariate config...\n\n")

# Load AOI
if (!file.exists(AOI_SHAPEFILE)) {
  stop("AOI shapefile not found: ", AOI_SHAPEFILE,
       "\n  This is the same AOI used in PHASE4_00_preprocess_rasters.R")
}

aoi <- st_read(AOI_SHAPEFILE, quiet = TRUE)
aoi <- st_transform(aoi, 5070)
cat("  ✓ AOI loaded:", nrow(aoi), "features\n")

# Get active covariates from config
active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)
fine_covs <- Filter(function(x) x$scale == "fine", active_covs)
coarse_covs <- Filter(function(x) x$scale == "coarse", active_covs)

cat("  ✓ Covariates:", length(active_covs),
    "(", length(fine_covs), "fine +", length(coarse_covs), "coarse)\n\n")

# =============================================================================
# STEP 1: LOAD REFERENCE DATA (for column structure)
# =============================================================================

cat("Step 1: Loading reference data...\n")

ref_file <- file.path(data_dir, "train_pooled.csv")
if (!file.exists(ref_file)) stop("Run PHASE4_01_prep_data.R first!")

ref_data <- read_csv(ref_file, show_col_types = FALSE)
ref_data <- ref_data %>% mutate(CN = as.character(CN))

covariates_10m <- grep("_10m$", names(ref_data), value = TRUE)
covariates_250m <- grep("_250m$", names(ref_data), value = TRUE)
all_cov_cols <- c(covariates_10m, covariates_250m)

# Training extent for urban sampling
ref_sf <- st_as_sf(ref_data, coords = c("lon", "lat"), crs = 4326)
training_extent <- st_bbox(st_transform(ref_sf, 5070))

cat("  ✓", nrow(ref_data), "reference points,", length(all_cov_cols), "covariates\n\n")

# =============================================================================
# STEP 2: SAMPLE WATER POINTS (CLIPPED TO AOI)
# =============================================================================

cat("Step 2: Sampling water points (clipped to AOI)...\n")

water_locations <- NULL

if (!file.exists(water_file)) {
  cat("  ⚠ Water layer not found:", water_file, "\n\n")
} else {
  water <- st_read(water_file, quiet = TRUE)
  water <- st_transform(water, 5070)
  cat("  Raw water features:", nrow(water), "\n")
  
  # Clip to AOI — ensures all points fall within raster coverage
  water_clipped <- st_intersection(water, st_union(aoi))
  water_clipped <- st_make_valid(water_clipped)
  
  # Remove tiny slivers from intersection
  water_areas <- as.numeric(st_area(water_clipped))
  water_clipped <- water_clipped[water_areas > 100, ]  # > 100 m²
  cat("  After AOI clip:", nrow(water_clipped), "features\n")
  
  if (nrow(water_clipped) == 0) {
    cat("  ⚠ No water features within AOI!\n\n")
  } else {
    set.seed(RANDOM_SEED)
    water_points <- st_sample(water_clipped, size = N_WATER_POINTS, type = "random")
    water_points_sf <- st_as_sf(data.frame(geometry = water_points))
    st_crs(water_points_sf) <- 5070
    
    coords_5070 <- st_coordinates(water_points_sf)
    coords_4326 <- st_coordinates(st_transform(water_points_sf, 4326))
    
    water_locations <- data.frame(
      x = coords_5070[, 1], y = coords_5070[, 2],
      lon = coords_4326[, 1], lat = coords_4326[, 2],
      type = "water",
      point_id = paste0("WATER_", seq_len(nrow(coords_5070)))
    )
    cat("  ✓ Sampled", nrow(water_locations), "water points\n\n")
  }
}

# =============================================================================
# STEP 3: SAMPLE URBAN POINTS (FROM RASTER PIXELS)
# =============================================================================

cat("Step 3: Sampling urban points (from low-NDVI raster pixels)...\n")

urban_locations <- NULL

ndvi_paths <- c(
  "D:/FIA_NEFIN/data/covariates/fine_10m_preprocessed/ndvi_s2_10m_5070_template.tif",
  "D:/FIA_NEFIN/data/covariates/fine_10m/S2_NDVI_10m_2020_2024.tif"
)

ndvi_file <- NULL
for (path in ndvi_paths) {
  if (file.exists(path)) { ndvi_file <- path; break }
}

if (is.null(ndvi_file)) {
  cat("  ⚠ NDVI not found, skipping urban\n\n")
} else {
  cat("  Using:", basename(ndvi_file), "\n")
  
  tryCatch({
    ndvi <- rast(ndvi_file)
    extent_vect <- vect(st_as_sfc(training_extent, crs = 5070))
    ndvi_cropped <- crop(ndvi, extent_vect)
    
    ndvi_vals <- values(ndvi_cropped, mat = FALSE)
    urban_mask <- (ndvi_vals < 0.2) & (ndvi_vals > -0.1) & !is.na(ndvi_vals)
    n_urban_px <- sum(urban_mask, na.rm = TRUE)
    cat("  Urban-candidate pixels:", n_urban_px, "\n")
    
    if (n_urban_px > 0) {
      urban_cells <- which(urban_mask)
      set.seed(RANDOM_SEED)
      sampled_cells <- sample(urban_cells, min(N_URBAN_POINTS, length(urban_cells)))
      
      urban_coords <- xyFromCell(ndvi_cropped, sampled_cells)
      urban_sf <- st_as_sf(
        data.frame(x = urban_coords[, 1], y = urban_coords[, 2]),
        coords = c("x", "y"), crs = 5070
      )
      coords_4326 <- st_coordinates(st_transform(urban_sf, 4326))
      
      urban_locations <- data.frame(
        x = urban_coords[, 1], y = urban_coords[, 2],
        lon = coords_4326[, 1], lat = coords_4326[, 2],
        type = "urban",
        point_id = paste0("URBAN_", seq_len(nrow(urban_coords)))
      )
      cat("  ✓ Sampled", nrow(urban_locations), "urban points\n\n")
    }
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n\n")
  })
}

# =============================================================================
# STEP 4: COMBINE
# =============================================================================

all_locations <- bind_rows(water_locations, urban_locations)
if (is.null(all_locations) || nrow(all_locations) == 0) stop("No points sampled!")

cat("Step 4: Combined", nrow(all_locations), "locations\n")
cat("  Water:", sum(all_locations$type == "water"), "\n")
cat("  Urban:", sum(all_locations$type == "urban"), "\n\n")

# =============================================================================
# STEP 5: EXTRACT COVARIATES (CONFIG-DRIVEN)
# =============================================================================
# Uses the exact same approach as PHASE4_extract_covariates.R:
#   - File paths from COVARIATES config
#   - WGS84 points → terra::extract handles CRS
#   - Verbose reporting per covariate

cat("Step 5: Extracting covariates...\n\n")

points_sf_4326 <- st_as_sf(all_locations, coords = c("lon", "lat"), crs = 4326)

extracted_df <- data.frame(matrix(NA, nrow = nrow(all_locations), ncol = length(all_cov_cols)))
names(extracted_df) <- all_cov_cols

for (key in names(active_covs)) {
  
  cov <- active_covs[[key]]
  col_name <- paste0(cov$name, "_", gsub("m", "", cov$resolution), "m")
  if (!col_name %in% all_cov_cols) next
  
  if (!file.exists(cov$path)) {
    cat("  ✗", col_name, "— file not found:", basename(cov$path), "\n")
    next
  }
  
  tryCatch({
    r <- rast(cov$path)
    result <- terra::extract(r, points_sf_4326, ID = FALSE)
    vals <- result[, 1]
    extracted_df[[col_name]] <- vals
    
    n_na <- sum(is.na(vals))
    n_ok <- length(vals) - n_na
    
    if (n_na == 0) {
      cat(sprintf("  ✓ %-25s  %d/%d  [%.3f, %.3f]\n",
                  col_name, n_ok, length(vals),
                  min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
    } else {
      cat(sprintf("  ~ %-25s  %d/%d  (%d NA)\n", col_name, n_ok, length(vals), n_na))
    }
  }, error = function(e) {
    cat(sprintf("  ✗ %-25s  ERROR: %s\n", col_name, e$message))
  })
}

cat("\n")

# =============================================================================
# STEP 6: DROP INCOMPLETE POINTS (no imputation)
# =============================================================================

cat("Step 6: Checking completeness...\n")

output_df <- bind_cols(all_locations, extracted_df)
complete_mask <- complete.cases(output_df[, all_cov_cols])
n_complete <- sum(complete_mask)
n_dropped <- sum(!complete_mask)

cat("  Complete:", n_complete, "/", nrow(output_df), "\n")

if (n_dropped > 0) {
  dropped <- output_df[!complete_mask, ]
  cat("  Dropped:", n_dropped,
      "(water:", sum(dropped$type == "water"),
      ", urban:", sum(dropped$type == "urban"), ")\n")
  
  # Show which covariates caused drops
  na_covs <- sapply(all_cov_cols, function(c) sum(is.na(dropped[[c]])))
  na_covs <- na_covs[na_covs > 0]
  cat("  Covariates with NAs:", paste(names(na_covs), collapse = ", "), "\n")
  
  output_df <- output_df[complete_mask, ]
}

cat("  Final:", nrow(output_df), "points\n")
cat("    Water:", sum(output_df$type == "water"), "\n")
cat("    Urban:", sum(output_df$type == "urban"), "\n\n")

if (nrow(output_df) < 50) {
  cat("  ⚠ Very few complete points! Check:\n")
  cat("    - Is the AOI shapefile correct?\n")
  cat("    - Do rasters cover the AOI?\n")
  cat("    - Try increasing N_WATER_POINTS to compensate\n\n")
}

# =============================================================================
# STEP 7: SET METADATA
# =============================================================================

cat("Step 7: Setting biomass = 0 and metadata...\n")

output_df$biomass <- 0
output_df$CN <- as.character(output_df$point_id)
output_df$dataset <- output_df$type
output_df$lat_for_extraction <- output_df$lat
output_df$lon_for_extraction <- output_df$lon

cat("  ✓ Done\n\n")

# =============================================================================
# STEP 8: SPLIT TRAIN/TEST
# =============================================================================

cat("Step 8: Splitting train/test (", TEST_PROPORTION * 100, "% test)...\n", sep = "")

set.seed(RANDOM_SEED)
n_test <- round(nrow(output_df) * TEST_PROPORTION)
test_idx <- sample(seq_len(nrow(output_df)), n_test)
train_idx <- setdiff(seq_len(nrow(output_df)), test_idx)

train_wu <- output_df[train_idx, ]
test_wu <- output_df[test_idx, ]

cat("  Train:", nrow(train_wu), " | Test:", nrow(test_wu), "\n\n")

# =============================================================================
# STEP 9: ADD TO EXISTING FILES
# =============================================================================

cat("Step 9: Adding to existing train/test files...\n\n")

add_and_save <- function(original_file, new_points, output_file) {
  if (!file.exists(original_file)) {
    cat("  ⚠", basename(original_file), "not found\n")
    return(invisible(NULL))
  }
  
  original <- read_csv(original_file, show_col_types = FALSE) %>%
    mutate(CN = as.character(CN))
  
  # Remove previous water/urban points (makes script re-runnable)
  n_before <- nrow(original)
  original <- original %>% filter(!grepl("^WATER_|^URBAN_", CN))
  n_removed <- n_before - nrow(original)
  if (n_removed > 0) cat("    Removed", n_removed, "previous water/urban\n")
  
  # Align columns
  for (col in setdiff(names(original), names(new_points))) new_points[[col]] <- NA
  new_aligned <- new_points[, intersect(names(original), names(new_points))]
  for (col in setdiff(names(original), names(new_aligned))) new_aligned[[col]] <- NA
  
  enhanced <- bind_rows(original, new_aligned[, names(original)])
  write_csv(enhanced, output_file)
  
  cat("  ✓", basename(output_file), "\n")
  cat("    Forest:", sum(enhanced$biomass > 0, na.rm = TRUE),
      " | Water/urban:", sum(enhanced$biomass == 0, na.rm = TRUE),
      " | Total:", nrow(enhanced), "\n")
}

for (file in c("train_fia_only.csv", "train_nefin_only.csv",
               "train_pooled.csv", "test_data.csv")) {
  filepath <- file.path(data_dir, file)
  if (!file.exists(filepath)) next
  
  pts <- if (grepl("test", file)) test_wu else train_wu
  outfile <- file.path(data_dir, gsub("\\.csv$", "_with_water_urban.csv", file))
  
  cat("  ", file, ":\n", sep = "")
  add_and_save(filepath, pts, outfile)
  cat("\n")
}

# =============================================================================
# STEP 10: VERIFICATION
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VERIFICATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

all_pass <- TRUE

for (file in c("train_fia_only.csv", "train_nefin_only.csv",
               "train_pooled.csv", "test_data.csv")) {
  ef <- file.path(data_dir, gsub("\\.csv$", "_with_water_urban.csv", file))
  if (!file.exists(ef)) next
  
  df <- read_csv(ef, show_col_types = FALSE)
  wu <- df %>% filter(biomass == 0)
  cov_check <- intersect(all_cov_cols, names(df))
  
  if (nrow(wu) == 0) {
    cat("  ✗", basename(ef), "— no water/urban points\n")
    all_pass <- FALSE
    next
  }
  
  wu_complete <- sum(complete.cases(wu[, cov_check]))
  status <- if (wu_complete == nrow(wu)) "✓ PASS" else "✗ FAIL"
  if (wu_complete != nrow(wu)) all_pass <- FALSE
  
  cat(sprintf("  %s  %-40s  wu=%d  complete=%d/%d\n",
              status, basename(ef), nrow(wu), wu_complete, nrow(wu)))
}

cat("\n")
if (all_pass) {
  cat("  ✓ ALL PASS — ready for model training\n\n")
} else {
  cat("  ⚠ SOME FAILED — check extraction output above\n\n")
}

# =============================================================================
# DONE
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  NEXT: Retrain models\n")
cat("  Rscript R/phase4_modeling/PHASE4_02b_spatial_cv.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n") 