# =============================================================================
# PHASE 4 - STEP 1: Data Preparation for Predictive Modeling (CONFIG-BASED)
# =============================================================================
# Prepares data for model training:
# - Loads baseline and augmented datasets
# - Checks available covariates against config
# - Creates train/test splits (70/30)
# - DOES NOT standardize (done within CV folds instead)
# - Saves prepared datasets for each scenario
#
# ADAPTIVE: Works with any number of covariates defined in config!
# =============================================================================

# Load configuration
source("R/00_config/PHASE4_config.R")

library(dplyr)
library(readr)
library(caret)  # For data splitting

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4 - STEP 1: DATA PREPARATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Print configuration
print_phase4_config()

# Create output directories
dir.create("data/processed/phase4_modeling", showWarnings = FALSE, recursive = TRUE)
output_dir <- "data/processed/phase4_modeling"

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Step 1: Loading datasets...\n")

baseline <- read_csv("data/processed/baseline_with_covariates.csv", 
                     show_col_types = FALSE)
augmented <- read_csv("data/processed/augmented_with_covariates.csv",
                      show_col_types = FALSE)

cat("  ✓ Baseline:", nrow(baseline), "plots\n")
cat("  ✓ Augmented:", nrow(augmented), "plots\n\n")

# =============================================================================
# CHECK AVAILABLE COVARIATES
# =============================================================================

cat("Step 2: Checking available covariates...\n")

# Get active covariates from config
source("R/00_config/PHASE4_config_covariates.R")

active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)

# Extract just the names (not the full list objects)
active_cov_names <- sapply(active_covs, function(x) x$name)

# Check which are actually in the data
available_covs <- intersect(active_cov_names, names(augmented))
missing_covs <- setdiff(active_cov_names, names(augmented))

cat("  Active covariates:", length(active_covs), "\n")
cat("  Available in data:", length(available_covs), "\n")

if (length(missing_covs) > 0) {
  cat("  ⚠ Missing covariates:", paste(missing_covs, collapse = ", "), "\n")
  cat("    These will be added when layers are processed\n")
}

cat("\n  Using covariates:\n")
for (cov in available_covs) {
  meta <- COVARIATES[[cov]]
  if (!is.null(meta)) {
    cat(sprintf("    • %s (%s, %s)\n", 
                meta$name, meta$resolution, meta$type))
  } else {
    cat(sprintf("    • %s\n", cov))
  }
}
cat("\n")

# Update active covariates to only available ones
covariates_to_use <- available_covs

# =============================================================================
# PREPARE FULL DATASET
# =============================================================================

cat("Step 3: Preparing full dataset...\n")

# Add dataset identifier to baseline
baseline <- baseline %>%
  mutate(dataset = "FIA")

# Combine into single dataset
full_data <- augmented %>%
  select(CN, dataset, biomass, lon, lat, all_of(covariates_to_use)) %>%
  filter(!is.na(biomass), biomass > 0)

cat("  ✓ Combined dataset:", nrow(full_data), "plots\n")
cat("    FIA:", sum(full_data$dataset == "FIA"), "\n")
cat("    NEFIN:", sum(full_data$dataset == "NEFIN"), "\n\n")

# =============================================================================
# CREATE TRAIN/TEST SPLITS (STRATIFIED BY DATASET)
# =============================================================================

cat("Step 4: Creating train/test splits (70/30)...\n")

set.seed(PHASE4_CONFIG$cv$seed)

# Stratified split to maintain FIA/NEFIN proportions
trainIndex <- createDataPartition(
  full_data$dataset, 
  p = 0.70,
  list = FALSE,
  times = 1
)

train_data <- full_data[trainIndex, ]
test_data <- full_data[-trainIndex, ]

cat("  Training set:", nrow(train_data), "plots\n")
cat("    FIA:", sum(train_data$dataset == "FIA"), "\n")
cat("    NEFIN:", sum(train_data$dataset == "NEFIN"), "\n")
cat("  Test set:", nrow(test_data), "plots\n")
cat("    FIA:", sum(test_data$dataset == "FIA"), "\n")
cat("    NEFIN:", sum(test_data$dataset == "NEFIN"), "\n\n")

# =============================================================================
# SKIP GLOBAL STANDARDIZATION (done within CV folds instead)
# =============================================================================

cat("Step 5: Skipping global standardization...\n")
cat("  Covariates will be standardized within each CV fold\n")
cat("  (Prevents data leakage in spatial cross-validation)\n\n")

# Calculate raw statistics for reference (not used for standardization)
scaling_params <- data.frame(
  covariate = covariates_to_use,
  mean = sapply(train_data[, covariates_to_use], mean, na.rm = TRUE),
  sd = sapply(train_data[, covariates_to_use], sd, na.rm = TRUE)
)

cat("  Raw covariate statistics (for reference only):\n")
print(scaling_params, row.names = FALSE)
cat("\n")

# =============================================================================
# CREATE SCENARIO-SPECIFIC DATASETS
# =============================================================================

cat("Step 6: Creating scenario-specific datasets...\n")

for (scenario_name in names(PHASE4_CONFIG$scenarios)) {
  scenario <- PHASE4_CONFIG$scenarios[[scenario_name]]
  
  # Filter data based on scenario
  train_scenario <- train_data %>% filter(eval(parse(text = scenario$filter)))
  test_scenario <- test_data %>% filter(eval(parse(text = scenario$filter)))
  
  cat(sprintf("  %s:\n", scenario$name))
  cat(sprintf("    Train: %d plots\n", nrow(train_scenario)))
  cat(sprintf("    Test:  %d plots\n", nrow(test_scenario)))
  
  # Save
  write_csv(train_scenario, 
            file.path(output_dir, paste0("train_", scenario_name, ".csv")))
  write_csv(test_scenario, 
            file.path(output_dir, paste0("test_", scenario_name, ".csv")))
}

cat("\n")

# =============================================================================
# SAVE COMPLETE DATASETS
# =============================================================================

cat("Step 7: Saving prepared datasets...\n")

# Save full train/test
write_csv(train_data, file.path(output_dir, "train_data.csv"))
write_csv(test_data, file.path(output_dir, "test_data.csv"))

# Save scaling parameters (for reference, not used for actual standardization)
write_csv(scaling_params, file.path(output_dir, "scaling_parameters.csv"))

# Save metadata
metadata <- list(
  creation_date = Sys.time(),
  n_total = nrow(full_data),
  n_train = nrow(train_data),
  n_test = nrow(test_data),
  train_test_ratio = 0.70,
  seed = PHASE4_CONFIG$cv$seed,
  covariates = covariates_to_use,
  n_covariates = length(covariates_to_use),
  scenarios = names(PHASE4_CONFIG$scenarios),
  scaling_method = "within-fold (done in CV script)"
)

write_csv(
  data.frame(
    parameter = names(metadata),
    value = sapply(metadata, function(x) paste(x, collapse = ", "))
  ),
  file.path(output_dir, "metadata.csv")
)

cat("  ✓ train_data.csv (RAW values - not standardized)\n")
cat("  ✓ test_data.csv (RAW values - not standardized)\n")
cat("  ✓ train/test sets for each scenario\n")
cat("  ✓ scaling_parameters.csv (reference only)\n")
cat("  ✓ metadata.csv\n\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DATA PREPARATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Response variable: Aboveground Biomass (Mg/ha)\n")
cat("  Range:", round(min(full_data$biomass), 1), "to", 
    round(max(full_data$biomass), 1), "Mg/ha\n")
cat("  Mean:", round(mean(full_data$biomass), 1), "Mg/ha\n")
cat("  SD:", round(sd(full_data$biomass), 1), "Mg/ha\n\n")

cat("Covariates:", length(covariates_to_use), "\n")
for (cov in covariates_to_use) {
  cat("  •", cov, "\n")
}
cat("\n")

cat("Datasets created:\n")
cat("  • Full (FIA + NEFIN):", nrow(train_data), "train,", nrow(test_data), "test\n")
for (scenario_name in names(PHASE4_CONFIG$scenarios)) {
  scenario <- PHASE4_CONFIG$scenarios[[scenario_name]]
  n_train <- nrow(train_data %>% filter(eval(parse(text = scenario$filter))))
  n_test <- nrow(test_data %>% filter(eval(parse(text = scenario$filter))))
  cat("  •", scenario$name, ":", n_train, "train,", n_test, "test\n")
}
cat("\n")

cat("Output directory:", output_dir, "\n")
cat("Files ready for model training!\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  NEXT STEP: Spatial cross-validation (with within-fold scaling)\n")
cat("  Rscript R/phase4_modeling/PHASE4_02b_spatial_cv.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")