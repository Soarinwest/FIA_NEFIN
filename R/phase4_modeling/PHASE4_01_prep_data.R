# =============================================================================
# PHASE 4 - STEP 1: Data Preparation (CORRECTED RESEARCH DESIGN)
# =============================================================================
# DESIGN FOR FUZZING COMPARISON:
#
# TEST SET (same for ALL models):
#   - 30% of NEFIN (precise coordinates)
#   - Ensures fair comparison across all scenarios
#
# TRAINING SETS (vary by scenario):
#   - FIA-only: Train on ALL FIA (fuzzed coords), test on 30% NEFIN
#   - NEFIN-only: Train on 70% NEFIN (precise coords), test on 30% NEFIN
#   - Pooled: Train on ALL FIA + 70% NEFIN, test on 30% NEFIN
#
# This isolates the effect of coordinate fuzzing!
# =============================================================================

source("R/00_config/config.R")
source("R/00_config/PHASE4_config.R")
source("R/00_config/PHASE4_config_covariates.R")

library(dplyr)
library(readr)
library(caret)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 4 - STEP 1: DATA PREPARATION (FIXED)\n")
cat("  TEST: 30% NEFIN (same for all models)\n")
cat("  TRAIN: Varies by scenario\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Create output directory
dir.create("data/processed/phase4_modeling", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# STEP 1: LOAD DATASETS
# =============================================================================

cat("Step 1: Loading datasets...\n")

baseline <- read_csv(
  "data/processed/baseline_with_covariates.csv",
  show_col_types = FALSE
)

augmented <- read_csv(
  "data/processed/augmented_with_covariates.csv",
  show_col_types = FALSE
)

cat("  ✓ Baseline (FIA):", nrow(baseline), "plots\n")
cat("  ✓ Augmented (FIA + NEFIN):", nrow(augmented), "plots\n\n")

# Fix data type mismatches
cat("  Converting CN columns to character...\n")
baseline <- baseline %>%
  mutate(CN = as.character(CN))

augmented <- augmented %>%
  mutate(CN = as.character(CN))

cat("  ✓ Data types standardized\n\n")

# =============================================================================
# STEP 2: IDENTIFY COVARIATES WITH SCALE-SPECIFIC NAMES
# =============================================================================

cat("Step 2: Checking available covariates...\n")

# Get active covariates
active_covs <- Filter(function(x) !is.null(x$active) && x$active, COVARIATES)

# CRITICAL FIX: Create scale-specific names
covariates_to_use <- sapply(active_covs, function(x) {
  paste0(x$name, "_", gsub("m", "", x$resolution), "m")
})

cat("  Active covariates:", length(active_covs), "\n")

# Check which are available in the data
available_in_data <- covariates_to_use[covariates_to_use %in% names(augmented)]

if (length(available_in_data) == 0) {
  stop("ERROR: No covariates found in data! Check covariate extraction.")
}

cat("  Available in data:", length(available_in_data), "\n")

if (length(available_in_data) < length(covariates_to_use)) {
  missing <- setdiff(covariates_to_use, available_in_data)
  cat("  ⚠ Missing covariates:", paste(head(missing, 5), collapse = ", "))
  if (length(missing) > 5) cat(", ...")
  cat("\n")
}

cat("\n  Using covariates:\n   ")
cat(paste(head(available_in_data, 5), collapse = ", "))
if (length(available_in_data) > 5) {
  cat("\n    ... (", length(available_in_data) - 5, "more)")
}
cat("\n\n")

# Update to only use available covariates
covariates_to_use <- available_in_data

# =============================================================================
# STEP 3: SEPARATE FIA AND NEFIN
# =============================================================================

cat("Step 3: Separating FIA and NEFIN datasets...\n")

# FIA data (from baseline - all fuzzed coordinates)
fia <- baseline %>%
  filter(dataset == "FIA")

# NEFIN data (from augmented - precise coordinates)
nefin <- augmented %>%
  filter(dataset == "NEFIN")

cat("  ✓ FIA plots:", nrow(fia), "\n")
cat("  ✓ NEFIN plots:", nrow(nefin), "\n\n")

# =============================================================================
# STEP 4: CREATE UNIVERSAL TEST SET (30% NEFIN)
# =============================================================================

cat("Step 4: Creating universal test set...\n")
cat("  Using 30% of NEFIN data (same for ALL models)\n\n")

set.seed(PHASE4_CONFIG$cv$seed)

# Split NEFIN: 30% test, 70% train
test_indices <- createDataPartition(
  nefin$biomass,
  p = 0.30,
  list = FALSE
)

nefin_test <- nefin[test_indices, ]
nefin_train <- nefin[-test_indices, ]

cat("  ✓ Test set created:\n")
cat("    Plots:", nrow(nefin_test), "\n")
cat("    Source: NEFIN only (precise coordinates)\n")
cat("    Biomass range:", round(min(nefin_test$biomass), 1), "to", 
    round(max(nefin_test$biomass), 1), "Mg/ha\n\n")

# =============================================================================
# STEP 5: CREATE SCENARIO-SPECIFIC TRAINING SETS
# =============================================================================

cat("Step 5: Creating scenario-specific training sets...\n\n")

# Scenario 1: FIA Only (fuzzed coordinates)
cat("  FIA Only:\n")
cat("    Train:", nrow(fia), "plots (ALL FIA - fuzzed coords)\n")
cat("    Test:", nrow(nefin_test), "plots (30% NEFIN - precise coords)\n")
cat("    → Evaluates: How well do fuzzed coordinates predict true locations?\n\n")

# Scenario 2: NEFIN Only (precise coordinates)
cat("  NEFIN Only:\n")
cat("    Train:", nrow(nefin_train), "plots (70% NEFIN - precise coords)\n")
cat("    Test:", nrow(nefin_test), "plots (30% NEFIN - precise coords)\n")
cat("    → Evaluates: Best-case scenario with precise coordinates\n\n")

# Scenario 3: Pooled (FIA + NEFIN)
train_pooled <- bind_rows(fia, nefin_train)

cat("  Pooled:\n")
cat("    Train:", nrow(train_pooled), "plots\n")
cat("      - FIA:", nrow(fia), "(fuzzed coords)\n")
cat("      - NEFIN:", nrow(nefin_train), "(precise coords, 70%)\n")
cat("    Test:", nrow(nefin_test), "plots (30% NEFIN - precise coords)\n")
cat("    → Evaluates: Combined dataset performance\n\n")

# =============================================================================
# STEP 6: CALCULATE REFERENCE STATISTICS
# =============================================================================

cat("Step 6: Calculating reference statistics...\n")
cat("  NOTE: Standardization done within CV folds to prevent leakage\n\n")

# Calculate statistics for reference (not actually used for standardization)
cat("  Pooled training data statistics (reference only):\n")

scaling_stats <- data.frame(
  covariate = covariates_to_use,
  mean = sapply(train_pooled[, covariates_to_use], mean, na.rm = TRUE),
  sd = sapply(train_pooled[, covariates_to_use], sd, na.rm = TRUE),
  min = sapply(train_pooled[, covariates_to_use], min, na.rm = TRUE),
  max = sapply(train_pooled[, covariates_to_use], max, na.rm = TRUE)
)

print(head(scaling_stats, 5))
if (nrow(scaling_stats) > 5) {
  cat("    ... (", nrow(scaling_stats) - 5, "more covariates)\n")
}
cat("\n")

# =============================================================================
# STEP 7: SAVE DATASETS
# =============================================================================

cat("Step 7: Saving datasets...\n\n")

# Save test set (same for all scenarios)
write_csv(
  nefin_test,
  "data/processed/phase4_modeling/test_data.csv"
)
cat("  ✓ test_data.csv (30% NEFIN, n=", nrow(nefin_test), ")\n")

# Save training sets
write_csv(
  fia,
  "data/processed/phase4_modeling/train_fia_only.csv"
)
cat("  ✓ train_fia_only.csv (n=", nrow(fia), ")\n")

write_csv(
  nefin_train,
  "data/processed/phase4_modeling/train_nefin_only.csv"
)
cat("  ✓ train_nefin_only.csv (n=", nrow(nefin_train), ")\n")

write_csv(
  train_pooled,
  "data/processed/phase4_modeling/train_pooled.csv"
)
cat("  ✓ train_pooled.csv (n=", nrow(train_pooled), ")\n\n")

# Save scaling parameters (for reference)
write_csv(
  scaling_stats,
  "data/processed/phase4_modeling/scaling_parameters.csv"
)
cat("  ✓ scaling_parameters.csv (reference only)\n\n")

# Save metadata
metadata <- data.frame(
  item = c(
    "creation_date",
    "test_set_size",
    "test_set_source",
    "train_fia_size",
    "train_nefin_size",
    "train_pooled_size",
    "n_covariates",
    "seed"
  ),
  value = c(
    as.character(Sys.time()),
    nrow(nefin_test),
    "30% NEFIN (precise coords)",
    nrow(fia),
    nrow(nefin_train),
    nrow(train_pooled),
    length(covariates_to_use),
    PHASE4_CONFIG$cv$seed
  )
)

write_csv(
  metadata,
  "data/processed/phase4_modeling/metadata.csv"
)
cat("  ✓ metadata.csv\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  DATA PREPARATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("RESEARCH DESIGN:\n")
cat("  Test set: 30% NEFIN (", nrow(nefin_test), "plots) - SAME FOR ALL MODELS\n")
cat("  Training sets:\n")
cat("    • FIA-only:", nrow(fia), "plots (fuzzed coords)\n")
cat("    • NEFIN-only:", nrow(nefin_train), "plots (precise coords, 70%)\n")
cat("    • Pooled:", nrow(train_pooled), "plots (FIA + 70% NEFIN)\n\n")

cat("BIOMASS DISTRIBUTION:\n")
cat("  Test set:\n")
cat("    Range:", round(min(nefin_test$biomass), 1), "to", 
    round(max(nefin_test$biomass), 1), "Mg/ha\n")
cat("    Mean:", round(mean(nefin_test$biomass), 1), "Mg/ha\n")
cat("    SD:", round(sd(nefin_test$biomass)), "Mg/ha\n\n")

cat("COVARIATES:\n")
cat("  Using", length(covariates_to_use), "covariates\n")
cat("  Standardization: Within-fold (in CV script)\n\n")

cat("OUTPUT FILES:\n")
cat("  • test_data.csv - Universal test set (30% NEFIN)\n")
cat("  • train_fia_only.csv - FIA training data\n")
cat("  • train_nefin_only.csv - NEFIN training data (70%)\n")
cat("  • train_pooled.csv - Combined training data\n")
cat("  • scaling_parameters.csv - Reference statistics\n")
cat("  • metadata.csv - Run metadata\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  NEXT STEP: Spatial cross-validation\n")
cat("  Rscript R/phase4_modeling/PHASE4_02b_spatial_cv.R\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")