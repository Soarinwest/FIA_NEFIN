# =============================================================================
# Convert NEFIN Biomass - FINAL
# =============================================================================

source("R/00_config/config.R")
source("R/utils/biomass_utils.R")

library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  PHASE B - STEP 2: CONVERT BIOMASS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

output_dir <- file.path(CONFIG$paths$interim_nefin, "biomass")
ensure_dir(output_dir)

# Load joined data
input_path <- file.path(CONFIG$paths$interim_nefin, "cleaned", "nefin_joined.csv")
nefin <- read_csv(input_path, show_col_types = FALSE)

cat("Loaded:", nrow(nefin), "rows\n\n")

# Convert biomass: kg/ha → Mg/ha
cat("Converting biomass (kg/ha → Mg/ha)...\n")
nefin_biomass <- nefin %>%
  mutate(biomass = biomass_kg_ha / 1000)

# Validate
cat("\nBiomass statistics (Mg/ha):\n")
stats <- summarize_biomass(nefin_biomass, "biomass")
cat(sprintf("  Min:    %.2f\n", stats$min))
cat(sprintf("  Median: %.2f\n", stats$median))
cat(sprintf("  Mean:   %.2f\n", stats$mean))
cat(sprintf("  Max:    %.2f\n", stats$max))
cat("\n")

# Save
output_path <- file.path(output_dir, "nefin_plot_biomass.csv")
write_csv(nefin_biomass, output_path)

cat("✓ Saved:", output_path, "\n\n")
cat("Next: R/02_process_nefin/03_create_nefin_dataset.R\n\n")