# =============================================================================
# QUICK DATA QUALITY VERIFICATION (5 minutes)
# =============================================================================

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  QUICK DATA QUALITY CHECKS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

library(dplyr)
library(readr)

# =============================================================================
# CHECK 1: EMPTY HEXAGONS
# =============================================================================

cat("CHECK 1: Empty Hexagons\n")
cat("───────────────────────────────────────────────────────────────────\n")

hex_files <- c(
  "100ha" = "data/processed/hex_aggregated/fia_hex_100ha.csv",
  "1kha" = "data/processed/hex_aggregated/fia_hex_1kha.csv",
  "10kha" = "data/processed/hex_aggregated/fia_hex_10kha.csv",
  "100kha" = "data/processed/hex_aggregated/fia_hex_100kha.csv"
)

for (scale in names(hex_files)) {
  if (file.exists(hex_files[scale])) {
    hex <- read_csv(hex_files[scale], show_col_types = FALSE)
    
    empty <- sum(hex$n_plots == 0, na.rm = TRUE)
    pct_empty <- round(100 * empty / nrow(hex), 1)
    
    cat(sprintf("  %-8s: %5d total | %5d empty (%4.1f%%) ", 
                scale, nrow(hex), empty, pct_empty))
    
    if (pct_empty > 50) {
      cat("⚠ HIGH\n")
    } else if (pct_empty > 20) {
      cat("⚠\n")
    } else {
      cat("✓\n")
    }
  }
}

cat("\n")

# =============================================================================
# CHECK 2: NAs IN KEY VARIABLES
# =============================================================================

cat("CHECK 2: Missing Values (NAs)\n")
cat("───────────────────────────────────────────────────────────────────\n")

# FIA plots
if (file.exists("data/processed/fia_plots.csv")) {
  fia <- read_csv("data/processed/fia_plots.csv", show_col_types = FALSE)
  
  cat("FIA Plots:\n")
  cat(sprintf("  Total plots: %s\n", format(nrow(fia), big.mark = ",")))
  
  key_vars <- c("biomass_Mg_ha", "ndvi_s2_mean", "tmean_prism", "ppt_prism")
  for (var in key_vars) {
    if (var %in% names(fia)) {
      na_count <- sum(is.na(fia[[var]]))
      pct_na <- round(100 * na_count / nrow(fia), 1)
      
      cat(sprintf("  %-20s: %5d NAs (%4.1f%%) ", var, na_count, pct_na))
      
      if (pct_na > 10) {
        cat("⚠ HIGH\n")
      } else if (pct_na > 0) {
        cat("⚠\n")
      } else {
        cat("✓\n")
      }
    }
  }
  cat("\n")
}

# NEFIN plots
nefin_files <- c(
  "data/processed/nefin_plots.csv",
  "data/raw/nefin/TREE_PLOT_DATA.csv"
)

nefin_file <- nefin_files[file.exists(nefin_files)][1]

if (!is.na(nefin_file)) {
  nefin <- read_csv(nefin_file, show_col_types = FALSE)
  
  cat("NEFIN Plots:\n")
  cat(sprintf("  Total plots: %s\n", format(nrow(nefin), big.mark = ",")))
  
  # Find biomass column
  biomass_col <- intersect(names(nefin), c("biomass_Mg_ha", "biomass", "Biomass"))[1]
  ndvi_col <- intersect(names(nefin), c("ndvi_s2_mean", "NDVI", "ndvi"))[1]
  
  if (!is.na(biomass_col)) {
    na_count <- sum(is.na(nefin[[biomass_col]]))
    pct_na <- round(100 * na_count / nrow(nefin), 1)
    cat(sprintf("  %-20s: %5d NAs (%4.1f%%) ", biomass_col, na_count, pct_na))
    if (pct_na > 0) cat("⚠\n") else cat("✓\n")
  }
  
  if (!is.na(ndvi_col)) {
    na_count <- sum(is.na(nefin[[ndvi_col]]))
    pct_na <- round(100 * na_count / nrow(nefin), 1)
    cat(sprintf("  %-20s: %5d NAs (%4.1f%%) ", ndvi_col, na_count, pct_na))
    if (pct_na > 0) cat("⚠\n") else cat("✓\n")
  }
  
  cat("\n")
}

# =============================================================================
# CHECK 3: ZERO BIOMASS
# =============================================================================

cat("CHECK 3: Zero Biomass Values\n")
cat("───────────────────────────────────────────────────────────────────\n")

if (file.exists("data/processed/fia_plots.csv")) {
  fia <- read_csv("data/processed/fia_plots.csv", show_col_types = FALSE)
  
  if ("biomass_Mg_ha" %in% names(fia)) {
    zero_count <- sum(fia$biomass_Mg_ha == 0, na.rm = TRUE)
    pct_zero <- round(100 * zero_count / nrow(fia), 1)
    
    cat(sprintf("  FIA:   %5d zeros (%4.1f%%) ", zero_count, pct_zero))
    
    if (pct_zero > 5) {
      cat("⚠ Check if legitimate\n")
    } else {
      cat("✓\n")
    }
    
    # Check distribution
    cat(sprintf("  Range: %.1f - %.1f Mg/ha\n", 
                min(fia$biomass_Mg_ha, na.rm = TRUE),
                max(fia$biomass_Mg_ha, na.rm = TRUE)))
  }
}

if (!is.na(nefin_file) && !is.na(biomass_col)) {
  nefin <- read_csv(nefin_file, show_col_types = FALSE)
  
  zero_count <- sum(nefin[[biomass_col]] == 0, na.rm = TRUE)
  pct_zero <- round(100 * zero_count / nrow(nefin), 1)
  
  cat(sprintf("  NEFIN: %5d zeros (%4.1f%%) ", zero_count, pct_zero))
  
  if (pct_zero > 5) {
    cat("⚠ Check if legitimate\n")
  } else {
    cat("✓\n")
  }
  
  cat(sprintf("  Range: %.1f - %.1f Mg/ha\n", 
              min(nefin[[biomass_col]], na.rm = TRUE),
              max(nefin[[biomass_col]], na.rm = TRUE)))
}

cat("\n")

# =============================================================================
# CHECK 4: MONTE CARLO OUTLIERS
# =============================================================================

cat("CHECK 4: Monte Carlo Uncertainty Outliers\n")
cat("───────────────────────────────────────────────────────────────────\n")

if (file.exists("data/processed/monte_carlo/plot_uncertainty.csv")) {
  mc <- read_csv("data/processed/monte_carlo/plot_uncertainty.csv", 
                 show_col_types = FALSE)
  
  cat("NDVI Uncertainty (SD across 100 replicates):\n")
  cat(sprintf("  Mean:   %.4f\n", mean(mc$ndvi_s2_sd, na.rm = TRUE)))
  cat(sprintf("  Median: %.4f\n", median(mc$ndvi_s2_sd, na.rm = TRUE)))
  cat(sprintf("  SD:     %.4f\n", sd(mc$ndvi_s2_sd, na.rm = TRUE)))
  cat(sprintf("  Max:    %.4f\n", max(mc$ndvi_s2_sd, na.rm = TRUE)))
  
  # Check for extreme outliers (>3 SD from mean)
  mean_val <- mean(mc$ndvi_s2_sd, na.rm = TRUE)
  sd_val <- sd(mc$ndvi_s2_sd, na.rm = TRUE)
  outliers <- sum(mc$ndvi_s2_sd > mean_val + 3*sd_val, na.rm = TRUE)
  
  cat(sprintf("  Outliers (>3 SD): %d ", outliers))
  if (outliers > 100) {
    cat("⚠ HIGH\n")
  } else if (outliers > 0) {
    cat("⚠\n")
  } else {
    cat("✓\n")
  }
  
  cat("\n")
}

# =============================================================================
# CHECK 5: AUGMENTED HEX ISSUES
# =============================================================================

cat("CHECK 5: Augmented Hex Data\n")
cat("───────────────────────────────────────────────────────────────────\n")

if (file.exists("data/processed/hex_aggregated/augmented_hex_100ha.csv")) {
  aug <- read_csv("data/processed/hex_aggregated/augmented_hex_100ha.csv",
                  show_col_types = FALSE)
  
  cat("100ha Augmented Hexes:\n")
  cat(sprintf("  Total hexes: %s\n", format(nrow(aug), big.mark = ",")))
  
  # Check NEFIN content
  nefin_hexes <- sum(aug$pct_nefin > 0, na.rm = TRUE)
  cat(sprintf("  Hexes with NEFIN: %s (%.1f%%)\n", 
              format(nefin_hexes, big.mark = ","),
              100 * nefin_hexes / nrow(aug)))
  
  # Check if augmentation improved
  if ("biomass_se" %in% names(aug)) {
    baseline_file <- "data/processed/hex_aggregated/fia_hex_100ha.csv"
    if (file.exists(baseline_file)) {
      baseline <- read_csv(baseline_file, show_col_types = FALSE)
      
      # Compare SE (should decrease with more data)
      aug_se <- mean(aug$biomass_se, na.rm = TRUE)
      base_se <- mean(baseline$biomass_se, na.rm = TRUE)
      
      cat(sprintf("  Mean SE baseline: %.2f\n", base_se))
      cat(sprintf("  Mean SE augmented: %.2f\n", aug_se))
      
      if (aug_se < base_se) {
        cat("  ✓ Augmentation reduced SE\n")
      } else {
        cat("  ⚠ Augmentation INCREASED SE (composition issue!)\n")
      }
    }
  }
}

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Key Issues to Address:\n\n")

cat("1. Empty Hexagons:\n")
cat("   → Filter out hexes with n_plots == 0\n")
cat("   → Recalculate all hex statistics\n\n")

cat("2. Missing Values:\n")
cat("   → Document which variables have NAs\n")
cat("   → Decide: impute or exclude?\n\n")

cat("3. Zero Biomass:\n")
cat("   → Verify if legitimate (clear-cuts, urban, water)\n")
cat("   → Or data extraction errors?\n\n")

cat("4. Composition Bias:\n")
cat("   → NEFIN plots systematically different\n")
cat("   → Don't pool naively!\n")
cat("   → Separate precision from composition effects\n\n")

cat("Verification complete!\n")
cat("See DATA_QUALITY_AND_STRUCTURE_REVIEW.md for full details\n\n")