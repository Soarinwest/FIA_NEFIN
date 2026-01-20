# Phase C: Create Comparison Datasets

**This is the key phase!** Creates baseline vs augmented datasets for the correct comparison.

## The Correct Research Question

**Does augmenting FIA with NEFIN's precise coordinates improve biomass estimates?**

NOT: "Is NEFIN better than FIA?" (different networks)

## Scripts

1. **01_validate_inputs.R**
   - Validates FIA and NEFIN datasets
   - Checks for overlapping plots (by CN)
   - Ensures schemas match

2. **02_create_baseline.R**
   - Creates FIA-only dataset
   - All coordinates fuzzed
   - Comparison benchmark

3. **03_create_augmented.R**
   - Combines FIA + NEFIN
   - For overlaps: uses NEFIN (true coords)
   - For FIA-only: keeps fuzzed
   - Adds all NEFIN plots

## Outputs

- **baseline.csv**: FIA-only (~22k plots, all fuzzed)
- **augmented.csv**: FIA + NEFIN (~25k plots, mixed coords)

## Why This Approach?

We're testing if adding precise coordinates to the FIA network improves estimates.
Both datasets cover the same region, but augmented has better coordinate precision
for NEFIN plots.
