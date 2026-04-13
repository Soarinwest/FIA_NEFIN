# R/03_create_comparison_datasets

Creates the baseline and augmented datasets used for the FIA vs NEFIN
comparison in Paper 1.

## Scripts

1. `01_validate_inputs.R`
 Validates FIA and NEFIN datasets.
 Checks for overlapping plots (by CN).
 Ensures schemas match.

2. `02_create_baseline.R`
 Creates FIA-only dataset.
 All coordinates are fuzzed.
 Serves as the comparison benchmark.

3. `03_create_augmented.R`
 Combines FIA and NEFIN.
 For overlapping plots: uses NEFIN (true coordinates).
 For FIA-only plots: keeps fuzzed coordinates.
 Adds all NEFIN-only plots.

## Outputs

- `baseline.csv` -- FIA-only dataset (all fuzzed coordinates)
- `augmented.csv` -- FIA + NEFIN combined (mixed coordinate sources)
