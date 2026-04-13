# R/07_edge_case_analysis

Diagnostic analysis scripts for Paper 1. These scripts are not part of
the core pipeline and do not need to be run to reproduce the main results.

They examine edge cases and structural properties of the FIA and NEFIN
datasets in detail: DBH distributions, species-level tail enrichment,
and data quality checks.

## Scripts

`00_nefin_data_quality_check.R`
 Checks for anomalous growth rates and flagged trees in the NEFIN dataset.
 Output in `data/processed/nefin_growth_check/`.

`01_compare_dataset_structure.R`
 Compares plot-level structural metrics between FIA and NEFIN
 (QMD, P95 DBH, percent large trees, mortality ratio).
 Output in `data/processed/edge_case_analysis_data_structure/`.

`02_compare_species_diameter_violin.R`
 Violin plots of DBH by species and dataset.
 Output in `data/processed/edge_case_analysis_species_structure/figures/`.

`03_compare_species_dbh_ecdf.R`
 ECDF comparisons of DBH by species.
 Output in `data/processed/edge_case_analysis_species_structure/`.

`10_extreme_tail_gap_analysis.R`
 Identifies DBH gaps between FIA and NEFIN at the extreme tail.
 Output in `data/processed/extreme_tail_gap_analysis/`.

`11_enhanced_ecdf_extreme_tail.R`
 Enhanced ECDF plots focused on the extreme DBH tail (above P95).

`run_edge_case_analysis.R`
 Runs all scripts in this directory in order.
