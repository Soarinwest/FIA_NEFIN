# R/06_analysis

Paper 1 analysis scripts: hexagon-scale comparison, Monte Carlo
coordinate uncertainty, large tree analysis, and summary statistics.

## Script reference

| Script | Purpose |
| --- | --- |
| 00_extract_fia_tree_data.R | Extract tree-level data for DBH analysis |
| 01a_filter_empty_hexagons.R | Remove hexagons with no plot data |
| 01b_aggregate_to_hexagons.R | Assign plots to hexagonal grid cells |
| 02_compare_datasets.R | Compute FIA vs NEFIN comparison metrics |
| 03_monte_carlo_generate_jitter.R | Generate 100 coordinate jitter replicates |
| 04_monte_carlo_extract_covariates.R | Extract covariates at jittered locations |
| 05_monte_carlo_analyze_uncertainty.R | Summarize Monte Carlo covariate uncertainty |
| 06_combined_analysis.R | Combined statistical analysis |
| 07_summary_statistics.R | Bootstrap confidence intervals for comparison metrics |
| 08_within_hex_variance.R | Within-hexagon biomass variance analysis |
| 09_large_tree_analysis.R | Species-level large tree tail enrichment |
| 10_hexagon_scale_impact.R | How comparison metrics change with hexagon scale |
| 11a_max_tree_per_plot.R | Maximum tree size per plot |
| 11b_spatial_decision_framework.R | Scale selection framework |
| 12_paper1_analysis.R | Generates all Paper 1 tables and figures |
| 13_summarize_and_view.R | Summary view of processed outputs |

Scripts 01a and 01b are both numbered 01 because filtering must precede
aggregation. Run 01a before 01b.

## Monte Carlo analysis

The Monte Carlo analysis (scripts 03-05) simulates 100 random displacements
of each FIA plot coordinate within the approximate fuzzing radius of 1.6 km.
Covariate values are extracted at each jittered location.
Results are in `data/processed/monte_carlo/`.
Output uncertainty metrics are in
`data/processed/monte_carlo/plot_uncertainty.csv`.
