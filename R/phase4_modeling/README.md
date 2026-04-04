# R/phase4_modeling

Phase D modeling scripts for Paper 2: random forest and XGBoost
biomass prediction at two spatial scales.

## Execution order

Run scripts in the following order from the project root.
All scripts source `R/00_config/PHASE4_config.R` at startup.
Set `EXTERNAL_DATA_ROOT` in that file before running.

1. `PHASE4_00_preprocess_rasters.R`
   Reads raw covariate rasters from EXTERNAL_DATA_ROOT.
   Aligns, clips, and reprojects to EPSG:5070.
   Writes to `fine_10m_preprocessed/` and `coarse_250m_preprocessed/`.
   Run once. Skip if preprocessed directories already exist.

2. `PHASE4_01_prep_data.R`
   Builds training and test datasets from FIA and NEFIN plot data
   and extracted covariate values.
   Produces `train_fia_only.csv`, `train_nefin_only.csv`, `train_pooled.csv`,
   and `test_data.csv` in `data/processed/phase4_modeling/`.

3. `PHASE4_02_spatial_cv.R`
   Runs 10-fold spatial block cross-validation for all six models
   (3 training scenarios x 2 spatial scales: 10 m and 250 m).
   Spatial folds use 25 km x 25 km blocks with a 10 km buffer.
   Seed: 42. Results in `data/processed/phase4_cv_results/`.

4. `PHASE4_03_predict_biomass.R`
   Applies trained models to covariate rasters for Chittenden County, VT.
   Outputs 10 m and 250 m prediction rasters to `data/predictions/phase4/`.
   Do not modify output rasters once generated.

5. `PHASE4_04_spatial_plots.R`
   Spatial residual maps and diagnostic spatial plots.

## Supporting scripts

`PHASE4_diagnostics.R`
  Variable importance and residual analysis.
  ETH Global Canopy Height 2020 (Lang et al.) is the top predictor
  at 100% variable importance in all six models.

`PHASE4_extract_covariates.R`
  Extracts covariate values at plot locations from preprocessed rasters.

`PHASE4_fuzzing_impact_analysis.R`
  Quantifies the effect of FIA coordinate fuzzing on model performance.
  Results in `data/processed/phase4_cv_results/fuzzing_*.csv`.

`paper2_analysis.R`
  Generates all Paper 2 tables and figures from CV results.
  Run after `PHASE4_02_spatial_cv.R` completes.

## Data modified by this directory

- `data/processed/phase4_modeling/` -- training and test CSVs
- `data/processed/phase4_cv_results/` -- CV outputs (do not modify)
- `data/processed/phase4_models/` -- trained model objects (do not modify)
- `data/predictions/phase4/` -- prediction rasters (do not modify)
