# R/00_config

Configuration files loaded at the start of each pipeline phase.
Source the appropriate config before running any analysis scripts.

## Files

`config.R`
  General project paths and global settings.
  Defines `CONFIG$paths` for all data directories.

`PHASE4_config.R`
  Modeling hyperparameters for Phase D (Paper 2).
  Key settings:
  - ntrees: 500
  - min_node_size: 5
  - mtry: floor(p/3) -- ranger regression default.
    This is NOT sqrt(p), which is the classification default.
  - seed: 42 -- used for all stochastic components
  - EXTERNAL_DATA_ROOT: path to external covariate rasters.
    Set this to match your local drive before running Phase D.

`PHASE4_config_covariates.R`
  Production covariate list used in the final models.
  References preprocessed rasters in `fine_10m_preprocessed/`
  and `coarse_250m_preprocessed/` under EXTERNAL_DATA_ROOT.
  Climate source: Daymet V4 (tmean, tmin, tmax, ppt).

`PHASE4_config_covariates_PREPROCESSED.R`
  Variant for use when rasters are already aligned to the target grid.
  Use this instead of `PHASE4_config_covariates.R` if
  `PHASE4_00_preprocess_rasters.R` has already been run and outputs exist.
