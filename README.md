# FIA-NEFIN Clean Pipeline Structure

This is the proposed clean file structure for the FIA-NEFIN comparison project.

## 🎯 Core Concept

**Current Problem**: Comparing FIA vs NEFIN as separate networks (WRONG)
**Correct Approach**: Does augmenting FIA with NEFIN's precise coordinates improve biomass estimates?

## 📊 Comparison Design

### Baseline Dataset (FIA-only)
- All FIA plots with fuzzed coordinates (~22k plots)
- Baseline for comparison

### Augmented Dataset (FIA + NEFIN)
- FIA plots (fuzzed coords) + NEFIN plots (true coords)
- Combined network (~25k plots)
- Tests if adding precise coordinates improves estimates

## 📁 Directory Structure

```
project/
├── R/                          # All R scripts
│   ├── 00_config/             # Configuration
│   ├── utils/                 # Reusable functions
│   ├── 01_process_fia/        # FIA processing (Phase A)
│   ├── 02_process_nefin/      # NEFIN processing (Phase B)
│   ├── 03_create_comparison_datasets/  # Baseline vs Augmented (Phase C)
│   ├── 04_assign_to_hexagons/ # Spatial joins (Phase D)
│   └── 05_extract_covariates/ # NDVI/PRISM extraction (Phase D)
│
├── data/
│   ├── raw/                   # Never modified
│   │   ├── fia_sqlite/
│   │   └── nefin/
│   ├── interim/               # Processing steps
│   │   ├── fia/
│   │   └── nefin/
│   └── processed/             # Final clean datasets
│       ├── fia_complete.csv
│       ├── nefin_complete.csv
│       ├── baseline.csv           # FIA-only
│       ├── augmented.csv          # FIA + NEFIN
│       ├── baseline_hex_assignments.csv
│       └── augmented_hex_assignments.csv
│
└── run_scripts/               # Convenience runners
    ├── run_phase_A.R
    ├── run_phase_B.R
    ├── run_phase_C.R
    ├── run_phase_D.R
    └── run_complete_pipeline.R
```

## Reproducing the Analysis

To reproduce the full pipeline from raw data to results:

```r
source("run_scripts/run_complete_pipeline_updated.R")
```

See `run_scripts/README.md` for a description of each phase script.
Before running, set `EXTERNAL_DATA_ROOT` in `R/00_config/PHASE4_config.R`
to point to the directory containing the covariate rasters and AOI shapefile.

---

## 🚀 Execution Flow

### Phase A: Process FIA
```r
source("run_scripts/run_phase_A.R")
# → data/processed/fia_complete.csv
```

### Phase B: Process NEFIN
```r
source("run_scripts/run_phase_B.R")
# → data/processed/nefin_complete.csv
```

### Phase C: Create Comparison Datasets
```r
source("run_scripts/run_phase_C.R")
# → baseline.csv (FIA-only)
# → augmented.csv (FIA + NEFIN)
```

### Phase D: Hex Assignment & Covariates
```r
source("run_scripts/run_phase_D.R")
# → Spatial joins at all scales
# → Covariate extraction
```

## 📋 Key Outputs

| File | Description | Use |
|------|-------------|-----|
| `fia_complete.csv` | All FIA plots (fuzzed) | Input for comparison |
| `nefin_complete.csv` | All NEFIN plots (true coords) | Input for comparison |
| `baseline.csv` | FIA-only dataset | Comparison benchmark |
| `augmented.csv` | FIA + NEFIN combined | Test dataset |

## 🔍 Research Question

**Does adding NEFIN's precise coordinates to the FIA network improve forest biomass estimates at different spatial scales?**

Not: "Is NEFIN better than FIA?" (different networks, not comparable)

## 📦 What's Included in This ZIP

- Complete R/ directory with all scripts
- Empty data/ directory structure
- Run scripts for each phase
- README files explaining each component
- Example configuration

## 🎓 Next Steps

1. Extract this structure to your project
2. Review the scripts
3. Run Phase A (or skip if you have fia_complete.csv)
4. Proceed through phases B, C, D

Questions? Check the README files in each directory!

---

## Data Sources

### Large raster data (external drive — not in repo)

All covariate rasters live on a separate drive. Set `EXTERNAL_DATA_ROOT` in
`R/00_config/PHASE4_config.R` to your local path before running the pipeline.

Required directory structure:
```
{EXTERNAL_DATA_ROOT}/
├── aoi/
│   └── Region.shp (+ .dbf, .prj, .shx, .cpg, .sbn, .sbx)
└── covariates/
    ├── fine_10m/              ← raw 10m covariates (GEE exports)
    ├── fine_10m_preprocessed/ ← aligned/clipped (output of PHASE4_00)
    ├── coarse_250m/           ← raw 250m covariates (GEE exports)
    └── coarse_250m_preprocessed/ ← aligned/clipped (output of PHASE4_00)
```

Fine scale covariates (10m):
- `canopy_height_10m_2020_NE.tif` — ETH Global Canopy Height 2020 (Lang et al.)
- `Elevation10m.tif`, `Slope10m.tif`, `Aspect10m.tif` — 10m DEM derivatives
- `S2_NDVI/EVI/NBR/NDWI_10m_2020_2024.tif` — Sentinel-2 spectral indices
- `S2_B2/B3/B4_10m_2020_2024.tif` — Sentinel-2 visible bands
- `tmean.tif`, `tmin.tif`, `tmax.tif`, `ppt.tif` — Daymet V4 resampled to 10m
- Note: S2_B8 (NIR) and S2_B11 (SWIR1) are present in raw but were excluded
  from preprocessing and are not used in any final model.

Coarse scale covariates (250m):
- `canopy_height_250m_2020_NE.tif` — ETH Global Canopy Height 2020 aggregated
- `elevation/slope/aspect_250m_NE.tif`
- `MODIS_NDVI/EVI/NBR/NDWI/RED/NIR/BLUE/GREEN/SWIR1_250m_2020_2024_NE.tif`
- `tmean.tif`, `tmin.tif`, `tmax.tif`, `ppt.tif` — Daymet V4 aggregated to 250m

GEE scripts to regenerate raw covariates are in `GEE/`. Run
`R/phase4_modeling/PHASE4_00_preprocess_rasters.R` to regenerate the
`_preprocessed` directories from the raw inputs.

### FIA data

Download state SQLite databases from FIADB:
https://apps.fs.usda.gov/fia/datamart/datamart.html

Required states: CT, MA, ME, NH, NY, RI, VT

Place each as: `data/raw/fia_sqlite/{STATE}/SQLite_FIADB_{STATE}.db`

### NEFIN data

Contact the Northeast Forest Inventory Network for plot data.
Source files expected at:
- `data/raw/nefin/NEFIN_plots.csv`
- `data/raw/nefin/TREE_PLOT_DATA.csv`
- `data/raw/nefin/TREE_RAW_DATA.csv`

### Daymet V4 climate rasters

Raw Daymet TIFs are included in `data/raw/daymet/` (tracked in repo).
GEE export script: `GEE/CLIMATE_01_daymet_1km_2020_2024.js`
Resampling script: `R/05_extract_covariates/resample_daymet_climate.R`

### Files NOT in the repository

The following large files are excluded via `.gitignore`:
- All raster files (`*.tif`) including all covariate and prediction rasters
- Model objects (`*.rds`) in `data/processed/phase4_models/`
- FIA SQLite databases in `data/raw/fia_sqlite/`
