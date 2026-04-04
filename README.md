# FIA_NEFIN

This repository supports a two-paper thesis comparing FIA (Forest Inventory
and Analysis) and NEFIN (Northeast Forest Inventory Network) plot data for
forest biomass prediction in the northeastern United States. The study covers
seven states (CT, MA, ME, NH, NY, RI, VT), 7,345 FIA plots and 457 NEFIN
plots, and produces biomass predictions at two spatial scales (10 m and 250 m)
for Chittenden County, Vermont using random forest models trained under three
scenarios: FIA-only, NEFIN-only, and pooled.

## Repository contents

```text
R/                    analysis scripts organized by pipeline phase
data/                 processed data outputs (large files excluded from git)
GEE/                  Google Earth Engine scripts for covariate generation
FIA_NEFIN_explorer/   Shiny application (self-contained)
manuscript_figures/   publication-ready figures
outputs/              additional model outputs
run_scripts/          pipeline entry points
```

## Prerequisites

- R >= 4.3
- Required packages: dplyr, readr, sf, terra, ggplot2, patchwork, tidyr,
  ranger, xgboost, blockCV, tidyterra, RSQLite, DBI, viridis
- To pin package versions, run:

```r
renv::init()
renv::snapshot()
```

## Data sources

### FIA data

Download state SQLite databases from the FIA DataMart:
https://apps.fs.usda.gov/fia/datamart/datamart.html

Required states: CT, MA, ME, NH, NY, RI, VT

Place each database at:
`data/raw/fia_sqlite/{STATE}/unzipped/SQLite_FIADB_{STATE}.db`

### NEFIN data

Contact the Northeast Forest Inventory Network for plot data.
Source files go in: `data/raw/nefin/`

Expected files:
- `data/raw/nefin/NEFIN_plots.csv`
- `data/raw/nefin/TREE_PLOT_DATA.csv`
- `data/raw/nefin/TREE_RAW_DATA.csv`

### Daymet V4 climate

Raw climate rasters are included in `data/raw/daymet/`.
GEE export script: `GEE/CLIMATE_01_daymet_1km_2020_2024.js`
Resampling script: `R/05_extract_covariates/resample_daymet_climate.R`

### Covariate rasters (external drive)

Large raster files are stored outside the repository.
See "External data setup" below.

## External data setup

Covariate rasters and the AOI shapefile live on an external drive.
Before running any Phase D scripts, set `EXTERNAL_DATA_ROOT` in
`R/00_config/PHASE4_config.R` to match your local path.

Default:

```r
EXTERNAL_DATA_ROOT <- "D:/FIA_NEFIN/data"
```

Required directory structure:

```text
{EXTERNAL_DATA_ROOT}/
  aoi/
    Region.shp
  covariates/
    fine_10m/
    fine_10m_preprocessed/
    coarse_250m/
    coarse_250m_preprocessed/
```

Fine-scale covariates (10 m):
- `canopy_height_10m_2020_NE.tif` -- ETH Global Canopy Height 2020 (Lang et al.)
- `Elevation10m.tif`, `Slope10m.tif`, `Aspect10m.tif` -- DEM derivatives
- `S2_NDVI/EVI/NBR/NDWI_10m_2020_2024.tif` -- Sentinel-2 spectral indices
- `S2_B2/B3/B4_10m_2020_2024.tif` -- Sentinel-2 visible bands
- `tmean.tif`, `tmin.tif`, `tmax.tif`, `ppt.tif` -- Daymet V4 resampled to 10 m

Coarse-scale covariates (250 m):
- `canopy_height_250m_2020_NE.tif` -- ETH Global Canopy Height 2020 aggregated
- `elevation/slope/aspect_250m_NE.tif`
- `MODIS_NDVI/EVI/NBR/NDWI/RED/NIR/BLUE/GREEN/SWIR1_250m_2020_2024_NE.tif`
- `tmean.tif`, `tmin.tif`, `tmax.tif`, `ppt.tif` -- Daymet V4 aggregated to 250 m

Run `R/phase4_modeling/PHASE4_00_preprocess_rasters.R` to generate the
`_preprocessed` directories from raw GEE exports.

Files excluded from git (see `.gitignore`):
- `data/raw/fia_sqlite/**/*.db` -- FIA SQLite databases
- `data/processed/phase4_models/` -- trained model objects
- `data/predictions/phase4/` -- prediction rasters
- `*.tif`, `*.rds`, `*.tif.ovr`, `*.tif.aux.xml`

## How to reproduce

Set working directory to `FIA_NEFIN/`, then:

```r
source("run_scripts/run_complete_pipeline_updated.R")
```

Or run phases individually -- see `run_scripts/README.md`.

## Shiny app

The interactive app is in `FIA_NEFIN_explorer/`.
Before first run or deployment:

```r
setwd("FIA_NEFIN_explorer")
source("data/prep_app_data.R") # run once to build app data
shiny::runApp()
```

## Citation

[Placeholder -- add thesis citation here before submission]
