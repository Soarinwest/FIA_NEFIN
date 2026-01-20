# Phase B: NEFIN Processing

Processes NEFIN raw data to match FIA schema.

## Scripts

1. **01_load_nefin.R** - Load and clean raw NEFIN data
2. **02_compute_biomass.R** - Compute plot-level biomass
3. **03_create_nefin_dataset.R** - Standardize to FIA schema

## Output

`data/processed/nefin_complete.csv` with identical schema to FIA:
- dataset = "NEFIN"
- coord_source = "true" (precise coordinates!)
