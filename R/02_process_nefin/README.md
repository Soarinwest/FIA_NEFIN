# R/02_process_nefin

Processes NEFIN raw data to match the FIA output schema.

## Scripts

1. `01_load_nefin.R` -- load and clean raw NEFIN data
2. `02_compute_biomass.R` -- compute plot-level biomass
3. `03_create_nefin_dataset.R` -- standardize to FIA schema

## Output

`data/processed/nefin_complete.csv` with schema identical to
`data/processed/fia_complete.csv`:
- dataset = "NEFIN"
- coord_source = "true"
