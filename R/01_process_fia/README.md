# Phase A: FIA Processing

Processes FIA SQLite databases into clean, standardized plot-level biomass dataset.

## Scripts

1. **01_extract_from_sqlite.R**
   - Reads state SQLite databases
   - Filters to study years (2020-2024)
   - Extracts PLOT, TREE, COND tables
   - Combines across states

2. **02_compute_biomass.R**
   - Filters to live trees
   - Applies TPA expansion factors
   - Aggregates tree → plot level
   - Converts pounds/acre → Mg/ha

3. **03_create_fia_dataset.R**
   - Standardizes column names
   - Adds comparison metadata
   - Validates coordinates & biomass

## Output

`data/processed/fia_complete.csv` with schema:
- CN, STATECD, COUNTYCD, PLOT, MEASYEAR
- lat, lon (fuzzed coordinates)
- lat_for_extraction, lon_for_extraction
- biomass (Mg/ha)
- dataset = "FIA"
- coord_source = "fuzzed"
