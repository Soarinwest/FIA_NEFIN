# run_scripts/

Convenience runners for the FIA-NEFIN analysis pipeline.

## To reproduce the full analysis

```r
source("run_scripts/run_complete_pipeline_updated.R")
```

This is the canonical entry point. It runs all five phases (A–E) in order
and produces all data outputs and manuscript figures.

## Phase scripts

| Script | Phase | What it does |
|--------|-------|--------------|
| `run_phase_A.R` | A | Process FIA SQLite databases → `data/processed/fia_complete.csv` |
| `run_phase_B.R` | B | Process NEFIN source data → `data/processed/nefin_complete.csv` |
| `run_phase_C.R` | C | Create baseline and augmented comparison datasets |
| `run_phase_D.R` | D | Hexagon assignment and covariate extraction |
| `run_analysis.R` | E | All analysis scripts and figure generation |

## Pipeline files

| Script | Description |
|--------|-------------|
| `run_complete_pipeline_updated.R` | **Canonical entry point.** Runs phases A–E. Use this. |
| `run_complete_pipeline.R` | Earlier version — phases A–D only, no analysis phase. Superseded. |

## Phase 4 modeling (optional)

Spatial biomass prediction models are run separately after the main pipeline:

```r
source("R/phase4_modeling/RUN_PHASE4_COMPLETE_UPDATED.R")
```

Requires external drive rasters. Set `EXTERNAL_DATA_ROOT` in
`R/00_config/PHASE4_config.R` first.
