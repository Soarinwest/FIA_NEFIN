# run_scripts

Pipeline entry points for the full FIA_NEFIN analysis.

## Canonical entry point

To reproduce the complete analysis:

```r
setwd("path/to/FIA_NEFIN")
source("run_scripts/run_complete_pipeline_updated.R")
```

## Script reference

| Script | Purpose |
| --- | --- |
| run_complete_pipeline_updated.R | Full pipeline, all phases including analysis |
| run_complete_pipeline.R | Earlier version, excludes run_analysis.R phase |
| run_analysis.R | Standalone analysis phase runner |
| run_phase_A.R | Data processing: FIA and NEFIN extraction and biomass |
| run_phase_B.R | Hexagon aggregation and covariate extraction |
| run_phase_C.R | Paper 1 compositional and scale analysis |
| run_phase_D.R | Paper 2 modeling: spatial CV, prediction, diagnostics |

`run_complete_pipeline_updated.R` is the superset of all phase scripts.
Use the individual phase scripts only if re-running a single stage.
