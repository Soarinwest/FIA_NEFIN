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
