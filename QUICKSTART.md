# Quick Start Guide

## 🎯 What This Pipeline Does

Creates **baseline** (FIA-only) vs **augmented** (FIA + NEFIN) datasets to answer:

> **Does augmenting FIA with NEFIN's precise coordinates improve forest biomass estimates?**

## 📁 File Structure

```
R/
├── 00_config/          # Configuration
├── utils/              # Reusable functions
├── 01_process_fia/     # Phase A: FIA processing
├── 02_process_nefin/   # Phase B: NEFIN processing
├── 03_create_comparison_datasets/  # Phase C: Baseline vs Augmented
├── 04_assign_to_hexagons/          # Phase D: Spatial joins
└── 05_extract_covariates/          # Phase D: Covariates

run_scripts/            # Convenience runners
data/                   # Your existing data structure
```

## 🚀 How to Run

### Option 1: You Already Have Processed Data

If `data/processed/fia_complete.csv` and `data/processed/nefin_complete.csv` exist:

```r
# Skip to Phase C
source("run_scripts/run_phase_C.R")  # Creates baseline.csv & augmented.csv
source("run_scripts/run_phase_D.R")  # Hex assignment & covariates
```

### Option 2: Start from Scratch

```r
# Run complete pipeline
source("run_scripts/run_complete_pipeline.R")
```

Or step by step:

```r
source("run_scripts/run_phase_A.R")  # FIA processing
source("run_scripts/run_phase_B.R")  # NEFIN processing
source("run_scripts/run_phase_C.R")  # Comparison datasets
source("run_scripts/run_phase_D.R")  # Hex & covariates
```

## 📊 Key Outputs

| File | What It Is | Use |
|------|------------|-----|
| `fia_complete.csv` | All FIA plots (fuzzed coords) | Input |
| `nefin_complete.csv` | All NEFIN plots (true coords) | Input |
| `baseline.csv` | FIA-only dataset | Benchmark |
| `augmented.csv` | FIA + NEFIN combined | Test dataset |

## 🔍 The Comparison

### Baseline (FIA-only)
- ~22,000 plots
- All coordinates fuzzed (±1.6km)
- Standard FIA network

### Augmented (FIA + NEFIN)
- ~25,000 plots
- FIA plots (fuzzed) + NEFIN plots (true coords)
- Enhanced network with precise coordinates

### Analysis
Compare biomass estimates between baseline and augmented at multiple scales (100ha → 100kha).

## ⚙️ Configuration

Edit `R/00_config/config.R` to change:
- States
- Year range
- Hex scales
- MC parameters

## 📖 Phase Details

### Phase A: Process FIA
SQLite → interim CSVs → plot biomass → standardized dataset

### Phase B: Process NEFIN
Raw CSV → plot biomass → standardized dataset (matching FIA schema)

### Phase C: Create Comparison Datasets
**Most Important!**
- Validates inputs
- Creates baseline (FIA-only)
- Creates augmented (FIA + NEFIN)
- Handles overlapping plots correctly

### Phase D: Spatial Processing
- Assigns to hexagons (all scales)
- Extracts environmental covariates

## 🐛 Troubleshooting

**Missing packages?**
```r
install.packages(c("dplyr", "readr", "sf", "DBI", "RSQLite", "ggplot2"))
```

**Data paths wrong?**
Check `R/00_config/config.R` paths match your structure.

**Phase B needs customization?**
NEFIN data structure varies - update Phase B scripts to match your files.

## 📚 Documentation

- Each phase has a README: `R/XX_*/README.md`
- Each script has inline documentation
- Validation reports generated automatically

## ✅ Next Steps After Running

1. Review validation reports in `data/processed/`
2. Check baseline.csv and augmented.csv
3. Run your analysis comparing the two datasets
4. Quantify improvement from precise coordinates

---

**Questions?** Check the phase READMEs or script comments!
