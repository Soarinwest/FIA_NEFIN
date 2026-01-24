# 🛰️ Google Earth Engine Processing Scripts
## NEFIN vs FIA Biomass Modeling - Covariate Extraction

This repository contains Google Earth Engine scripts to process remote sensing and topographic layers for comparing NEFIN (precise coordinates) vs FIA (fuzzed coordinates) forest biomass models.

---

## 📋 **OVERVIEW**

**Study Design**: Test whether coordinate fuzzing affects biomass prediction accuracy at two spatial scales:
- **Fine scale** (10m Sentinel-2): Where fuzzing should matter
- **Coarse scale** (250m MODIS): Where fuzzing might not matter

**Time Period**: 2020–2024 (5-year mean composites)

**Study Region**: Northeastern US (ME, NH, VT, MA, CT, RI, NY)

---

## 📁 **SCRIPT INVENTORY**

### 🌍 **Sentinel-2 Scripts (10m resolution)**

1. **`S2_01_bands_mean_2020_2024.js`**
   - **Purpose**: Extract individual spectral bands
   - **Bands**: B2 (Blue), B3 (Green), B4 (Red), B8 (NIR), B11 (SWIR1)
   - **Temporal Agg**: 5-year MEAN (2020-2024)
   - **Output**: 5 files @ 10m

2. **`S2_02_indices_mean_2020_2024.js`**
   - **Purpose**: Calculate spectral indices
   - **Indices**: EVI, NBR, NDWI
   - **Temporal Agg**: 5-year MEAN
   - **Output**: 3 files @ 10m
   - **Note**: NDVI already processed separately

3. **`S2_03_NDVI_stdev_2020_2024.js`**
   - **Purpose**: NDVI temporal variability
   - **Metric**: Standard deviation (phenology/disturbance indicator)
   - **Temporal Agg**: 5-year STDEV
   - **Output**: 1 file @ 10m

### 🛰️ **MODIS Scripts (250m resolution)**

4. **`MODIS_01_bands_mean_2020_2024.js`**
   - **Purpose**: Extract individual spectral bands
   - **Products**: MOD09Q1 (250m) + MOD09A1 (500m→250m)
   - **Bands**: B1 (Red), B2 (NIR), B3 (Blue), B4 (Green), B6 (SWIR1)
   - **Temporal Agg**: 5-year MEAN
   - **Output**: 5 files @ 250m

5. **`MODIS_02_indices_mean_2020_2024.js`**
   - **Purpose**: Calculate spectral indices
   - **Products**: MOD13Q1 (EVI) + MOD09Q1/A1 (NBR, NDWI)
   - **Indices**: EVI, NBR, NDWI
   - **Temporal Agg**: 5-year MEAN
   - **Output**: 3 files @ 250m
   - **Note**: NDVI already processed separately

### 🏔️ **DEM Scripts (10m + 250m)**

6. **`DEM_01_derivatives_10m.js`**
   - **Purpose**: Topographic derivatives from USGS NED
   - **Layers**: Elevation, Slope, Aspect, TPI
   - **Resolution**: Native 10m + aggregated 250m
   - **Output**: 4 files @ 10m + 3 files @ 250m
   - **Special**: Circular mean for aspect aggregation

### 🌡️ **Land Surface Temperature (Optional)**

7. **`LST_01_day_night_mean_2020_2024.js`**
   - **Purpose**: Microclimate variation (complements PRISM)
   - **Product**: MOD11A2 (8-day composite)
   - **Layers**: LST Day, LST Night, Diurnal Range
   - **Temporal Agg**: 5-year MEAN
   - **Output**: 3 files @ 1km

---

## 🎯 **MODELING STRATEGY**

### **Model Set 1: FINE SCALE (10m)**
**Covariates** (~13 total):
- Spectral (Sentinel-2): NDVI, EVI, NBR, NDWI, B4, B8, B11
- Topography: elevation, slope, aspect
- Climate: temperature, precipitation (already have)
- Optional: NDVI_SD (variability)

**Hypothesis**: NEFIN >>> FIA (fuzzing hurts at fine resolution!)

### **Model Set 2: COARSE SCALE (250m)**
**Covariates** (~12 total):
- Spectral (MODIS): NDVI, EVI, NBR, NDWI, Red, NIR, SWIR1
- Topography: elevation_250m, slope_250m, aspect_250m
- Climate: temperature, precipitation

**Hypothesis**: NEFIN ≈ FIA (fuzzing doesn't matter at coarse resolution!)

---

## 🚀 **HOW TO RUN**

### **Step 1: Set Up GEE**
1. Go to: https://code.earthengine.google.com/
2. Sign in with Google account
3. Copy/paste each script into the Code Editor

### **Step 2: Run Scripts**
For each script:
1. **Paste code** into editor
2. **Click "Run"** button
3. **Check Tasks** tab (top-right)
4. **Click "RUN"** on each export task
5. **Select Google Drive** folder destination

### **Step 3: Organize Outputs**
All files export to your Google Drive folder: `NEFIN_FIA_Covariates/`

Expected file structure:
```
NEFIN_FIA_Covariates/
├── S2_B2_10m_2020_2024_NE.tif
├── S2_B3_10m_2020_2024_NE.tif
├── S2_B4_10m_2020_2024_NE.tif
├── S2_B8_10m_2020_2024_NE.tif
├── S2_B11_10m_2020_2024_NE.tif
├── S2_EVI_10m_2020_2024_NE.tif
├── S2_NBR_10m_2020_2024_NE.tif
├── S2_NDWI_10m_2020_2024_NE.tif
├── S2_NDVI_SD_10m_2020_2024_NE.tif
├── MODIS_RED_250m_2020_2024_NE.tif
├── MODIS_NIR_250m_2020_2024_NE.tif
├── MODIS_BLUE_250m_2020_2024_NE.tif
├── MODIS_GREEN_250m_2020_2024_NE.tif
├── MODIS_SWIR1_250m_2020_2024_NE.tif
├── MODIS_EVI_250m_2020_2024_NE.tif
├── MODIS_NBR_250m_2020_2024_NE.tif
├── MODIS_NDWI_250m_2020_2024_NE.tif
├── elevation_10m_NE.tif
├── slope_10m_NE.tif
├── aspect_10m_NE.tif
├── tpi_10m_NE.tif
├── elevation_250m_NE.tif
├── slope_250m_NE.tif
├── aspect_250m_NE.tif
└── (LST files if processed)
```

---

## ⏱️ **PROCESSING TIME**

**Sentinel-2 scripts**: ~30-60 minutes each (high resolution)
**MODIS scripts**: ~15-30 minutes each
**DEM script**: ~10-20 minutes
**LST script**: ~15-30 minutes

**Total**: ~2-4 hours for all exports

---

## 📊 **DATA SPECIFICATIONS**

| Layer Type | Resolution | Product | Temporal |
|------------|-----------|---------|----------|
| S2 Bands | 10m | COPERNICUS/S2_SR_HARMONIZED | Mean 2020-2024 |
| S2 Indices | 10m | Calculated from S2 | Mean 2020-2024 |
| S2 NDVI_SD | 10m | Calculated from S2 | StdDev 2020-2024 |
| MODIS Bands | 250m | MOD09Q1/A1 | Mean 2020-2024 |
| MODIS Indices | 250m | MOD13Q1 + Calculated | Mean 2020-2024 |
| DEM | 10m → 250m | USGS/NED | Static |
| LST | 1km | MOD11A2 | Mean 2020-2024 |

---

## 🔬 **NEXT STEPS (R Processing)**

Once GEE exports complete:

1. **Download** all files from Google Drive
2. **Organize** into local directory structure:
   ```
   data/raw/
   ├── dem/
   ├── ndvi/s2/
   ├── ndvi/modis/
   ├── prism/
   └── lst/
   ```
3. **Run extraction scripts** (to be created):
   - `PHASE4_02_extract_covariates_10m.R`
   - `PHASE4_03_extract_covariates_250m.R`
4. **Output**:
   - `baseline_with_covariates_10m.csv`
   - `augmented_with_covariates_10m.csv`
   - `baseline_with_covariates_250m.csv`
   - `augmented_with_covariates_250m.csv`

---

## 🛠️ **TROUBLESHOOTING**

### **Script won't run**
- Check if you're signed into GEE
- Verify region geometry loads (red polygon should appear)
- Check console for error messages

### **Export fails**
- Image might be too large → reduce region or increase `maxPixels`
- Check Google Drive storage space
- Try exporting to Cloud Storage instead of Drive

### **Quality concerns**
- All scripts include cloud/QA masking
- Growing season filter: May-September
- Check layer visualizations before exporting

### **Missing data**
- S2: Some areas might have cloud gaps (5-year composite should fill most)
- MODIS: Better temporal coverage, coarser resolution
- DEM: Complete coverage (static)

---

## 📝 **NOTES**

### **CRS / Projection**
- All exports use **EPSG:4326 (WGS84)** to match your existing PRISM/FIA pipeline
- Processing done in native projections (EPSG:5070 for S2, sinusoidal for MODIS)
- Reprojection happens during export

### **Cloud Masking**
- **S2**: Uses Scene Classification Layer (SCL) - keeps vegetation, bare soil, water
- **MODIS**: Uses QA flags - keeps only good quality pixels

### **Temporal Filtering**
- All scripts filter to **May-September** (growing season)
- This matches typical forest biomass acquisition period
- Reduces phenology variation for evergreen/deciduous mix

### **B11 Resampling**
- SWIR1 (B11) native at 20m, resampled to 10m in processing
- Acceptable for forest applications (forests vary at >10m scale anyway)

---

## ✅ **PRIORITY CHECKLIST**

### **Must Have** (for strong models):
- [x] DEM derivatives (elevation, slope, aspect)
- [x] S2: NDVI (already done), EVI, NBR, NDWI
- [x] MODIS: NDVI (already done), EVI
- [x] Climate: temp, precip (already have)

### **Nice to Have** (for comprehensive models):
- [x] S2 individual bands (B4, B8, B11)
- [x] MODIS individual bands (Red, NIR, SWIR1)
- [x] S2 NDVI_SD (variability)

### **Optional** (if time):
- [x] TPI (topographic position)
- [x] LST (day/night temperature)
- [ ] More MODIS bands (B3-Blue, B4-Green at 500m)

---

## 🎓 **MANUSCRIPT IMPLICATIONS**

This processing enables the key comparison:

> **"At fine resolution (10m Sentinel-2), precise NEFIN coordinates yield significantly better biomass predictions than fuzzed FIA coordinates. However, at coarse resolution (250m MODIS), the difference disappears, suggesting fuzzing effects are scale-dependent."**

**Gold statement for discussion**:
> "Use NEFIN for fine-scale mapping; FIA is acceptable for regional modeling."

---

## 📧 **SUPPORT**

For GEE questions:
- GEE Forum: https://groups.google.com/g/google-earth-engine-developers
- GEE Docs: https://developers.google.com/earth-engine

For script questions:
- Check console messages
- Verify data availability in your region
- Test with smaller region first (single state)

---

**Last Updated**: January 2026  
**Author**: Soren Duvivier  
**Project**: NEFIN vs FIA Coordinate Precision Study
