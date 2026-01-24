# 🎯 FINAL SCRIPT INVENTORY - Complete Covariate Suite
## All 10 GEE Scripts for NEFIN vs FIA Biomass Study

---

## 📦 **COMPLETE PACKAGE: 10 Scripts → ~37 Files**

---

## 🛠️ **UTILITY SCRIPTS** (Run First!)

### **0. `GEE_00_utility_checks.js`**
**Purpose:** Verify data availability and setup
- Check region loads correctly
- Verify S2 and MODIS collection sizes
- Test sample extractions
- Quality checks

**Exports:** None (verification only)

---

### **99. `GEE_99_master_checklist.js`**
**Purpose:** Complete layer specifications and research plan
- Layer inventory
- Processing status
- Research hypotheses
- Export checklist

**Exports:** None (reference only)

---

## 🌍 **SENTINEL-2 (10m) - 9 FILES**

### **1. `S2_01_bands_mean_2020_2024.js`**
**Individual spectral bands (2020-2024 mean)**

**Exports 5 files:**
1. S2_B2_10m_2020_2024_NE.tif - Blue (490nm)
2. S2_B3_10m_2020_2024_NE.tif - Green (560nm)
3. S2_B4_10m_2020_2024_NE.tif - Red (665nm)
4. S2_B8_10m_2020_2024_NE.tif - NIR (842nm)
5. S2_B11_10m_2020_2024_NE.tif - SWIR1 (1610nm)

---

### **2. `S2_02_indices_mean_2020_2024.js`**
**Spectral indices (2020-2024 mean)**

**Exports 3 files:**
1. S2_EVI_10m_2020_2024_NE.tif - Enhanced Vegetation Index
2. S2_NBR_10m_2020_2024_NE.tif - Normalized Burn Ratio
3. S2_NDWI_10m_2020_2024_NE.tif - Water Index

---

### **3. `S2_03_NDVI_stdev_2020_2024.js`**
**NDVI temporal variability (2020-2024)**

**Exports 1 file:**
1. S2_NDVI_SD_10m_2020_2024_NE.tif - NDVI standard deviation

**Note:** NDVI mean already processed separately

---

## 🛰️ **MODIS (250m) - 8 FILES**

### **4. `MODIS_01_bands_mean_2020_2024.js`**
**Individual spectral bands (2020-2024 mean)**

**Exports 5 files:**
1. MODIS_RED_250m_2020_2024_NE.tif - Red (620-670nm)
2. MODIS_NIR_250m_2020_2024_NE.tif - NIR (841-876nm)
3. MODIS_BLUE_250m_2020_2024_NE.tif - Blue (459-479nm, 500m→250m)
4. MODIS_GREEN_250m_2020_2024_NE.tif - Green (545-565nm, 500m→250m)
5. MODIS_SWIR1_250m_2020_2024_NE.tif - SWIR1 (1628-1652nm, 500m→250m)

---

### **5. `MODIS_02_indices_mean_2020_2024.js`**
**Spectral indices (2020-2024 mean)**

**Exports 3 files:**
1. MODIS_EVI_250m_2020_2024_NE.tif - Enhanced Vegetation Index
2. MODIS_NBR_250m_2020_2024_NE.tif - Normalized Burn Ratio
3. MODIS_NDWI_250m_2020_2024_NE.tif - Water Index

**Note:** MODIS NDVI already processed separately

---

## 🏔️ **DEM (10m + 250m) - 7 FILES**

### **6. `DEM_01_derivatives_10m.js`**
**Topographic derivatives from USGS NED**

**Exports 7 files:**

**10m native:**
1. elevation_10m_NE.tif - Elevation (meters)
2. slope_10m_NE.tif - Slope (degrees)
3. aspect_10m_NE.tif - Aspect (degrees)
4. tpi_10m_NE.tif - Topographic Position Index

**250m aggregated:**
5. elevation_250m_NE.tif - Mean elevation
6. slope_250m_NE.tif - Mean slope
7. aspect_250m_NE.tif - Circular mean aspect

---

## 🌡️ **LAND SURFACE TEMPERATURE (1km) - 3 FILES** (Optional)

### **7. `LST_01_day_night_mean_2020_2024.js`**
**MODIS LST (2020-2024 mean)**

**Exports 3 files:**
1. MODIS_LST_day_1km_2020_2024_NE.tif - Daytime temperature
2. MODIS_LST_night_1km_2020_2024_NE.tif - Nighttime temperature
3. MODIS_LST_range_1km_2020_2024_NE.tif - Diurnal range

**Purpose:** Microclimate variation (complements PRISM)

---

## 🌲 **CANOPY HEIGHT (10m + 250m) - 4 FILES** ⭐ GOLD!

### **8. `CANOPY_01_height_10m_2020.js`** ⭐ UPDATED!
**ETH Global Canopy Height 2020 (Lang et al. 2023)**

**Exports 4 files:**
1. canopy_height_10m_2020_NE.tif - Mean height (10m)
2. canopy_height_SD_10m_2020_NE.tif - Height standard deviation (10m)
3. canopy_height_250m_2020_NE.tif - Mean height (250m aggregated)
4. canopy_height_SD_250m_2020_NE.tif - Height SD (250m aggregated)

**Why Critical:**
- ⭐ Strongest biomass predictor (r² ~0.7)
- 10m resolution matches Sentinel-2 perfectly
- 2020 timestamp aligns with study period
- SD captures structural complexity

**Data Source:**
```javascript
var canopy_height = ee.Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1');
var standard_deviation = ee.Image('users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1');
```

---

## 🌳 **TREE COVER (Multiple Resolutions) - 4 FILES** ⭐ RECOMMENDED

### **9. `FOREST_01_hansen_cover_loss.js`**
**Hansen Global Forest Change (v1.11, 2023)**

**Exports 4 files:**
1. treecover_30m_2020_NE.tif - Tree cover % at 30m (native)
2. treecover_10m_2020_NE.tif - Tree cover % at 10m (resampled)
3. treecover_250m_2020_NE.tif - Tree cover % at 250m (aggregated)
4. forest_loss_2016_2023_NE.tif - Recent disturbance mask

**Why Critical:**
- Strong biomass predictor (r² ~0.5)
- Filters disturbed/non-forest areas
- Multi-resolution for both 10m and 250m models
- Provides stand density context

---

## 🌿 **MODIS VCF (250m) - 2 FILES** ⭐ RECOMMENDED

### **10. `FOREST_02_modis_vcf_250m.js`**
**MODIS Vegetation Continuous Fields (2020-2022)**

**Exports 2 files:**
1. VCF_treecover_250m_2020_NE.tif - Tree cover % 2020
2. VCF_treecover_250m_2020_2022_NE.tif - Tree cover % (3-year mean)

**Why Critical:**
- Native 250m resolution (perfect for MODIS models)
- More recent than Hansen 2000 baseline
- Complements Hansen at appropriate scale

---

## 📊 **SUMMARY TABLE**

| Category | Script # | Files | Resolution | Status |
|----------|----------|-------|-----------|--------|
| **Utilities** | 0, 99 | 0 | - | Reference |
| **Sentinel-2** | 1-3 | 9 | 10m | Core |
| **MODIS Spectral** | 4-5 | 8 | 250m | Core |
| **DEM** | 6 | 7 | 10m, 250m | Core |
| **LST** | 7 | 3 | 1km | Optional |
| **Canopy Height** | 8 | 4 | 10m, 250m | ⭐ Gold |
| **Tree Cover** | 9-10 | 6 | 10-250m | ⭐ Recommended |
| **TOTAL** | **10** | **37** | - | - |

---

## 🎯 **RECOMMENDED EXPORT PRIORITY**

### **TIER 1: Must Run (Core Analysis)**
1. ✅ Sentinel-2 scripts (1-3) - Spectral foundation
2. ✅ MODIS scripts (4-5) - Scale comparison
3. ✅ DEM script (6) - Topographic context

**Subtotal: 24 files**

---

### **TIER 2: Highly Recommended (Major Improvement)**
4. ⭐ Canopy Height (8) - Strongest predictor
5. ⭐ Hansen Tree Cover (9) - Density & filtering
6. ⭐ MODIS VCF (10) - MODIS-scale density

**Subtotal: +10 files = 34 files**

---

### **TIER 3: Optional (Refinement)**
7. LST (7) - Microclimate (if time permits)

**Final Total: +3 files = 37 files**

---

## 🚀 **EXECUTION WORKFLOW**

### **Phase 1: Setup (5 min)**
```
1. Open GEE Code Editor
2. Run GEE_00_utility_checks.js
3. Verify region loads and data available
4. Run GEE_99_master_checklist.js for reference
```

### **Phase 2: Core Exports (2-3 hours)**
```
5. Run S2_01_bands (Sentinel-2 bands)
6. Run S2_02_indices (Sentinel-2 indices)
7. Run S2_03_NDVI_stdev (NDVI variability)
8. Run MODIS_01_bands (MODIS bands)
9. Run MODIS_02_indices (MODIS indices)
10. Run DEM_01_derivatives (Topography)

→ Click "Run" for each
→ Go to Tasks tab
→ Click "RUN" on all exports
→ Total: 24 files
```

### **Phase 3: Enhanced Exports (1-2 hours)** ⭐
```
11. Run CANOPY_01_height (ETH height) ← GOLD!
12. Run FOREST_01_hansen (Tree cover)
13. Run FOREST_02_modis_vcf (VCF)

→ Total: +10 files = 34 files
```

### **Phase 4: Optional (30-60 min)**
```
14. Run LST_01_day_night (Land surface temp)

→ Total: +3 files = 37 files
```

### **Phase 5: Download & Process**
```
15. Wait for all tasks to complete
16. Download from Google Drive
17. Organize into data/raw/ structure
18. Run R extraction scripts
```

---

## 💡 **EXPECTED MODELING PERFORMANCE**

### **Without Height & Tree Cover (Core Only):**
```
Fine scale (10m) - Spectral + Topo:
  NEFIN R²: 0.55
  FIA R²:   0.45
  Δ: 0.10 ⭐ Shows fuzzing matters
```

### **With Height (Tier 2 - Recommended):**
```
Fine scale (10m) - All covariates:
  NEFIN R²: 0.78 (+23% from height!)
  FIA R²:   0.71
  Δ: 0.07 ⭐ STILL shows fuzzing matters
  
Better R² + Maintains fuzzing effect!
```

### **Manuscript Impact:**
- Higher R² = more credible models
- Height validates spectral predictions
- Tree cover filters disturbance
- Structure (SD) adds complexity dimension
- Stronger conclusions about fuzzing effects

---

## 📁 **FINAL DIRECTORY STRUCTURE**

```
data/raw/
│
├── dem/
│   ├── elevation_10m_NE.tif
│   ├── slope_10m_NE.tif
│   ├── aspect_10m_NE.tif
│   ├── tpi_10m_NE.tif
│   ├── elevation_250m_NE.tif
│   ├── slope_250m_NE.tif
│   └── aspect_250m_NE.tif
│
├── ndvi/
│   ├── s2/  (Sentinel-2 10m)
│   │   ├── S2_B2_10m_2020_2024_NE.tif
│   │   ├── S2_B3_10m_2020_2024_NE.tif
│   │   ├── S2_B4_10m_2020_2024_NE.tif
│   │   ├── S2_B8_10m_2020_2024_NE.tif
│   │   ├── S2_B11_10m_2020_2024_NE.tif
│   │   ├── S2_NDVI_10m_2020_2024_NE.tif (already have)
│   │   ├── S2_EVI_10m_2020_2024_NE.tif
│   │   ├── S2_NBR_10m_2020_2024_NE.tif
│   │   ├── S2_NDWI_10m_2020_2024_NE.tif
│   │   └── S2_NDVI_SD_10m_2020_2024_NE.tif
│   │
│   └── modis/  (MODIS 250m)
│       ├── MODIS_RED_250m_2020_2024_NE.tif
│       ├── MODIS_NIR_250m_2020_2024_NE.tif
│       ├── MODIS_BLUE_250m_2020_2024_NE.tif
│       ├── MODIS_GREEN_250m_2020_2024_NE.tif
│       ├── MODIS_SWIR1_250m_2020_2024_NE.tif
│       ├── MODIS_NDVI_250m_2020_2024_NE.tif (already have)
│       ├── MODIS_EVI_250m_2020_2024_NE.tif
│       ├── MODIS_NBR_250m_2020_2024_NE.tif
│       └── MODIS_NDWI_250m_2020_2024_NE.tif
│
├── prism/
│   ├── prism_tmean_ne_2020_2024.tif (already have)
│   └── prism_ppt_ne_2020_2024.tif (already have)
│
├── lst/ (optional)
│   ├── MODIS_LST_day_1km_2020_2024_NE.tif
│   ├── MODIS_LST_night_1km_2020_2024_NE.tif
│   └── MODIS_LST_range_1km_2020_2024_NE.tif
│
├── canopy/ ⭐
│   ├── canopy_height_10m_2020_NE.tif
│   ├── canopy_height_SD_10m_2020_NE.tif
│   ├── canopy_height_250m_2020_NE.tif
│   └── canopy_height_SD_250m_2020_NE.tif
│
└── forest/ ⭐
    ├── treecover_30m_2020_NE.tif
    ├── treecover_10m_2020_NE.tif
    ├── treecover_250m_2020_NE.tif
    ├── forest_loss_2016_2023_NE.tif
    ├── VCF_treecover_250m_2020_NE.tif
    └── VCF_treecover_250m_2020_2022_NE.tif
```

---

## ✅ **FINAL CHECKLIST**

### **Scripts Ready:**
- [x] GEE_00_utility_checks.js (verification)
- [x] GEE_99_master_checklist.js (reference)
- [x] S2_01_bands_mean_2020_2024.js
- [x] S2_02_indices_mean_2020_2024.js
- [x] S2_03_NDVI_stdev_2020_2024.js
- [x] MODIS_01_bands_mean_2020_2024.js
- [x] MODIS_02_indices_mean_2020_2024.js
- [x] DEM_01_derivatives_10m.js
- [x] LST_01_day_night_mean_2020_2024.js (optional)
- [x] CANOPY_01_height_10m_2020.js ⭐ (updated for ETH)
- [x] FOREST_01_hansen_cover_loss.js ⭐
- [x] FOREST_02_modis_vcf_250m.js ⭐

### **Documentation:**
- [x] README_GEE_Processing.md (main guide)
- [x] SCRIPT_OUTPUTS_REFERENCE.md (outputs list)
- [x] ADDITIONAL_LAYERS_GUIDE.md (forest structure)
- [x] ETH_CANOPY_HEIGHT_GUIDE.md ⭐ (height deep dive)

---

## 🎉 **YOU'RE ALL SET!**

**Total Package:**
- ✅ 10 production scripts (+ 2 utilities)
- ✅ 37 covariate files
- ✅ Both 10m and 250m resolutions
- ✅ Spectral + Topographic + Climate + Structure
- ✅ Best available data (ETH height!)

**Expected Impact:**
- R² improvement: +20-30% from height
- Stronger fuzzing effect demonstration
- Multi-scale analysis (10m vs 250m)
- Structural complexity insights
- High-impact manuscript

**Processing Time:**
- Core exports: ~2-3 hours
- Enhanced exports: +1-2 hours
- Total: ~3-5 hours for everything

**Ready to run!** 🚀
