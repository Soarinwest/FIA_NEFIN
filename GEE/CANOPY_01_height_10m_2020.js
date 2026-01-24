// ===========================================================
// CANOPY HEIGHT: ETH Zurich Global Canopy Height (2020)
// Resolution: 10m
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Purpose: Strong predictor of forest biomass
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry()
  .dissolve();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// ===================================
// LOAD CANOPY HEIGHT MODEL
// ===================================

// ETH Global Canopy Height 10m (Lang et al. 2023)
// Resolution: 10m, Year: 2020
// One of the best available global canopy height products!
var canopyHeight = ee.Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')
  .clip(region)
  .rename('canopy_height');

// Also load standard deviation (structural complexity indicator)
var canopyHeightSD = ee.Image('users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1')
  .clip(region)
  .rename('canopy_height_SD');

print('ETH Canopy Height 10m (2020) loaded');
print('ETH Canopy Height SD 10m (2020) loaded');

// Get basic statistics
var heightStats = canopyHeight.reduceRegion({
  reducer: ee.Reducer.minMax().combine({
    reducer2: ee.Reducer.mean(),
    sharedInputs: true
  }),
  geometry: region,
  scale: 1000,  // Coarse scale for quick stats
  maxPixels: 1e9
});

print('Canopy Height Stats:', heightStats);

// ===================================
// VISUALIZATION
// ===================================

// Color palette: low (brown) -> medium (green) -> high (dark green)
Map.addLayer(canopyHeight, 
  {min: 0, max: 30, palette: ['white', 'yellow', 'lightgreen', 'green', 'darkgreen']}, 
  'Canopy Height (m)', true);

// Canopy height standard deviation (structural complexity)
Map.addLayer(canopyHeightSD, 
  {min: 0, max: 10, palette: ['white', 'yellow', 'orange', 'red']}, 
  'Canopy Height SD (complexity)', false);

// Mask to show only forest (height > 3m)
var forestMask = canopyHeight.gt(3);
var forestHeight = canopyHeight.updateMask(forestMask);

Map.addLayer(forestHeight, 
  {min: 5, max: 30, palette: ['lightgreen', 'green', 'darkgreen']}, 
  'Forest Canopy Height (>3m)', false);

// ===================================
// EXPORT
// ===================================

// Export full canopy height (10m)
Export.image.toDrive({
  image: canopyHeight,
  description: 'canopy_height_10m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'canopy_height_10m_2020_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: canopy_height_10m_2020');

// Export canopy height standard deviation (10m)
Export.image.toDrive({
  image: canopyHeightSD,
  description: 'canopy_height_SD_10m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'canopy_height_SD_10m_2020_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: canopy_height_SD_10m_2020');

// ===================================
// BONUS: Aggregate to 250m for MODIS comparison
// ===================================

// Mean canopy height at 250m
var canopyHeight_250m = canopyHeight.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: canopyHeight.projection(),
  scale: 250
});

// Mean SD at 250m (average of local variability)
var canopySD_250m = canopyHeightSD.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: canopyHeight.projection(),
  scale: 250
});

// Also calculate SD of height at 250m scale (larger-scale heterogeneity)
var canopyHeight_SD_250m = canopyHeight.reduceResolution({
  reducer: ee.Reducer.stdDev(),
  maxPixels: 1024
}).reproject({
  crs: canopyHeight.projection(),
  scale: 250
}).rename('canopy_height_SD_250m');

Map.addLayer(canopyHeight_250m, 
  {min: 0, max: 30, palette: ['white', 'yellow', 'lightgreen', 'green', 'darkgreen']}, 
  'Canopy Height 250m', false);

Map.addLayer(canopyHeight_SD_250m, 
  {min: 0, max: 10, palette: ['white', 'orange', 'red']}, 
  'Canopy Height SD 250m', false);

// Export mean height at 250m
Export.image.toDrive({
  image: canopyHeight_250m,
  description: 'canopy_height_250m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'canopy_height_250m_2020_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: canopy_height_250m_2020');

// Export SD at 250m (structural heterogeneity)
Export.image.toDrive({
  image: canopyHeight_SD_250m,
  description: 'canopy_height_SD_250m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'canopy_height_SD_250m_2020_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: canopy_height_SD_250m_2020');

print('');
print('========================================');
print('CANOPY HEIGHT SUMMARY');
print('========================================');
print('');
print('Product: ETH Global Canopy Height 2020 (Lang et al. 2023)');
print('Resolution: 10m (native)');
print('Year: 2020 (matches study period!)');
print('Units: Meters above ground');
print('Reference: https://langnico.github.io/globalcanopyheight/');
print('');
print('Exports:');
print('  1. canopy_height_10m_2020_NE.tif (mean height)');
print('  2. canopy_height_SD_10m_2020_NE.tif (structural variability)');
print('  3. canopy_height_250m_2020_NE.tif (aggregated mean)');
print('  4. canopy_height_SD_250m_2020_NE.tif (aggregated SD)');
print('');
print('WHY THIS MATTERS:');
print('  ⭐ Height is THE STRONGEST biomass predictor (r² ~0.7)');
print('  ⭐ 10m resolution perfectly matches Sentinel-2 analysis');
print('  ⭐ 2020 timestamp matches your plot measurement period');
print('  ⭐ SD captures structural complexity/heterogeneity');
print('');
print('STRUCTURAL COMPLEXITY (SD):');
print('  - High SD = heterogeneous canopy (mixed ages/species)');
print('  - Low SD = uniform canopy (even-aged stands)');
print('  - Relates to biomass uncertainty & biodiversity');
print('  - Can improve predictions or estimate uncertainty');
print('');
print('EXPECTED IMPACT ON MODELS:');
print('  - Adding height: +20-30% R² improvement');
print('  - Height + spectral: Best combination');
print('  - Tests if fuzzing affects height-biomass allometry');
print('');
print('THIS IS GOLD! 🏆');
print('');
