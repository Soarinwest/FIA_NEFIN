// ===========================================================
// FOREST STRUCTURE: Tree Cover & Loss
// Product: Hansen Global Forest Change v1.11 (2000-2023)
// Resolution: 30m
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Purpose: Forest cover density and disturbance history
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
// LOAD HANSEN GLOBAL FOREST CHANGE
// ===================================

var hansen = ee.Image('UMD/hansen/global_forest_change_2023_v1_11');

print('Hansen GFC v1.11 loaded (2000-2023)');

// ===================================
// EXTRACT KEY LAYERS
// ===================================

// 1. Tree canopy cover in year 2000 (0-100%)
var treecover2000 = hansen.select('treecover2000').clip(region);

// 2. Forest loss year (2001-2023)
var lossYear = hansen.select('lossyear').clip(region);

// 3. Forest gain 2000-2012 (binary)
var gain = hansen.select('gain').clip(region);

// 4. Create forest cover for 2020
// Start with 2000 cover, remove pixels lost before 2020
var lostBefore2020 = lossYear.gt(0).and(lossYear.lte(20));  // Years 1-20 = 2001-2020
var treecover2020 = treecover2000
  .where(lostBefore2020, 0)  // Set lost areas to 0
  .rename('treecover2020');

// 5. Recent disturbance (2016-2023)
var recentLoss = lossYear.gte(16).and(lossYear.lte(23));  // Years 16-23 = 2016-2023

// ===================================
// VISUALIZATION
// ===================================

Map.addLayer(treecover2000, 
  {min: 0, max: 100, palette: ['white', 'lightgreen', 'green', 'darkgreen']}, 
  'Tree Cover 2000 (%)', false);

Map.addLayer(treecover2020, 
  {min: 0, max: 100, palette: ['white', 'lightgreen', 'green', 'darkgreen']}, 
  'Tree Cover 2020 (%)', true);

Map.addLayer(lossYear.selfMask(), 
  {min: 1, max: 23, palette: ['yellow', 'orange', 'red']}, 
  'Loss Year (2001-2023)', false);

Map.addLayer(recentLoss.selfMask(), 
  {palette: ['red']}, 
  'Recent Loss (2016-2023)', false);

// ===================================
// EXPORT: TREE COVER 2020
// ===================================

// Tree cover percentage (2020) at native 30m
Export.image.toDrive({
  image: treecover2020,
  description: 'treecover_30m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'treecover_30m_2020_NE',
  region: region,
  scale: 30,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: treecover_30m_2020');

// ===================================
// AGGREGATE TO 10m (for Sentinel-2 comparison)
// ===================================

// Resample to 10m using mode/mean
var treecover_10m = treecover2020.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: 'EPSG:5070',
  scale: 10
});

Export.image.toDrive({
  image: treecover_10m,
  description: 'treecover_10m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'treecover_10m_2020_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: treecover_10m_2020 (resampled from 30m)');

// ===================================
// AGGREGATE TO 250m (for MODIS comparison)
// ===================================

var treecover_250m = treecover2020.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: 'EPSG:5070',
  scale: 250
});

Export.image.toDrive({
  image: treecover_250m,
  description: 'treecover_250m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'treecover_250m_2020_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: treecover_250m_2020');

// ===================================
// EXPORT: RECENT DISTURBANCE (2016-2023)
// ===================================

// Binary layer: 1 = disturbed, 0 = not disturbed
Export.image.toDrive({
  image: recentLoss.unmask(0).byte(),
  description: 'forest_loss_2016_2023',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'forest_loss_2016_2023_NE',
  region: region,
  scale: 30,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: forest_loss_2016_2023');

// ===================================
// BONUS: Disturbance-Free Forest Mask
// ===================================

// Pixels with high tree cover (>50%) and no recent loss
var stableForest = treecover2020.gt(50).and(recentLoss.not());

Map.addLayer(stableForest.selfMask(), 
  {palette: ['darkgreen']}, 
  'Stable Forest (>50%, no loss)', false);

// Optional: Export stable forest mask
/*
Export.image.toDrive({
  image: stableForest.unmask(0).byte(),
  description: 'stable_forest_mask_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'stable_forest_mask_2020_NE',
  region: region,
  scale: 30,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
*/

print('');
print('========================================');
print('HANSEN FOREST STRUCTURE SUMMARY');
print('========================================');
print('');
print('Product: Hansen Global Forest Change v1.11');
print('Base Year: 2000');
print('Updates Through: 2023');
print('Native Resolution: 30m');
print('');
print('Exports:');
print('  1. treecover_30m_2020_NE.tif (native)');
print('  2. treecover_10m_2020_NE.tif (resampled)');
print('  3. treecover_250m_2020_NE.tif (aggregated)');
print('  4. forest_loss_2016_2023_NE.tif (disturbance)');
print('');
print('WHY THIS MATTERS:');
print('  - Tree cover % strongly relates to biomass');
print('  - Filters disturbed areas from analysis');
print('  - Provides context for plot conditions');
print('  - Multi-resolution for both 10m and 250m models');
print('');
print('USAGE IN MODELS:');
print('  - Use treecover as covariate');
print('  - Mask out recent disturbance (2016-2023)');
print('  - Focus analysis on stable forests');
print('');
