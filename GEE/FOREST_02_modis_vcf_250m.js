// ===========================================================
// VEGETATION CONTINUOUS FIELDS: MODIS VCF (250m)
// Product: MODIS/006/MOD44B (annual, 2000-2022)
// Resolution: 250m
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Purpose: Tree cover at MODIS scale (complements Hansen)
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// ===================================
// LOAD MODIS VCF
// ===================================

var vcf = ee.ImageCollection('MODIS/061/MOD44B');

print('MODIS VCF collection loaded');

// Get MODIS projection
var modisProj = vcf.first().select('Percent_Tree_Cover').projection();
var regionMODIS = region.transform(modisProj, 1);

// ===================================
// EXTRACT 2020 DATA
// ===================================

// VCF is annual data - get 2020
var vcf2020 = vcf
  .filterDate('2020-01-01', '2020-12-31')
  .first();

// Extract key bands
var treecover = vcf2020.select('Percent_Tree_Cover').clip(regionMODIS);
var nonTreeVeg = vcf2020.select('Percent_NonTree_Vegetation').clip(regionMODIS);
var bareGround = vcf2020.select('Percent_NonVegetated').clip(regionMODIS);

// Quality layer
var quality = vcf2020.select('Quality').clip(regionMODIS);

print('VCF 2020 loaded');

// ===================================
// VISUALIZATION
// ===================================

Map.addLayer(treecover, 
  {min: 0, max: 100, palette: ['white', 'lightgreen', 'green', 'darkgreen']}, 
  'Tree Cover 2020 (%)', true);

Map.addLayer(nonTreeVeg, 
  {min: 0, max: 100, palette: ['white', 'yellow', 'orange']}, 
  'Non-Tree Vegetation (%)', false);

Map.addLayer(bareGround, 
  {min: 0, max: 100, palette: ['white', 'brown', 'black']}, 
  'Bare Ground (%)', false);

// High quality forest mask (>50% tree cover, good quality)
var forestMask = treecover.gt(50).and(quality.lte(1));
Map.addLayer(forestMask.selfMask(), 
  {palette: ['darkgreen']}, 
  'Dense Forest (>50%)', false);

// ===================================
// EXPORT VCF 2020
// ===================================

// Tree cover percentage
Export.image.toDrive({
  image: treecover,
  description: 'VCF_treecover_250m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'VCF_treecover_250m_2020_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: VCF_treecover_250m_2020');

// Non-tree vegetation (optional)
/*
Export.image.toDrive({
  image: nonTreeVeg,
  description: 'VCF_nontree_250m_2020',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'VCF_nontree_250m_2020_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
*/

// ===================================
// TIME SERIES: 2020-2024 MEAN
// ===================================

// Calculate mean tree cover 2020-2022 (2023-2024 not yet available)
var vcf_timeseries = vcf
  .filterDate('2020-01-01', '2022-12-31')
  .select('Percent_Tree_Cover');

var treecover_mean = vcf_timeseries
  .mean()
  .clip(regionMODIS)
  .rename('treecover_mean');

print('VCF time series size (2020-2022):', vcf_timeseries.size());

Map.addLayer(treecover_mean, 
  {min: 0, max: 100, palette: ['white', 'lightgreen', 'green', 'darkgreen']}, 
  'Tree Cover Mean (2020-2022)', false);

// Export mean
Export.image.toDrive({
  image: treecover_mean,
  description: 'VCF_treecover_250m_2020_2022',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'VCF_treecover_250m_2020_2022_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: VCF_treecover_250m_2020_2022 (mean)');

// ===================================
// COMPARE WITH HANSEN
// ===================================

// Load Hansen for comparison (if needed)
var hansen = ee.Image('UMD/hansen/global_forest_change_2023_v1_11');
var hansenCover = hansen.select('treecover2000')
  .reduceResolution({
    reducer: ee.Reducer.mean(),
    maxPixels: 1024
  })
  .reproject({
    crs: modisProj,
    scale: 250
  })
  .clip(regionMODIS);

// Calculate difference (VCF - Hansen)
var coverDiff = treecover.subtract(hansenCover).rename('cover_difference');

Map.addLayer(coverDiff, 
  {min: -50, max: 50, palette: ['red', 'white', 'blue']}, 
  'VCF - Hansen Difference', false);

print('');
print('========================================');
print('MODIS VCF SUMMARY');
print('========================================');
print('');
print('Product: MODIS Vegetation Continuous Fields (MOD44B)');
print('Resolution: 250m (native MODIS scale)');
print('Available Years: 2000-2022 (annual)');
print('Temporal: 2020 + Mean 2020-2022');
print('');
print('Exports:');
print('  1. VCF_treecover_250m_2020_NE.tif');
print('  2. VCF_treecover_250m_2020_2022_NE.tif (mean)');
print('');
print('WHY THIS MATTERS:');
print('  - Native 250m resolution (matches MODIS analysis)');
print('  - Annual updates (more current than Hansen 2000)');
print('  - Provides tree/non-tree/bare breakdown');
print('  - Perfect complement to 250m MODIS spectral data');
print('');
print('RECOMMENDED USE:');
print('  - Use VCF for 250m MODIS models');
print('  - Use Hansen for 10m Sentinel-2 models');
print('  - Both provide tree cover but at different scales');
print('');
print('NOTE: VCF 2023-2024 not yet available');
print('      Using 2020 or 2020-2022 mean for now');
print('');
