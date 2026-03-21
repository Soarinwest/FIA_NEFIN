// ===========================================================
// Sentinel-2 NDVI METRICS: 2020–2024
// Exports: Mean, Std Dev, and CV
// Purpose: Greenness + temporal variability indicators
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 10m
// CRS: EPSG:5070 (Albers Equal Area)
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];
var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry()
  .dissolve();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// ===========================================================
// CLOUD MASKING
// ===========================================================

function maskS2clouds(image) {
  var scl = image.select('SCL');
  var mask = scl.eq(4).or(scl.eq(5)).or(scl.eq(6)); // veg, bare, water
  return image.updateMask(mask).copyProperties(image, ['system:time_start']);
}

// ===========================================================
// ADD NDVI BAND
// ===========================================================

function addNDVI(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
}

// ===========================================================
// LOAD AND PROCESS SENTINEL-2
// ===========================================================

var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month')) // May–Sep (growing season)
  .map(maskS2clouds)
  .map(addNDVI);

print('Sentinel-2 collection size:', s2.size());

// ===========================================================
// CALCULATE NDVI METRICS
// ===========================================================

// 1. NDVI MEAN - Overall greenness
var ndviMean = s2
  .select('NDVI')
  .mean()
  .rename('NDVI_Mean')
  .clip(region)
  .reproject({
    crs: 'EPSG:5070',
    scale: 10
  });

// 2. NDVI STANDARD DEVIATION - Temporal variability
var ndviSD = s2
  .select('NDVI')
  .reduce(ee.Reducer.stdDev())
  .rename('NDVI_SD')
  .clip(region)
  .reproject({
    crs: 'EPSG:5070',
    scale: 10
  });

// 3. NDVI COEFFICIENT OF VARIATION - Normalized variability
var ndviCV = ndviSD.divide(ndviMean).rename('NDVI_CV')
  .clip(region)
  .reproject({
    crs: 'EPSG:5070',
    scale: 10
  });

// ===========================================================
// VISUALIZATION
// ===========================================================

Map.addLayer(ndviMean, 
  {min: 0.3, max: 0.9, palette: ['red', 'yellow', 'green']}, 
  'NDVI Mean', true);

Map.addLayer(ndviSD, 
  {min: 0, max: 0.2, palette: ['white', 'yellow', 'red']}, 
  'NDVI Std Dev', false);

Map.addLayer(ndviCV, 
  {min: 0, max: 0.3, palette: ['white', 'orange', 'red']}, 
  'NDVI CV', false);

// ===========================================================
// EXPORT ALL THREE METRICS (EPSG:5070)
// ===========================================================

// Export 1: NDVI Mean
Export.image.toDrive({
  image: ndviMean,
  description: 'S2_NDVI_Mean_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'S2_NDVI_Mean_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

// Export 2: NDVI Standard Deviation
Export.image.toDrive({
  image: ndviSD,
  description: 'S2_NDVI_SD_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'S2_NDVI_SD_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

// Export 3: NDVI Coefficient of Variation
Export.image.toDrive({
  image: ndviCV,
  description: 'S2_NDVI_CV_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'S2_NDVI_CV_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

print('✓ All exports configured in EPSG:5070');
print('  1. NDVI Mean - Overall greenness');
print('  2. NDVI SD - Temporal variability');
print('  3. NDVI CV - Normalized variability');

// ===========================================================
// NOTES
// ===========================================================
// 
// NDVI MEAN:
// - Baseline vegetation greenness (2020-2024 growing season)
// - Higher values = denser/healthier vegetation
// - Use this for standard biomass modeling
//
// NDVI STANDARD DEVIATION (SD):
// - Temporal variability across 5 years
// - High SD = disturbance, phenology changes, variable conditions
// - Low SD = stable forests
// - Useful for detecting recent disturbances or dynamic areas
//
// NDVI COEFFICIENT OF VARIATION (CV = SD/Mean):
// - Normalized variability (accounts for mean NDVI)
// - Better for comparing variability across different forest types
// - High CV in low-productivity areas might just reflect low mean
//
// EPSG:5070 (Albers Equal Area Conic):
// - Best for continental US area calculations
// - Preserves area (important for biomass!)
// - Native CRS for many US ecological datasets
//
// All three exports will be in EPSG:5070 for direct use in your models
//
// ===========================================================