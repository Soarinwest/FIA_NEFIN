// ===========================================================
// Sentinel-2 NDVI STANDARD DEVIATION: 2020–2024
// Purpose: Temporal variability (phenology/disturbance indicator)
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 10m
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry()
  .dissolve();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// Cloud masking function
function maskS2clouds(image) {
  var scl = image.select('SCL');
  var mask = scl.eq(4).or(scl.eq(5)).or(scl.eq(6)); // veg, bare, water
  return image.updateMask(mask).copyProperties(image, ['system:time_start']);
}

// Function to add NDVI
function addNDVI(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
}

// Load and process Sentinel-2 collection
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month')) // May–Sep
  .map(maskS2clouds)
  .map(addNDVI);

print('Sentinel-2 collection size:', s2.size());

// Calculate NDVI standard deviation (temporal variability)
var ndviSD = s2
  .select('NDVI')
  .reduce(ee.Reducer.stdDev())
  .rename('NDVI_SD')
  .clip(region)
  .reproject({
    crs: 'EPSG:5070',
    scale: 10
  });

// Visualization - higher SD = more variable (disturbance/phenology)
Map.addLayer(ndviSD, 
  {min: 0, max: 0.2, palette: ['white', 'yellow', 'red']}, 
  'S2 NDVI Std Dev', true);

// Export to Drive
Export.image.toDrive({
  image: ndviSD,
  description: 'S2_NDVI_SD_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'S2_NDVI_SD_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: S2_NDVI_SD_10m_2020_2024');

// Optional: Also calculate coefficient of variation (CV = SD / Mean)
var ndviMean = s2.select('NDVI').mean();
var ndviCV = ndviSD.divide(ndviMean).rename('NDVI_CV');

Map.addLayer(ndviCV, 
  {min: 0, max: 0.3, palette: ['white', 'orange', 'red']}, 
  'S2 NDVI CV', false);

// Uncomment to export CV as well
/*
Export.image.toDrive({
  image: ndviCV,
  description: 'S2_NDVI_CV_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'S2_NDVI_CV_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
*/
