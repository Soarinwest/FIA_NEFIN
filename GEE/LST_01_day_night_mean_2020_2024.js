// ===========================================================
// MODIS LAND SURFACE TEMPERATURE: 2020–2024 MEAN
// Product: MOD11A2 (8-day composite, 1km)
// Layers: LST Day, LST Night
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 1km (native)
// Purpose: Microclimate variation (complements PRISM)
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// ===================================
// LOAD MODIS LST
// ===================================

var mod11a2 = ee.ImageCollection('MODIS/061/MOD11A2');

// Get native MODIS projection
var modisProj = mod11a2.first().select('LST_Day_1km').projection();
var regionMODIS = region.transform(modisProj, 1);

// QA masking function
function maskLST(image) {
  var qa = image.select('QC_Day');  // Day QA (same structure for Night)
  
  // Keep only high quality pixels
  // Bits 0-1 = 00 (good quality)
  var mask = qa.bitwiseAnd(3).eq(0);
  
  // Scale and convert Kelvin to Celsius
  var lstDay = image.select('LST_Day_1km')
    .multiply(0.02)  // Scale factor
    .subtract(273.15);  // K to C
  
  var lstNight = image.select('LST_Night_1km')
    .multiply(0.02)
    .subtract(273.15);
  
  return image.addBands([lstDay.rename('LST_Day_C'), lstNight.rename('LST_Night_C')])
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

// Filter collection
var lstCollection = mod11a2
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))  // May-Sep
  .map(maskLST);

print('MODIS LST collection size:', lstCollection.size());

// ===================================
// CALCULATE MEAN LST
// ===================================

// Day LST
var lstDayMean = lstCollection
  .select('LST_Day_C')
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_LST_Day');

// Night LST
var lstNightMean = lstCollection
  .select('LST_Night_C')
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_LST_Night');

// Diurnal temperature range (Day - Night)
var lstRange = lstDayMean.subtract(lstNightMean).rename('MODIS_LST_Range');

// ===================================
// VISUALIZATION
// ===================================

Map.addLayer(lstDayMean, 
  {min: 15, max: 30, palette: ['blue', 'green', 'yellow', 'red']}, 
  'LST Day Mean (°C)', false);

Map.addLayer(lstNightMean, 
  {min: 5, max: 20, palette: ['blue', 'cyan', 'yellow', 'orange']}, 
  'LST Night Mean (°C)', false);

Map.addLayer(lstRange, 
  {min: 5, max: 20, palette: ['white', 'orange', 'red']}, 
  'LST Diurnal Range (°C)', false);

// ===================================
// EXPORT
// ===================================

// Day LST
Export.image.toDrive({
  image: lstDayMean,
  description: 'MODIS_LST_day_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_LST_day_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Night LST
Export.image.toDrive({
  image: lstNightMean,
  description: 'MODIS_LST_night_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_LST_night_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Optional: Diurnal Range
Export.image.toDrive({
  image: lstRange,
  description: 'MODIS_LST_range_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_LST_range_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('All LST layers queued for export');

// ===================================
// BONUS: Calculate LST Statistics
// ===================================

// Can also calculate temporal variability if useful
var lstDaySD = lstCollection
  .select('LST_Day_C')
  .reduce(ee.Reducer.stdDev())
  .clip(regionMODIS)
  .rename('MODIS_LST_Day_SD');

var lstNightSD = lstCollection
  .select('LST_Night_C')
  .reduce(ee.Reducer.stdDev())
  .clip(regionMODIS)
  .rename('MODIS_LST_Night_SD');

Map.addLayer(lstDaySD, 
  {min: 0, max: 5, palette: ['white', 'yellow', 'red']}, 
  'LST Day Variability', false);

// Uncomment to export standard deviations
/*
Export.image.toDrive({
  image: lstDaySD,
  description: 'MODIS_LST_day_SD_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_LST_day_SD_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: lstNightSD,
  description: 'MODIS_LST_night_SD_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_LST_night_SD_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});
*/
