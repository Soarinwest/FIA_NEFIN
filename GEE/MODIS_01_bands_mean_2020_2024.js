// ===========================================================
// MODIS INDIVIDUAL BANDS: 2020–2024 MEAN
// Product: MOD09Q1 (250m) + MOD09A1 (500m→250m)
// Bands: B1 (Red), B2 (NIR), B3 (Blue), B4 (Green), B6 (SWIR1)
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 250m
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// MOD09Q1: 250m surface reflectance (Red, NIR)
var mod09q1 = ee.ImageCollection('MODIS/061/MOD09Q1');

// MOD09A1: 500m surface reflectance (Blue, Green, SWIR1, SWIR2)
var mod09a1 = ee.ImageCollection('MODIS/061/MOD09A1');

// Get native MODIS projection
var modisProj = mod09q1.first().select('sur_refl_b01').projection();

// Reproject region to MODIS CRS
var regionMODIS = region.transform(modisProj, 1);

// QA masking function for MOD09Q1
function maskMOD09Q1(image) {
  var qa = image.select('QA');
  // Keep only high quality pixels (bits 0-1 = 00)
  var mask = qa.bitwiseAnd(3).eq(0);
  
  return image.select(['sur_refl_b01', 'sur_refl_b02'])
              .multiply(0.0001)  // Scale factor
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

// QA masking function for MOD09A1
function maskMOD09A1(image) {
  var qa = image.select('StateQA');
  // Keep clear pixels (bits 0-1 = 00)
  var mask = qa.bitwiseAnd(3).eq(0);
  
  return image.select(['sur_refl_b03', 'sur_refl_b04', 'sur_refl_b06'])
              .multiply(0.0001)  // Scale factor
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

// Filter collections
var q1_filtered = mod09q1
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))
  .map(maskMOD09Q1);

var a1_filtered = mod09a1
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))
  .map(maskMOD09A1);

print('MOD09Q1 collection size:', q1_filtered.size());
print('MOD09A1 collection size:', a1_filtered.size());

// ===================================
// PROCESS 250m BANDS (Red, NIR)
// ===================================

var bands250m = [
  {modis: 'sur_refl_b01', name: 'RED', description: 'Red (620-670nm)'},
  {modis: 'sur_refl_b02', name: 'NIR', description: 'NIR (841-876nm)'}
];

bands250m.forEach(function(band) {
  var composite = q1_filtered
    .select(band.modis)
    .mean()
    .clip(regionMODIS)
    .rename('MODIS_' + band.name);
  
  Map.addLayer(composite, 
    {min: 0, max: 0.4, palette: ['black', 'white']}, 
    'MODIS ' + band.name, false);
  
  Export.image.toDrive({
    image: composite,
    description: 'MODIS_' + band.name + '_250m_2020_2024',
    folder: 'NEFIN_FIA_Covariates',
    fileNamePrefix: 'MODIS_' + band.name + '_250m_2020_2024_NE',
    region: region,
    scale: 250,
    crs: 'EPSG:4326',
    maxPixels: 1e13
  });
  
  print('Exported: MODIS_' + band.name + '_250m_2020_2024');
});

// ===================================
// PROCESS 500m→250m BANDS (Blue, Green, SWIR1)
// ===================================

var bands500m = [
  {modis: 'sur_refl_b03', name: 'BLUE', description: 'Blue (459-479nm)'},
  {modis: 'sur_refl_b04', name: 'GREEN', description: 'Green (545-565nm)'},
  {modis: 'sur_refl_b06', name: 'SWIR1', description: 'SWIR1 (1628-1652nm)'}
];

bands500m.forEach(function(band) {
  var composite = a1_filtered
    .select(band.modis)
    .mean()
    .clip(regionMODIS)
    .rename('MODIS_' + band.name);
  
  Map.addLayer(composite, 
    {min: 0, max: 0.4, palette: ['black', 'white']}, 
    'MODIS ' + band.name, false);
  
  // Export at 250m (will resample from 500m)
  Export.image.toDrive({
    image: composite,
    description: 'MODIS_' + band.name + '_250m_2020_2024',
    folder: 'NEFIN_FIA_Covariates',
    fileNamePrefix: 'MODIS_' + band.name + '_250m_2020_2024_NE',
    region: region,
    scale: 250,  // Resampled from 500m
    crs: 'EPSG:4326',
    maxPixels: 1e13
  });
  
  print('Exported: MODIS_' + band.name + '_250m_2020_2024 (resampled from 500m)');
});
