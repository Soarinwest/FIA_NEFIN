// ===========================================================
// MODIS SPECTRAL INDICES: 2020–2024 MEAN
// Indices: EVI, NBR, NDWI (NDVI already done separately)
// Product: MOD13Q1 (EVI pre-computed) + MOD09Q1/A1 (for NBR, NDWI)
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

// Get native MODIS projection
var modisProj = ee.ImageCollection('MODIS/061/MOD13Q1')
  .first().select('NDVI').projection();

var regionMODIS = region.transform(modisProj, 1);

// ===================================
// OPTION 1: EVI from MOD13Q1 (easiest!)
// ===================================

var mod13q1 = ee.ImageCollection('MODIS/061/MOD13Q1');

function maskMOD13Q1(image) {
  var qa = image.select('SummaryQA');
  var mask = qa.lte(1);  // Good quality
  
  var evi = image.select('EVI').multiply(0.0001);
  
  return evi.updateMask(mask)
            .copyProperties(image, ['system:time_start']);
}

var eviComposite = mod13q1
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))
  .map(maskMOD13Q1)
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_EVI');

Map.addLayer(eviComposite, 
  {min: 0, max: 0.8, palette: ['red', 'yellow', 'green']}, 
  'MODIS EVI', false);

Export.image.toDrive({
  image: eviComposite,
  description: 'MODIS_EVI_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_EVI_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: MODIS_EVI_250m_2020_2024');

// ===================================
// OPTION 2: Calculate NBR and NDWI from surface reflectance
// ===================================

var mod09q1 = ee.ImageCollection('MODIS/061/MOD09Q1');  // 250m: Red, NIR
var mod09a1 = ee.ImageCollection('MODIS/061/MOD09A1');  // 500m: Blue, Green, SWIR1, SWIR2

function maskMOD09Q1(image) {
  var qa = image.select('QA');
  var mask = qa.bitwiseAnd(3).eq(0);
  
  return image.select(['sur_refl_b01', 'sur_refl_b02'])
              .multiply(0.0001)
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

function maskMOD09A1(image) {
  var qa = image.select('StateQA');
  var mask = qa.bitwiseAnd(3).eq(0);
  
  return image.select(['sur_refl_b04', 'sur_refl_b06', 'sur_refl_b07'])
              .multiply(0.0001)
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

// Calculate NBR = (NIR - SWIR2) / (NIR + SWIR2)
// Need to join 250m NIR with 500m SWIR2
var filter = ee.Filter.equals({leftField: 'system:time_start', rightField: 'system:time_start'});
var joined = ee.Join.inner().apply(q1_filtered, a1_filtered, filter);

var nbrCollection = ee.ImageCollection(joined.map(function(feature) {
  var q1 = ee.Image(feature.get('primary'));
  var a1 = ee.Image(feature.get('secondary'));
  
  var nir = q1.select('sur_refl_b02');
  var swir2 = a1.select('sur_refl_b07');  // SWIR2 (2105-2155nm)
  
  var nbr = nir.subtract(swir2).divide(nir.add(swir2)).rename('NBR');
  
  return nbr.copyProperties(q1, ['system:time_start']);
}));

var nbrComposite = nbrCollection
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_NBR');

Map.addLayer(nbrComposite, 
  {min: -1, max: 1, palette: ['brown', 'yellow', 'green']}, 
  'MODIS NBR', false);

Export.image.toDrive({
  image: nbrComposite,
  description: 'MODIS_NBR_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_NBR_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: MODIS_NBR_250m_2020_2024');

// Calculate NDWI = (Green - NIR) / (Green + NIR)
var ndwiCollection = ee.ImageCollection(joined.map(function(feature) {
  var q1 = ee.Image(feature.get('primary'));
  var a1 = ee.Image(feature.get('secondary'));
  
  var nir = q1.select('sur_refl_b02');
  var green = a1.select('sur_refl_b04');
  
  var ndwi = green.subtract(nir).divide(green.add(nir)).rename('NDWI');
  
  return ndwi.copyProperties(q1, ['system:time_start']);
}));

var ndwiComposite = ndwiCollection
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_NDWI');

Map.addLayer(ndwiComposite, 
  {min: -1, max: 1, palette: ['brown', 'white', 'blue']}, 
  'MODIS NDWI', false);

Export.image.toDrive({
  image: ndwiComposite,
  description: 'MODIS_NDWI_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_NDWI_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: MODIS_NDWI_250m_2020_2024');
