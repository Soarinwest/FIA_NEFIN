// ===========================================================
// MODIS SPECTRAL INDICES: 2020–2024 MEAN
// Indices: NDVI, EVI, NBR, NDWI
// Product: MOD13Q1 (NDVI, EVI) + MOD09Q1/A1 (NBR, NDWI)
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 250m
// CRS: EPSG:5070 (Albers Equal Area)
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

// ===========================================================
// PART 1: NDVI & EVI from MOD13Q1 (Pre-computed indices)
// ===========================================================

var mod13q1 = ee.ImageCollection('MODIS/061/MOD13Q1');

function maskMOD13Q1(image) {
  var qa = image.select('SummaryQA');
  var mask = qa.lte(1);  // Good quality (0=good, 1=marginal)
  
  // Scale factors: 0.0001 for both NDVI and EVI
  var ndvi = image.select('NDVI').multiply(0.0001);
  var evi = image.select('EVI').multiply(0.0001);
  
  return image.addBands(ndvi, null, true)
              .addBands(evi, null, true)
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

var mod13q1_filtered = mod13q1
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))  // May–Sep (growing season)
  .map(maskMOD13Q1);

print('MOD13Q1 collection size:', mod13q1_filtered.size());

// --- NDVI Composite ---
var ndviComposite = mod13q1_filtered
  .select('NDVI')
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_NDVI');

Map.addLayer(ndviComposite, 
  {min: 0.3, max: 0.9, palette: ['red', 'yellow', 'green']}, 
  'MODIS NDVI', true);

// --- EVI Composite ---
var eviComposite = mod13q1_filtered
  .select('EVI')
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_EVI');

Map.addLayer(eviComposite, 
  {min: 0, max: 0.8, palette: ['red', 'yellow', 'green']}, 
  'MODIS EVI', false);

// ===========================================================
// PART 2: NBR & NDWI from Surface Reflectance
// ===========================================================

var mod09q1 = ee.ImageCollection('MODIS/061/MOD09Q1');  // 250m: Red, NIR
var mod09a1 = ee.ImageCollection('MODIS/061/MOD09A1');  // 500m: Blue, Green, SWIR

function maskMOD09Q1(image) {
  var qa = image.select('QA');
  var mask = qa.bitwiseAnd(3).eq(0);  // Clear pixels
  
  return image.select(['sur_refl_b01', 'sur_refl_b02'])
              .multiply(0.0001)
              .updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

function maskMOD09A1(image) {
  var qa = image.select('StateQA');
  var mask = qa.bitwiseAnd(3).eq(0);  // Clear pixels
  
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

// Join Q1 (250m) and A1 (500m) by date
var filter = ee.Filter.equals({
  leftField: 'system:time_start',
  rightField: 'system:time_start'
});
var joined = ee.Join.inner().apply(q1_filtered, a1_filtered, filter);

print('Joined collection size:', joined.size());

// --- NBR: (NIR - SWIR2) / (NIR + SWIR2) ---
var nbrCollection = ee.ImageCollection(joined.map(function(feature) {
  var q1 = ee.Image(feature.get('primary'));
  var a1 = ee.Image(feature.get('secondary'));
  
  var nir = q1.select('sur_refl_b02');
  var swir2 = a1.select('sur_refl_b07');  // SWIR2 (2105-2155nm)
  
  var nbr = nir.subtract(swir2)
               .divide(nir.add(swir2))
               .rename('NBR');
  
  return nbr.copyProperties(q1, ['system:time_start']);
}));

var nbrComposite = nbrCollection
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_NBR');

Map.addLayer(nbrComposite, 
  {min: -1, max: 1, palette: ['brown', 'yellow', 'green']}, 
  'MODIS NBR', false);

// --- NDWI: (Green - NIR) / (Green + NIR) ---
var ndwiCollection = ee.ImageCollection(joined.map(function(feature) {
  var q1 = ee.Image(feature.get('primary'));
  var a1 = ee.Image(feature.get('secondary'));
  
  var nir = q1.select('sur_refl_b02');
  var green = a1.select('sur_refl_b04');
  
  var ndwi = green.subtract(nir)
                  .divide(green.add(nir))
                  .rename('NDWI');
  
  return ndwi.copyProperties(q1, ['system:time_start']);
}));

var ndwiComposite = ndwiCollection
  .mean()
  .clip(regionMODIS)
  .rename('MODIS_NDWI');

Map.addLayer(ndwiComposite, 
  {min: -1, max: 1, palette: ['brown', 'white', 'blue']}, 
  'MODIS NDWI', false);

// ===========================================================
// EXPORT ALL FOUR INDICES (EPSG:5070)
// ===========================================================

// Export 1: NDVI
Export.image.toDrive({
  image: ndviComposite,
  description: 'MODIS_NDVI_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_NDVI_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

// Export 2: EVI
Export.image.toDrive({
  image: eviComposite,
  description: 'MODIS_EVI_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_EVI_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

// Export 3: NBR
Export.image.toDrive({
  image: nbrComposite,
  description: 'MODIS_NBR_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_NBR_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

// Export 4: NDWI
Export.image.toDrive({
  image: ndwiComposite,
  description: 'MODIS_NDWI_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'MODIS_NDWI_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:5070',  // ← Changed to 5070
  maxPixels: 1e13
});

print('✓ All exports configured in EPSG:5070');
print('  1. MODIS NDVI - Vegetation greenness');
print('  2. MODIS EVI - Enhanced vegetation index');
print('  3. MODIS NBR - Burn ratio (disturbance)');
print('  4. MODIS NDWI - Water content');

// ===========================================================
// NOTES
// ===========================================================
// 
// MODIS INDICES (250m):
// 
// 1. NDVI (Normalized Difference Vegetation Index):
//    - (NIR - Red) / (NIR + Red)
//    - Standard vegetation greenness
//    - Range: -1 to 1 (higher = more vegetation)
//    - From MOD13Q1 (pre-computed, QA'd)
//
// 2. EVI (Enhanced Vegetation Index):
//    - Reduces atmospheric/soil effects vs NDVI
//    - Better in high biomass areas
//    - More sensitive to canopy structural variations
//    - From MOD13Q1 (pre-computed, QA'd)
//
// 3. NBR (Normalized Burn Ratio):
//    - (NIR - SWIR2) / (NIR + SWIR2)
//    - Sensitive to fire damage and disturbance
//    - Lower values = burned/disturbed areas
//    - Calculated from MOD09Q1 + MOD09A1
//
// 4. NDWI (Normalized Difference Water Index):
//    - (Green - NIR) / (Green + NIR)
//    - Water content and moisture stress
//    - Higher values = more water/moisture
//    - Calculated from MOD09Q1 + MOD09A1
//
// QUALITY FILTERING:
// - MOD13Q1: SummaryQA ≤ 1 (good/marginal quality)
// - MOD09: State/QA bitwise filter for clear pixels
// - Growing season only: May–September
// - 5-year mean: 2020–2024
//
// EPSG:5070 (Albers Equal Area Conic):
// - Best for continental US area calculations
// - Preserves area (critical for biomass!)
// - Matches MODIS native projection characteristics
//
// ===========================================================