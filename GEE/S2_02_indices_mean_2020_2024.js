// ===========================================================
// Sentinel-2 SPECTRAL INDICES: 2020–2024 MEAN
// Indices: EVI, NBR, NDWI (NDVI already processed separately)
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

// Function to add spectral indices
function addIndices(image) {
  // EVI = 2.5 × (NIR-Red)/(NIR+6×Red-7.5×Blue+1)
  var evi = image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'NIR': image.select('B8'),
      'RED': image.select('B4'),
      'BLUE': image.select('B2')
    }).rename('EVI');
  
  // NBR = (NIR - SWIR2) / (NIR + SWIR2)
  // Note: Using B12 (SWIR2) for NBR, not B11
  var nbr = image.normalizedDifference(['B8', 'B12']).rename('NBR');
  
  // NDWI = (Green - NIR) / (Green + NIR)
  var ndwi = image.normalizedDifference(['B3', 'B8']).rename('NDWI');
  
  return image.addBands([evi, nbr, ndwi]);
}

// Load and process Sentinel-2 collection
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month')) // May–Sep
  .map(maskS2clouds)
  .map(addIndices);

print('Sentinel-2 collection size:', s2.size());

// Define indices to export
var indices = [
  {band: 'EVI', min: -1, max: 1, palette: ['red', 'yellow', 'green']},
  {band: 'NBR', min: -1, max: 1, palette: ['brown', 'yellow', 'green']},
  {band: 'NDWI', min: -1, max: 1, palette: ['brown', 'white', 'blue']}
];

// Process each index
indices.forEach(function(idx) {
  var composite = s2
    .select(idx.band)
    .mean()
    .clip(region)  // Explicitly clip to region
    .reproject({
      crs: 'EPSG:5070',
      scale: 10
    });
  
  // Visualization
  Map.addLayer(composite, 
    {min: idx.min, max: idx.max, palette: idx.palette}, 
    'S2 ' + idx.band + ' Mean', false);
  
  // Export to Drive
  Export.image.toDrive({
    image: composite,
    description: 'S2_' + idx.band + '_10m_2020_2024',
    folder: 'NEFIN_FIA_Covariates',
    fileNamePrefix: 'S2_' + idx.band + '_10m_2020_2024_NE',
    region: region,
    scale: 10,
    crs: 'EPSG:4326',
    maxPixels: 1e13
  });
  
  print('Exported: S2_' + idx.band + '_10m_2020_2024');
});
