// ===========================================================
// Sentinel-2 INDIVIDUAL BANDS: 2020–2024 MEAN
// Bands: B2 (Blue), B3 (Green), B4 (Red), B8 (NIR), B11 (SWIR1)
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 10m (B11 resampled from 20m)
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

// Load Sentinel-2 collection
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month')) // May–Sep
  .map(maskS2clouds);

print('Sentinel-2 collection size:', s2.size());

// Bands to process
var bands = [
  {name: 'B2', description: 'Blue', scale: 10},
  {name: 'B3', description: 'Green', scale: 10},
  {name: 'B4', description: 'Red', scale: 10},
  {name: 'B8', description: 'NIR', scale: 10},
  {name: 'B11', description: 'SWIR1', scale: 10}  // Will be resampled from 20m
];

// Process each band
bands.forEach(function(band) {
  var composite = s2
    .select(band.name)
    .mean()
    .clip(region)  // Explicitly clip to region
    .reproject({
      crs: 'EPSG:5070',
      scale: band.scale
    });
  
  // Visualization (adjust min/max for each band type)
  var visParams = band.name === 'B11' 
    ? {min: 500, max: 3000, palette: ['black', 'white']}
    : {min: 0, max: 3000, palette: ['black', 'white']};
  
  Map.addLayer(composite, visParams, 'S2 ' + band.description + ' Mean', false);
  
  // Export to Drive
  Export.image.toDrive({
    image: composite,
    description: 'S2_' + band.name + '_10m_2020_2024',
    folder: 'NEFIN_FIA_Covariates',
    fileNamePrefix: 'S2_' + band.name + '_10m_2020_2024_NE',
    region: region,
    scale: band.scale,
    crs: 'EPSG:4326',  // WGS84 to match your pipeline
    maxPixels: 1e13
  });
  
  print('Exported: S2_' + band.name + '_10m_2020_2024');
});
