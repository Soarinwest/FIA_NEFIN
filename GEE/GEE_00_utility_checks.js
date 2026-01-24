// ===========================================================
// GEE UTILITY: Check Export Status & Data Verification
// Purpose: Monitor processing and verify layer characteristics
// ===========================================================

// ===========================================
// SECTION 1: Check Your Region
// ===========================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry()
  .dissolve();

// Print region stats
print('Study Region:', region);
print('Region Area (sq km):', region.area().divide(1e6));
print('Region Bounds:', region.bounds());

// Visualize
Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Study Region');

// ===========================================
// SECTION 2: Quick Data Availability Check
// ===========================================

// Check Sentinel-2 availability
var s2Check = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'));

print('S2 Images Available (2020-2024):', s2Check.size());
print('Date Range:', s2Check.aggregate_min('system:time_start'), 
      'to', s2Check.aggregate_max('system:time_start'));

// Check MODIS availability
var modisCheck = ee.ImageCollection('MODIS/061/MOD13Q1')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'));

print('MODIS Images Available (2020-2024):', modisCheck.size());

// ===========================================
// SECTION 3: Verify DEM Coverage
// ===========================================

var ned = ee.Image('USGS/NED');
var demClip = ned.clip(region);

// Check for gaps
var demStats = demClip.reduceRegion({
  reducer: ee.Reducer.minMax(),
  geometry: region,
  scale: 1000,
  maxPixels: 1e9
});

print('DEM Min Elevation (m):', demStats.get('elevation_min'));
print('DEM Max Elevation (m):', demStats.get('elevation_max'));

Map.addLayer(demClip, 
  {min: 0, max: 1500, palette: ['green', 'yellow', 'brown', 'white']}, 
  'DEM Coverage', false);

// ===========================================
// SECTION 4: Sample Points Test
// ===========================================

// Test extraction at sample points (like you'll do in R)
var samplePoints = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([-72.5, 43.5]), {name: 'Test_VT'}),
  ee.Feature(ee.Geometry.Point([-71.5, 44.5]), {name: 'Test_NH'}),
  ee.Feature(ee.Geometry.Point([-69.0, 45.5]), {name: 'Test_ME'})
]);

Map.addLayer(samplePoints, {color: 'blue'}, 'Test Points');

// Quick NDVI extraction test
var s2_test = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'))
  .map(function(img) {
    var scl = img.select('SCL');
    var mask = scl.eq(4).or(scl.eq(5)).or(scl.eq(6));
    var ndvi = img.normalizedDifference(['B8', 'B4']).rename('NDVI');
    return img.addBands(ndvi).updateMask(mask);
  })
  .select('NDVI')
  .mean();

var testExtraction = s2_test.reduceRegions({
  collection: samplePoints,
  reducer: ee.Reducer.first(),
  scale: 10
});

print('Test Point Extraction:', testExtraction);

// ===========================================
// SECTION 5: Processing Status Summary
// ===========================================

print('========================================');
print('PROCESSING CHECKLIST');
print('========================================');
print('');
print('✓ Check these before exporting:');
print('  - Region loads correctly (red polygon)');
print('  - S2 collection has 100+ images');
print('  - MODIS collection has 40+ images');
print('  - DEM covers full region');
print('  - Test extraction shows valid values');
print('');
print('Expected outputs:');
print('  - Sentinel-2: 9 layers @ 10m');
print('  - MODIS: 8 layers @ 250m');
print('  - DEM: 7 layers (4 @ 10m, 3 @ 250m)');
print('  - LST: 3 layers @ 1km (optional)');
print('  - TOTAL: ~27 files');
print('');
print('========================================');

// ===========================================
// SECTION 6: Export Status Checker
// ===========================================

// This can't check actual task status, but shows expected exports
var exportList = [
  // Sentinel-2
  'S2_B2_10m_2020_2024',
  'S2_B3_10m_2020_2024',
  'S2_B4_10m_2020_2024',
  'S2_B8_10m_2020_2024',
  'S2_B11_10m_2020_2024',
  'S2_EVI_10m_2020_2024',
  'S2_NBR_10m_2020_2024',
  'S2_NDWI_10m_2020_2024',
  'S2_NDVI_SD_10m_2020_2024',
  
  // MODIS
  'MODIS_RED_250m_2020_2024',
  'MODIS_NIR_250m_2020_2024',
  'MODIS_BLUE_250m_2020_2024',
  'MODIS_GREEN_250m_2020_2024',
  'MODIS_SWIR1_250m_2020_2024',
  'MODIS_EVI_250m_2020_2024',
  'MODIS_NBR_250m_2020_2024',
  'MODIS_NDWI_250m_2020_2024',
  
  // DEM
  'elevation_10m',
  'slope_10m',
  'aspect_10m',
  'tpi_10m',
  'elevation_250m',
  'slope_250m',
  'aspect_250m',
  
  // LST (optional)
  'MODIS_LST_day_1km_2020_2024',
  'MODIS_LST_night_1km_2020_2024',
  'MODIS_LST_range_1km_2020_2024'
];

print('Expected Exports:', exportList.length, 'files');
exportList.forEach(function(name, index) {
  print('  ' + (index + 1) + '. ' + name);
});

print('');
print('To check actual export status:');
print('  1. Click "Tasks" tab (top-right)');
print('  2. Look for RUNNING/COMPLETED/FAILED');
print('  3. Check Google Drive folder: NEFIN_FIA_Covariates');

// ===========================================
// SECTION 7: Data Quality Indicators
// ===========================================

// Calculate temporal coverage statistics
var s2_monthly = ee.List.sequence(2020, 2024).map(function(year) {
  year = ee.Number(year);
  return ee.List.sequence(5, 9).map(function(month) {
    var start = ee.Date.fromYMD(year, ee.Number(month), 1);
    var end = start.advance(1, 'month');
    var count = s2Check.filterDate(start, end).size();
    return ee.Dictionary({
      year: year,
      month: month,
      count: count
    });
  });
}).flatten();

print('S2 Monthly Coverage Sample:', s2_monthly.slice(0, 5));

// ===========================================
// SECTION 8: Coordinate System Verification
// ===========================================

print('');
print('========================================');
print('COORDINATE SYSTEM INFO');
print('========================================');
print('');
print('Export CRS: EPSG:4326 (WGS84)');
print('S2 Processing CRS: EPSG:5070 (Albers Equal Area)');
print('MODIS Native CRS: Sinusoidal');
print('');
print('All layers will be in WGS84 (lat/lon) for R extraction!');
print('');

// ===========================================
// SECTION 9: Next Steps
// ===========================================

print('========================================');
print('NEXT STEPS AFTER EXPORTS COMPLETE');
print('========================================');
print('');
print('1. Download all files from Google Drive');
print('2. Organize into local directory structure');
print('3. Verify file sizes (S2 files ~500MB-2GB each)');
print('4. Check raster properties in R/QGIS:');
print('   - CRS = EPSG:4326');
print('   - Resolution = 10m, 250m, or 1km');
print('   - Extent = NE US bounding box');
print('5. Run extraction scripts to match plot coordinates');
print('');
print('R extraction workflow:');
print('  library(terra)');
print('  r <- rast("S2_NDVI_10m_2020_2024_NE.tif")');
print('  plots <- vect("plot_coordinates.shp")');
print('  values <- extract(r, plots)');
print('');
print('========================================');
