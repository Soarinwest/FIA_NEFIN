// ===========================================================
// DEM DERIVATIVES: USGS NED 10m
// Layers: Elevation, Slope, Aspect, TPI
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 10m (native)
// ===========================================================

var states = ee.FeatureCollection('TIGER/2018/States');
var stateFips = ['23', '33', '50', '36', '25', '09', '44'];

var region = states
  .filter(ee.Filter.inList('STATEFP', stateFips))
  .geometry()
  .dissolve();

Map.centerObject(region, 6);
Map.addLayer(region, {color: 'red'}, 'NE Region', false);

// ===================================
// LOAD DEM
// ===================================

// USGS NED 10m (1/3 arc-second) - best available for CONUS
var ned = ee.Image('USGS/NED');

// Clip to region
var elevation = ned.clip(region).rename('elevation');

print('DEM loaded - USGS NED 10m');

// ===================================
// CALCULATE DERIVATIVES
// ===================================

// Slope (degrees)
var slope = ee.Terrain.slope(elevation).rename('slope');

// Aspect (degrees, 0-360)
var aspect = ee.Terrain.aspect(elevation).rename('aspect');

// Topographic Position Index (TPI)
// TPI = elevation - mean(neighborhood elevation)
// Positive = ridges/hilltops, Negative = valleys
var tpi = elevation.subtract(elevation.focalMean({
  radius: 100,  // 100m radius (~10 pixels)
  units: 'meters',
  kernelType: 'circle'
})).rename('TPI');

// ===================================
// VISUALIZATION
// ===================================

Map.addLayer(elevation, 
  {min: 0, max: 1500, palette: ['green', 'yellow', 'brown', 'white']}, 
  'Elevation', false);

Map.addLayer(slope, 
  {min: 0, max: 45, palette: ['white', 'yellow', 'red']}, 
  'Slope', false);

Map.addLayer(aspect, 
  {min: 0, max: 360, palette: ['blue', 'green', 'red', 'blue']}, 
  'Aspect', false);

Map.addLayer(tpi, 
  {min: -50, max: 50, palette: ['blue', 'white', 'red']}, 
  'TPI', false);

// ===================================
// EXPORT
// ===================================

// Elevation
Export.image.toDrive({
  image: elevation,
  description: 'elevation_10m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'elevation_10m_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Slope
Export.image.toDrive({
  image: slope,
  description: 'slope_10m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'slope_10m_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Aspect
Export.image.toDrive({
  image: aspect,
  description: 'aspect_10m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'aspect_10m_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// TPI (Optional)
Export.image.toDrive({
  image: tpi,
  description: 'tpi_10m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'tpi_10m_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('All DEM derivatives queued for export');

// ===================================
// BONUS: Aggregate to 250m for MODIS comparison
// ===================================

// Mean elevation at 250m
var elevation_250m = elevation.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: elevation.projection(),
  scale: 250
});

// Mean slope at 250m
var slope_250m = slope.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({
  crs: elevation.projection(),
  scale: 250
});

// Circular mean for aspect (can't just average degrees!)
// Convert to unit vectors, average, convert back
var aspectRad = aspect.multiply(Math.PI / 180);
var aspectX = aspectRad.cos();
var aspectY = aspectRad.sin();

var aspectX_250m = aspectX.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({crs: elevation.projection(), scale: 250});

var aspectY_250m = aspectY.reduceResolution({
  reducer: ee.Reducer.mean(),
  maxPixels: 1024
}).reproject({crs: elevation.projection(), scale: 250});

var aspect_250m = aspectY_250m.atan2(aspectX_250m)
  .multiply(180 / Math.PI)
  .add(180)  // Convert from -180:180 to 0:360
  .rename('aspect');

// Export 250m aggregated layers
Export.image.toDrive({
  image: elevation_250m,
  description: 'elevation_250m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'elevation_250m_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: slope_250m,
  description: 'slope_250m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'slope_250m_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: aspect_250m,
  description: 'aspect_250m',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'aspect_250m_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('250m aggregated layers also queued for export');
