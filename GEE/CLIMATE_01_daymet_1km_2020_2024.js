// ===========================================================
// DAYMET CLIMATE DATA: 2020–2024 MEAN
// Variables: Tmin, Tmax, Tmean, Precipitation
// Product: Daymet V4 (1km daily → aggregated)
// Region: NE US (ME, NH, VT, NY, MA, CT, RI)
// Resolution: 1km (native)
// Purpose: High-resolution climate for fine-scale modeling
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
// LOAD DAYMET V4
// ===================================

var daymet = ee.ImageCollection('NASA/ORNL/DAYMET_V4');

print('Daymet V4 collection loaded');

// ===================================
// FILTER TO STUDY PERIOD & GROWING SEASON
// ===================================

// Filter to 2020-2024 growing season (May-September)
var daymetFiltered = daymet
  .filterBounds(region)
  .filterDate('2020-01-01', '2024-12-31')
  .filter(ee.Filter.calendarRange(5, 9, 'month'));  // May-Sep

print('Daymet images (2020-2024, May-Sep):', daymetFiltered.size());

// ===================================
// CALCULATE MEAN CLIMATE VARIABLES
// ===================================

// Temperature minimum (°C)
var tmin = daymetFiltered
  .select('tmin')
  .mean()
  .clip(region)
  .rename('tmin_mean');

// Temperature maximum (°C)
var tmax = daymetFiltered
  .select('tmax')
  .mean()
  .clip(region)
  .rename('tmax_mean');

// Temperature mean (average of daily min and max)
var tmean = tmin.add(tmax).divide(2).rename('tmean');

// Precipitation (mm/day)
var prcp = daymetFiltered
  .select('prcp')
  .mean()
  .clip(region)
  .rename('prcp_mean');

// Total growing season precipitation (sum over May-Sep, averaged across years)
// This is more ecologically relevant than daily mean
var prcpTotal = daymetFiltered
  .select('prcp')
  .reduce(ee.Reducer.sum())  // Sum per year
  .clip(region)
  .rename('prcp_total_gs');

print('Climate variables calculated');

// ===================================
// VISUALIZATION
// ===================================

Map.addLayer(tmin, 
  {min: 5, max: 20, palette: ['blue', 'cyan', 'yellow', 'orange', 'red']}, 
  'Tmin Mean (°C)', false);

Map.addLayer(tmax, 
  {min: 15, max: 30, palette: ['blue', 'cyan', 'yellow', 'orange', 'red']}, 
  'Tmax Mean (°C)', false);

Map.addLayer(tmean, 
  {min: 10, max: 25, palette: ['blue', 'cyan', 'yellow', 'orange', 'red']}, 
  'Tmean (°C)', true);

Map.addLayer(prcp, 
  {min: 2, max: 5, palette: ['white', 'lightblue', 'blue', 'darkblue']}, 
  'Precip Mean (mm/day)', false);

Map.addLayer(prcpTotal, 
  {min: 400, max: 800, palette: ['white', 'lightblue', 'blue', 'darkblue']}, 
  'Precip Total GS (mm)', false);

// ===================================
// EXPORT AT NATIVE 1KM RESOLUTION
// ===================================

// Export Tmin
Export.image.toDrive({
  image: tmin,
  description: 'Daymet_tmin_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmin_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Export Tmax
Export.image.toDrive({
  image: tmax,
  description: 'Daymet_tmax_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmax_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Export Tmean
Export.image.toDrive({
  image: tmean,
  description: 'Daymet_tmean_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmean_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

// Export daily precipitation mean
Export.image.toDrive({
  image: prcp,
  description: 'Daymet_prcp_1km_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_prcp_1km_2020_2024_NE',
  region: region,
  scale: 1000,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: Daymet 1km climate layers (4 files)');

// ===================================
// RESAMPLE TO 250M FOR MODIS MODELS
// ===================================

// For better integration with 250m MODIS data
// Use bilinear interpolation for smooth climate surfaces

var tmin_250m = tmin
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 250
  })
  .clip(region);  // ← Clip to region for smaller exports

var tmax_250m = tmax
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 250
  })
  .clip(region);  // ← Clip to region

var tmean_250m = tmean
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 250
  })
  .clip(region);  // ← Clip to region

var prcp_250m = prcp
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 250
  })
  .clip(region);  // ← Clip to region

// Export 250m versions
Export.image.toDrive({
  image: tmin_250m,
  description: 'Daymet_tmin_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmin_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: tmax_250m,
  description: 'Daymet_tmax_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmax_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: tmean_250m,
  description: 'Daymet_tmean_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmean_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: prcp_250m,
  description: 'Daymet_prcp_250m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_prcp_250m_2020_2024_NE',
  region: region,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: Daymet 250m resampled layers (4 files)');
print('NOTE: 250m = 250 METERS, all clipped to NE region');

// ===================================
// EXPORT AT 10M FOR SENTINEL-2 MODELS
// ===================================

// Note: This is bilinear interpolation from 1km
// Files will be large (~2-4 GB each) but provides spatially-smooth climate

var tmin_10m = tmin
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 10
  })
  .clip(region);  // ← Clip to region for smaller exports

var tmax_10m = tmax
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 10
  })
  .clip(region);  // ← Clip to region

var tmean_10m = tmean
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 10
  })
  .clip(region);  // ← Clip to region

var prcp_10m = prcp
  .resample('bilinear')
  .reproject({
    crs: 'EPSG:4326',
    scale: 10
  })
  .clip(region);  // ← Clip to region

// Export 10m versions
Export.image.toDrive({
  image: tmin_10m,
  description: 'Daymet_tmin_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmin_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: tmax_10m,
  description: 'Daymet_tmax_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmax_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: tmean_10m,
  description: 'Daymet_tmean_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_tmean_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

Export.image.toDrive({
  image: prcp_10m,
  description: 'Daymet_prcp_10m_2020_2024',
  folder: 'NEFIN_FIA_Covariates',
  fileNamePrefix: 'Daymet_prcp_10m_2020_2024_NE',
  region: region,
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});

print('Exported: Daymet 10m resampled layers (4 files)');
print('NOTE: 10m = 10 METERS, all clipped to NE region');
print('NOTE: 10m files will be large (~2-4 GB each)');
print('Total 10m climate export: ~8-16 GB');

// ===================================
// SUMMARY STATISTICS
// ===================================

var stats = tmean.reduceRegion({
  reducer: ee.Reducer.minMax().combine({
    reducer2: ee.Reducer.mean(),
    sharedInputs: true
  }),
  geometry: region,
  scale: 1000,
  maxPixels: 1e9
});

print('Temperature statistics (°C):', stats);

print('');
print('========================================');
print('DAYMET CLIMATE SUMMARY');
print('========================================');
print('');
print('Product: Daymet V4 Daily Surface Weather');
print('Resolution: 1km (1000 meters) native');
print('Time Period: 2020-2024 (5 years)');
print('Season: May-September (growing season)');
print('');
print('Variables:');
print('  - Tmin: Minimum temperature (°C)');
print('  - Tmax: Maximum temperature (°C)');
print('  - Tmean: Mean temperature (°C)');
print('  - Precip: Precipitation (mm/day)');
print('');
print('Exports (ALL CLIPPED TO NE STATES):');
print('  Native 1km: 4 files (~20-40 MB each)');
print('  Resampled 250m (250 METERS): 4 files (~100-200 MB each)');
print('  Resampled 10m (10 METERS): 4 files (~2-4 GB each)');
print('  Total: 12 files (~8-13 GB)');
print('');
print('WHY DAYMET > PRISM:');
print('  - 1km resolution vs 4km (4x finer!)');
print('  - Daily data → better temporal aggregation');
print('  - Designed for ecological applications');
print('  - Better captures local climate variation');
print('');
print('RESOLUTION NOTES:');
print('  - 250m = 250 METERS (for MODIS models)');
print('  - 10m = 10 METERS (for Sentinel-2 models)');
print('  - Both resampled from 1km using bilinear interpolation');
print('  - All exports clipped to NE states for efficiency');
print('');
print('========================================');