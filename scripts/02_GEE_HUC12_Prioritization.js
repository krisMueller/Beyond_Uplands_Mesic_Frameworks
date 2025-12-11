/**
 * =================================================================================
 * SCRIPT: 02_GEE_HUC12_Prioritization.js
 * MANUSCRIPT: Beyond Uplands: Integrating Riparian Areas into Sagebrush Conservation Design
 * 
 * DESCRIPTION:
 * This script aggregates the specific ecological and ownership metrics required 
 * to classify watersheds in the subsequent R analysis.
 * 
 * DATA AVAILABILITY NOTE:
 * This script references assets hosted in private Google Earth Engine repositories 
 * (projects/wlfw-um/... and projects/ee-krismueller134/...). These paths are 
 * preserved to document the exact methodology. Users wishing to reproduce this 
 * analysis must upload their own VBET and Sagebrush Conservation Design data 
 * and update the asset paths accordingly.
 * =================================================================================
 */

// =================================================================================
//  1. DATA IMPORT & DGO PRE-PROCESSING
// =================================================================================

// ------------------------------------------------
//  Import DGO Features (Valley Bottom Segments)
// ------------------------------------------------
{
  var basePath = 'projects/wlfw-um/assets/mesic/dgoMetricsOutput_processed/';
  var assetSuffixes = [
    'processed_grid_cell_0', 'processed_grid_cell_1', 'processed_grid_cell_2',
    'processed_grid_cell_3', 'processed_grid_cell_4', 'processed_grid_cell_5',
    'processed_grid_cell_6', 'processed_grid_cell_7', 'processed_grid_cell_8'
  ];

  var mergedCollection = ee.FeatureCollection(assetSuffixes.map(function(suffix) {
    return ee.FeatureCollection(basePath + suffix);
  })).flatten();

  // Filter out persistent water bodies (Water proportion > 0.25)
  var filteredCollection = mergedCollection.map(function(feat) {
    var segmentArea = ee.Number(feat.get('segment_area'));
    var persistentWaterArea = ee.Number(feat.get('sum_persistentWater_area_m2'));
    var propWater = ee.Algorithms.If(
      persistentWaterArea,
      persistentWaterArea.divide(segmentArea),
      0
    );
    return feat.set({ 'prop_persistent_water': propWater });
  });

  filteredCollection = filteredCollection.filter(ee.Filter.lt('prop_persistent_water', 0.25));
}

// ------------------------------------------------
//  Create Base Masks
// ------------------------------------------------
var nass = ee.ImageCollection("USDA/NASS/CDL");

// Non-Developed Mask
var nonDevelopedMask = nass.select('cropland')
  .map(function(image) {
    var developed = image.eq(82).or(image.gte(121).and(image.lte(124)));
    return developed;
  })
  .mosaic().unmask(0).eq(0);

// Non-Cultivated Mask
var cropLandcoverMask = nass
  .sort('system:time_start', false)
  .filter(ee.Filter.neq('system:index', '2024'))
  .limit(11)
  .map(function(image) {return image.select('cultivated').eq(2);})
  .sum().gt(0).eq(0);

// DGO (Valley Bottom) Mask
var dgoMask = ee.Image().paint(filteredCollection, 1).rename('isDgo').reproject('EPSG:4326', null, 5);


// =================================================================================
//  2. VEGETATION PERSISTENCE LAYERS
// =================================================================================

var ndviCollection = ee.ImageCollection("projects/ee-krismueller134/assets/Mesic/FinalLayers/PeriodMeanNdviCollection")
  .filter(ee.Filter.neq('year', 2012));

var standardizedNdviCollection = ndviCollection.map(function(image) {
  return image.divide(10000).toFloat();
});

var collectionSize = standardizedNdviCollection.size();
var vegPersistencePercent = standardizedNdviCollection
  .map(function(img){ return img.gte(0.3); }) 
  .sum()
  .multiply(100.0)
  .divide(collectionSize)
  .updateMask(nonDevelopedMask.eq(1))
  .updateMask(cropLandcoverMask.eq(1));

// Classification: 1=Low, 2=Med, 3=High (0 is ignored as R script does not use it)
var persistenceClass = ee.Image(0)
  .where(vegPersistencePercent.eq(0), 0)                                         
  .where(vegPersistencePercent.gt(0).and(vegPersistencePercent.lt(25)), 1)    
  .where(vegPersistencePercent.gte(25).and(vegPersistencePercent.lt(75)), 2)   
  .where(vegPersistencePercent.gte(75), 3)                                       
  .unmask(0)
  .updateMask(dgoMask.eq(1))
  .updateMask(nonDevelopedMask.eq(1))
  .updateMask(cropLandcoverMask.eq(1))
  .reproject('EPSG:4326', null, 5)
  .rename('persistenceClass');


// =================================================================================
//  3. SAGEBRUSH CONSERVATION DESIGN & LANDSCAPE CONTEXT
// =================================================================================

var scd = ee.ImageCollection('projects/wlfw-um/assets/scd/SEIv11');
// R script only uses Core (1) and Growth (2) from 2020
var scd2020Core = scd.filter(ee.Filter.eq('year', '2020')).first().select([11]).eq(1).unmask(0);
var scd2020Growth = scd.filter(ee.Filter.eq('year', '2020')).first().select([11]).eq(2).unmask(0);

// Helper function for 5km buffer masks
function createBufferMask(sourceImage, maskDgo, maskDev, maskCrop) {
  var dist = sourceImage.fastDistanceTransform(512).sqrt().multiply(ee.Image.pixelArea().sqrt());
  return dist.lte(5000)
    .unmask(0)
    .updateMask(maskDgo.eq(1))
    .updateMask(maskDev.eq(1))
    .updateMask(maskCrop.eq(1))
    .reproject('EPSG:4326', null, 5);
}

var isWithin5km2020Core = createBufferMask(scd2020Core, dgoMask, nonDevelopedMask, cropLandcoverMask).rename('isWithin5km2020Core');
var isWithin5km2020Growth = createBufferMask(scd2020Growth, dgoMask, nonDevelopedMask, cropLandcoverMask).rename('isWithin5km2020Growth');

// Ownership (Public vs Private)
var padus = ee.FeatureCollection("projects/ee-krismueller134/assets/Misc/biome_clipped_landOwnership_2025");
var ownNames = ['Bureau of Land Management', 'Forest Service', 'U.S. Fish and Wildlife Service', 'National Park Service', 'American Indian Lands', 'Non-Governmental Organization', 'State Land Board', 'State Department of Land', 'State Fish and Wildlife', 'Other or Unknown State Land'];
var publicImg = ee.Image().paint(padus.filter(ee.Filter.inList('MngNm_Desc', ownNames)), 1).rename('isPublic').unmask();

var privateLands = publicImg.eq(0).rename('isPrivate').updateMask(dgoMask.eq(1)).reproject('EPSG:4326', null, 5);
var publicLands = publicImg.eq(1).rename('isPublic').updateMask(dgoMask.eq(1)).reproject('EPSG:4326', null, 5);


// =================================================================================
//  4. HUC12 AGGREGATION & METRIC CALCULATION
// =================================================================================

var HUC12 = ee.FeatureCollection("projects/ee-krismueller134/assets/Mesic/Misc/sagebrushBiome_clipped_huc12_watersheds");
var pixelArea = ee.Image.pixelArea();

// Define Images for Reduction (Only those used in R)
var allAreaImages = ee.Image.cat([
  dgoMask.eq(1).multiply(pixelArea).rename('area_ValleyBottom_m2'),
  persistenceClass.eq(1).multiply(pixelArea).rename('areaValley_PersistenceClassLow_m2'),
  persistenceClass.eq(2).multiply(pixelArea).rename('areaValley_PersistenceClassMed_m2'),
  persistenceClass.eq(3).multiply(pixelArea).rename('areaValley_PersistenceClassHigh_m2'),
  isWithin5km2020Core.eq(1).multiply(pixelArea).rename('areaValley_Within5kmCore_m2'),
  isWithin5km2020Growth.eq(1).multiply(pixelArea).rename('areaValley_Within5kmGrowth_m2'),
  publicLands.eq(1).multiply(pixelArea).rename('areaValley_publicLands_m2'),
  privateLands.eq(1).multiply(pixelArea).rename('areaValley_privateLands_m2')
]);

// Run Reduce Regions
var huc12AreaSums = allAreaImages.reduceRegions({
  collection: HUC12,
  reducer: ee.Reducer.sum(),
  scale: 5 // Matches DGO reproject scale
});

// Calculate Proportions
var huc12Metrics = huc12AreaSums.map(function(feature) {
  var areaValleyBottom = ee.Number(feature.get('area_ValleyBottom_m2'));
  
  // Helpers for simple proportions (Area of X / Area of Valley Bottom)
  var getValleyProp = function(key) {
    return ee.Algorithms.If(areaValleyBottom.gt(0), ee.Number(feature.get(key)).divide(areaValleyBottom), 0);
  };
  
  return feature.set({
    'propValley_PersistenceClassLow': getValleyProp('areaValley_PersistenceClassLow_m2'),
    'propValley_PersistenceClassMed': getValleyProp('areaValley_PersistenceClassMed_m2'),
    'propValley_PersistenceClassHigh': getValleyProp('areaValley_PersistenceClassHigh_m2'),
    'propValley_Within5kmCore': getValleyProp('areaValley_Within5kmCore_m2'),
    'propValley_Within5kmGrowth': getValleyProp('areaValley_Within5kmGrowth_m2'),
    'propValley_PublicLands': getValleyProp('areaValley_publicLands_m2'),
    'propValley_PrivateLands': getValleyProp('areaValley_privateLands_m2')
  });
});


// =================================================================================
//  5. EXPORT
// =================================================================================

// Select only the properties required by the R script
// Renamed to meet Shapefile 10-character header limit

var oldPropertyNamesForShp = [
  'name',
  'area_ValleyBottom_m2',            // R: ArVBm2
  'propValley_PersistenceClassLow',  // R: PrVPclL
  'propValley_PersistenceClassMed',  // R: PrVPclM
  'propValley_PersistenceClassHigh', // R: PrVPclH
  'propValley_Within5kmCore',        // R: PrVW5kC
  'propValley_Within5kmGrowth',      // R: PrVW5kG
  'propValley_PublicLands',          // R: PrVPu
  'propValley_PrivateLands'          // R: PrVPr
];

var newPropertyNamesForShp = [
  'name',
  'ArVBm2',
  'PrVPclL',
  'PrVPclM',
  'PrVPclH',
  'PrVW5kC',
  'PrVW5kG',
  'PrVPu',
  'PrVPr'
];

var huc12MetricsForShp = huc12Metrics.select({
  propertySelectors: oldPropertyNamesForShp,
  newProperties: newPropertyNamesForShp,
  retainGeometry: true
});

Export.table.toDrive({
  collection: huc12MetricsForShp,
  description: 'YYYYMMDD_HUC12_DGO_proportions_Metrics_SHP', 
  folder: "YOUR_FOLDER",
  fileFormat: 'SHP'
});