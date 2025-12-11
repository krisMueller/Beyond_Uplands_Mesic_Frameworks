/**
 * =================================================================================
 * SCRIPT: 01_GEE_Biome_States.js
 * MANUSCRIPT: Beyond Uplands: Integrating Riparian Areas into Sagebrush Conservation Design
 * 
 * DESCRIPTION:
 * This script calculates vegetation persistence statistics across user-defined 
 * ecoregions (Great Basin, Rocky Mountains, Northern Great Plains) and the 
 * entire Sagebrush Biome. It utilizes Valley Bottom Extraction Tool (VBET) outputs
 * and remote sensing indices to classify mesic persistence.
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
//  1. CONFIGURATION & ECOREGION DEFINITIONS
// =================================================================================
var ecoregions = ee.FeatureCollection("projects/ee-krismueller134/assets/Mesic/Misc/biome_clipped_reduced_eight_ecoregions")

var greatBasin = ["Northern Basin and Range", "Columbia Plateau"];
var rockyMountains = ["Canadian Rockies", "Wyoming Basin", "Wasatch and Uinta Mountains", "Colorado Plateaus", "Columbia Mountains/Northern Rockies"];
var northernGreatPlains = "Northwestern Glaciated Plains";

// =================================================================================
//  2. DATA IMPORT & PRE-PROCESSING
// =================================================================================

// ------------------------------------------------
//  Import Discrete Geographic Objects (DGOs)
// ------------------------------------------------
// NOTE: This block imports and merges pre-processed VBET segments.
// The resulting 'filteredCollection' is used to create the DGO mask.

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

  // Filter collection to remove persistent water bodies
  var filteredCollection = mergedCollection.map(function(feat) {
    var segmentArea = ee.Number(feat.get('segment_area'));
    var persistentWaterArea = ee.Number(feat.get('sum_persistentWater_area_m2'));
    
    var propWater = ee.Algorithms.If(
      persistentWaterArea,
      persistentWaterArea.divide(segmentArea),
      0
    );

    return feat.set({
      'prop_persistent_water': propWater
    });
  });

  // Apply final filter (Water proportion < 0.25)
  filteredCollection = filteredCollection.filter(
      ee.Filter.and(ee.Filter.lt('prop_persistent_water', 0.25))
  );
}

// ------------------------------------------------
//  Create Masks (Development, Cropland, DGO)
// ------------------------------------------------

var nass = ee.ImageCollection("USDA/NASS/CDL");

// Non-Developed Area Mask
var nonDevelopedMask = nass.select('cropland')
  .map(function(image) {
    var developed = image.eq(82).or(image.gte(121).and(image.lte(124)));
    return developed;
  })
  .mosaic().unmask(0).eq(0);

// Non-Cultivated Mask (11-year history)
var cropLandcoverMask = nass
  .sort('system:time_start', false)
  .filter(ee.Filter.neq('system:index', '2024'))
  .limit(11) 
  .map(function(image) {return image.select('cultivated').eq(2);})
  .sum()
  .gt(0) 
  .eq(0);

// DGO Outline Mask (Valley Bottoms)
var dgoMask = ee.Image().paint(filteredCollection, 1).rename('isDgo')
                                                     .reproject('EPSG:4326', null, 5);

// =================================================================================
//  3. PERSISTENCE CLASSIFICATION
// =================================================================================

// Green Vegetation Mask (NDVI >= 0.3)
var ndviCollection = ee.ImageCollection("projects/ee-krismueller134/assets/Mesic/FinalLayers/PeriodMeanNdviCollection")
  .filter(ee.Filter.neq('year', 2012));

var standardizedNdviCollection = ndviCollection.map(function(image) {
  return image.divide(10000).toFloat();
});

// Calculate Persistence Percentage
var collectionSize = standardizedNdviCollection.size();
var vegPersistencePercent = standardizedNdviCollection
  .map(function(img){ return img.gte(0.3); }) 
  .sum() 
  .multiply(100.0) 
  .divide(collectionSize) 
  .updateMask(nonDevelopedMask.eq(1))
  .updateMask(cropLandcoverMask.eq(1));

// Create Classified Layer (0=None, 1=Low, 2=Med, 3=High)
var persistenceClass = ee.Image(0)
  .where(vegPersistencePercent.eq(0), 0)                                         
  .where(vegPersistencePercent.gt(0).and(vegPersistencePercent.lt(25)), 1)    
  .where(vegPersistencePercent.gte(25).and(vegPersistencePercent.lt(75)), 2)   
  .where(vegPersistencePercent.gte(75), 3)                                       
  .unmask(0)
  .updateMask(dgoMask.eq(1))
  .updateMask(nonDevelopedMask.eq(1))
  .updateMask(cropLandcoverMask.eq(1))
  .rename('persistenceClass');

Map.addLayer(persistenceClass, {min: 0, max: 3, palette: ['black', 'lightgreen', 'green', 'darkgreen']}, 'Vegetation Persistence Class', true);

// =================================================================================
//  4. SAGEBRUSH CONSERVATION DESIGN (SCD) INTEGRATION
// =================================================================================

var scd = ee.ImageCollection('projects/wlfw-um/assets/scd/SEIv11');

// Define Core and Growth Opportunity Areas (2020)
var scd2020Core = scd.filter(ee.Filter.eq('year', '2020')).first().select([11]).eq(1).unmask(0);
var scd2020Growth = scd.filter(ee.Filter.eq('year', '2020')).first().select([11]).eq(2).unmask(0);
var uplands = scd2020Core.add(scd2020Growth).gte(1).unmask(0);

// Calculate 5km Buffer
var uplandsDist = uplands.fastDistanceTransform(512).sqrt().multiply(ee.Image.pixelArea().sqrt());
var isWithin5kmUplands = uplandsDist.lte(5000).unmask(0);

// =================================================================================
//  5. REGIONAL ANALYSIS
// =================================================================================

// Load Geometries
var ecoregions = ee.FeatureCollection("projects/ee-krismueller134/assets/Mesic/Misc/biome_clipped_reduced_eight_ecoregions");
var sagebrushBiome = ee.FeatureCollection("projects/ee-krismueller134/assets/Misc/Sagebrush_Biome");
var sagebrushBiomeGeom = sagebrushBiome.geometry().dissolve();

var greatBasinGeom = ecoregions.filter(ee.Filter.inList('na_l3name', greatBasin)).geometry().dissolve();
var rockyMountainsGeom = ecoregions.filter(ee.Filter.inList('na_l3name', rockyMountains)).geometry().dissolve();
var northernGreatPlainsGeom = ecoregions.filter(ee.Filter.eq('na_l3name', northernGreatPlains)).geometry().dissolve();

// Visualization
Map.addLayer(sagebrushBiomeGeom, {color: 'cyan'}, 'Sagebrush Biome', false);
Map.addLayer(greatBasinGeom, {color: 'yellow'}, 'Great Basin Ecoregion', false);

// Prepare Analysis Mask
var pixelArea = ee.Image.pixelArea();
var M2_TO_ACRES = 0.0002471054;
var scale = 30; 
var analysisMask = dgoMask.eq(1);

// Create Multi-band Image for Area Calculation
var areaImage = ee.Image.cat([
  analysisMask, 
  persistenceClass.eq(0),
  persistenceClass.eq(1),
  persistenceClass.eq(2),
  persistenceClass.eq(3),
  persistenceClass.eq(0).updateMask(isWithin5kmUplands),
  persistenceClass.eq(1).updateMask(isWithin5kmUplands),
  persistenceClass.eq(2).updateMask(isWithin5kmUplands),
  persistenceClass.eq(3).updateMask(isWithin5kmUplands)
]).multiply(pixelArea).multiply(M2_TO_ACRES)
.rename([
  'total_valley_bottom_acres',
  'class_0_acres', 'class_1_acres', 'class_2_acres', 'class_3_acres',
  'class_0_near_uplands_acres', 'class_1_near_uplands_acres', 
  'class_2_near_uplands_acres', 'class_3_near_uplands_acres'
]);

// Run Analysis Regions
var analysisRegions = ee.FeatureCollection([
  ee.Feature(sagebrushBiomeGeom, {'ecoregion': 'Total Sagebrush Biome'}),
  ee.Feature(greatBasinGeom, {'ecoregion': 'Great Basin'}),
  ee.Feature(rockyMountainsGeom, {'ecoregion': 'Rocky Mountains Combined'}),
  ee.Feature(northernGreatPlainsGeom, {'ecoregion': 'Northern Great Plains'})
]);

var statsByRegion = areaImage.reduceRegions({
  collection: analysisRegions,
  reducer: ee.Reducer.sum(),
  scale: scale,
  tileScale: 4 
});

// Calculate Proportions
var finalTable = statsByRegion.map(function(feature) {
  var totalArea = ee.Number(feature.get('total_valley_bottom_acres'));
  var getProportion = function(areaProperty) {
    var area = ee.Number(feature.get(areaProperty));
    return ee.Algorithms.If(totalArea.gt(0), area.divide(totalArea).multiply(100), 0);
  };

  return feature.set({
    'proportion_class_0_pct': getProportion('class_0_acres'),
    'proportion_class_1_pct': getProportion('class_1_acres'),
    'proportion_class_2_pct': getProportion('class_2_acres'),
    'proportion_class_3_pct': getProportion('class_3_acres'),
    'proportion_class_0_near_uplands_pct': getProportion('class_0_near_uplands_acres'),
    'proportion_class_1_near_uplands_pct': getProportion('class_1_near_uplands_acres'),
    'proportion_class_2_near_uplands_pct': getProportion('class_2_near_uplands_acres'),
    'proportion_class_3_near_uplands_pct': getProportion('class_3_near_uplands_acres')
  });
});

// =================================================================================
//  6. EXPORT
// =================================================================================
Export.table.toDrive({
  collection: finalTable,
  description: 'YYYYMMDD_Ecoregion_And_Biome_Persistence_Stats_30m',
  folder: 'YOUR_FOLDER',
  fileNamePrefix: 'YYYMMDD_ecoregionAndBiomePersistenceStats_30m',
  fileFormat: 'CSV'
});