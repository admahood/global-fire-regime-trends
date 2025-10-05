/**
 * Set of functions used in the Global Fire Report analysis
 * 
 * Author:
 * Maxwell C. Cook, PhD Student, Department of Geography
 * University of Colorado Boulder
 * maxwell.cook@colorado.edu
 * 
 */

// Reproject to WGS84
exports.reprojWGS = function( imageCol ){
  
  return imageCol.map(function(image){
    var proj = image.projection().getInfo();
    var transform = proj.transform, crs = proj.crs;

    return image
                .setDefaultProjection(proj.crs)
                .reduceResolution({
                  reducer: ee.Reducer.mean(),
                  maxPixels: 1e13
                })
                .reproject({
                  crs: 'EPSG:4326',
                  scale: 500
                });
    });
};


// Reproject a FeatureCollection
exports.transWGS = function( ftr ) {
  
  var transformed = ftr.transform('EPSG:4326', 0.001);
  
  return transformed;
};


// function to retrieve the percent area of a particular raster class
exports.percentClass = function(image, feature, rclass, res, prop, name){
  
  //calculate the area of the geometry (km2)
  var area = feature.geometry().area().divide(1000000);
  
  //subset to pixel class value, clip to feature geometry
  var a = image.eq(rclass).clip(feature.geometry());
  
  //mask out all other pixel values, get the image area
  var b = a.updateMask(a).multiply(ee.Image.pixelArea());
  
  //calculate the area of pixels of a given class within the feature
  var c = b.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: feature.geometry(),
    scale: res,
    maxPixels: 1e13
  });
  
  // get the percent of the area
  var areakm2 = ee.Number(c.get(prop))
    .divide(1000000).divide(area).multiply(100);
  
  // set the property name
  return c.set(prop, areakm2).rename([prop], [name]);
  
};