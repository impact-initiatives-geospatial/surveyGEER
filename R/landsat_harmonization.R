# Function to get and rename bands of interest from OLI.

#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
#'
renameOli = function(img) {
  img$select(c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'pixel_qa'),
             c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa'))
}

# Function to get and rename bands of interest from ETM+.
#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
renameEtm = function(img) {
  img$select(c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'pixel_qa'),
             c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa'))
}

# Function to convert etm to oli using coefficients


#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
etmToOli = function(img) {
  coefficients = list(
    itcps = ee$Image$constant(c(0.0003, 0.0088, 0.0061, 0.0412, 0.0254, 0.0172))$multiply(10000),
    slopes = ee$Image$constant(c(0.8474, 0.8483, 0.9047, 0.8462, 0.8937, 0.9071))
  )
  img = img$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2'))$multiply(ee$Image(coefficients$slopes))$add(coefficients$itcps)$round()$toShort()$addBands(img$select('pixel_qa'))

  return(img)
}

# This function gets NDVI to landsat.

#' Title
#'
#' @param image
#'
#' @return
#' @export
#'
#' @examples
addNDVI = function(image) {
  return(image$addBands(image$normalizedDifference(c('NIR', 'Red'))$rename('NDVI')))
}

# This function gets NDWI for landsat.


#' Title
#'
#' @param image
#'
#' @return
#' @export
#'
#' @examples
addNDWI = function(image) {
  return(image$addBands(image$normalizedDifference(c('Green', 'NIR'))$rename('NDWI')))
}

#get near burn index

#' Title
#'
#' @param image
#'
#' @return
#' @export
#'
#' @examples
calcNbr = function(image) {
  return(image$addBands(image$normalizedDifference(c('NIR', 'SWIR2'))$rename('NBR')))
}


# Mask out bad pixels (cloud masker)
#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
fmask = function(img) {
  cloudShadowBitMask = base::bitwShiftL(1,3)
  cloudsBitMask = base::bitwShiftL(1,5)
  qa = img$select('pixel_qa')
  mask = qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$And(qa$bitwiseAnd(cloudsBitMask)$eq(0))

  return(img$updateMask(mask))
}

# Define function to prepare OLI images.


prepOli = function(img) {
  orig = img
  img = renameOli(img)
  img = fmask(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# Define function to prepare ETM+ images.


#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
prepEtm = function(img) {
  orig = img
  img = renameEtm(img)
  img = fmask(img)
  img = etmToOli(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# without Roy harmonizing
#' Title
#'
#' @param img
#'
#' @return
#' @export
#'
#' @examples
prepEtm_cloud = function(img) {
  orig = img
  img = renameEtm(img)
  img = fmask(img)
  img = addNDVI(img)
  img = addNDWI(img)
  img = calcNbr(img)

  return(ee$Image(img$copyProperties(orig, orig$propertyNames())))
}

# function for pre-processing landsat data

get_landsat_roy_harmonized <- function(){

  # Bring in image collections from landsat 5,7,8
  oliCol <-  ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')
  etmCol <-  ee$ImageCollection('LANDSAT/LE07/C01/T1_SR')
  tmCol <-  ee$ImageCollection('LANDSAT/LT05/C01/T1_SR')
  ld4Col <- ee$ImageCollection('LANDSAT/LT04/C01/T1_SR')
  landsat_quality_filter <- ee$Filter$lt('CLOUD_COVER', 50)$
    lt('GEOMETRIC_RMSE_MODEL', 10)$
    Or(ee$Filter$eq('IMAGE_QUALITY', 9),ee$Filter$eq('IMAGE_QUALITY_OLI', 9))

  # Filter collections and prepare them for merging.
  oliCol = oliCol$filter(landsat_quality_filter)$map(prepOli)
  etmCol = etmCol$filter(landsat_quality_filter)$map(rgee::ee_utils_pyfunc(prepEtm))
  tmCol = tmCol$filter(landsat_quality_filter)$map(rgee::ee_utils_pyfunc(prepEtm))
  collection = ee$ImageCollection(oliCol$merge(etmCol)$merge(tmCol))
  return(collection)



}
