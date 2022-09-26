#' modis_mask_clouds : function to mask clouds, cloud shadows, snow.
#' @name cloud_mask_modis
#' @title mask_modis_clouds
#' @param x MODIS `ee$Image` or `ee$ImageCollection`
#' @export
#' @examples \dontrun{
#'  library(easyrgee)
#'  library(rgee)
#'  ee_Initialize()
#'  modisIC <- ee$ImageCollection("MODIS/006/MOD13Q1")
#'  # mask an imageCollection
#'  modisIC_cloud_masked <- cloud_mask_modis(modisIC)
#'  # mask an image
#'  modis_first_cloud_masked <- cloud_mask_modis(modisIC$first())
#'
#' }



cloud_mask_modis <- function(x,...){
  UseMethod('cloud_mask_modis')
}


#' @name cloud_mask_modis
#' @export
cloud_mask_modis.ee.image.Image = function(x){
  # make a new single band image from the pixel qa band
  pixel_qa = x$select("SummaryQA")
  # retain clear (0) and water (1) pixels
  return( x$updateMask(pixel_qa$eq(0)))

}

#' @name cloud_mask_modis
#' @export

cloud_mask_modis.ee.imagecollection.ImageCollection = function(x){
  x$map(function(img){
    cloud_mask_modis(img)
  }
  )
}






#' bitwise_extract
#'
#' @param value ee$Number or ee$Image to extract from
#' @param from_bit int or ee$Number with the first bit
#' @param to_bit  int or ee$number with the last bit (inclusive)
#'
#' @description utility to extract bitmask values - https://spatialthoughts.com/2021/08/19/qa-bands-bitmasks-gee/
#'
#' @return bit

bitwise_extract <-  function(value, from_bit,to_bit){
  if(is.null(to_bit)){
    to_bit<- from_bit
  }

  maskSize = ee$Number(1)$add(to_bit)$subtract(from_bit)
  mask = ee$Number(1)$leftShift(maskSize)$subtract(1)
  return (value$rightShift(from_bit)$bitwiseAnd(mask))
}


#' @title cloud_and_quality_mask_modis
#'
#' @param x image or image collection
#' @param ... Other arguments to pass to
#' @return image/image collection masked
#' @export
#'
#' @examples \dontrun{
#' library(rgee)
#' library(easyrgee)
#'ee_Initialize()
#'
#' modis_ic = ee$ImageCollection('MODIS/006/MYD13Q1')$
#'     filter(ee$Filter$date('2015-08-01', '2015-08-31'))
#'
#' modis_first_masked<- cloud_and_quality_mask_modis(modis_ic$first())
#'
#' # just demonstrating that it also works on ic
#' modis_ic_masked <-  cloud_and_quality_mask_modis(modis_ic)
#'
#' # now we can visualize
#' # lets make a point to zoom in on
#' ptdf <- data.frame(lon=44.43604342660201,lat=3.39908380146876)
#' ptsf <- sf::st_as_sf(ptdf, coords=c("lon","lat"),crs=4326)
#' pt_ee <- sf_as_ee(ptsf)
#'
#' # center and zoom
#' Map$centerObject(pt_ee, 13)
#'
#' # here is a standard MODIS ndvi viz set up
#' ndviVis = list(
#'     min=0.0,
#'     max= 8000.0,
#'     palette= c(
#'         'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
#'         '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
#'         '012E01', '011D01', '011301'
#'         )
#' )
#'
#' Map$addLayer(modist_first_masked$select("NDVI"),ndviVis)
#'
#'
#' }



cloud_and_quality_mask_modis <- function(x,...){
  UseMethod('cloud_and_quality_mask_modis')
}


#' @export

cloud_and_quality_mask_modis.ee.image.Image <-  function(x){
  pixel_qa = x$select("SummaryQA")
  cloudMask = bitwise_extract(pixel_qa, 0, 1)$lte(1)
  dataQualityMask = bitwise_extract(pixel_qa, 2, 5)$lte(10)
  # should figure out why And instead of "and" ... seems to match javascript output though
  cloudQualityMask = cloudMask$And(dataQualityMask)
  return (x$updateMask(cloudQualityMask))
}



#' @export
cloud_and_quality_mask_modis.ee.imagecollection.ImageCollection <-  function(x){
  x$map(function(img){
    cloud_and_quality_mask_modis(img)
  }
  )
}







