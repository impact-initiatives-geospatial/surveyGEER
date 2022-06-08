

#'accumulate bands to list
#' @description  Function to be used with [rgee] `iterate`  to return a list of images containing the image
#' @param img ee Image class object
#' @param lst list containing first img to iterate over
#' @return ee List of images with each item containing the successive accumulation of




accumulate_img_bands_to_list <-  function(img,lst){
  previous <- ee$Image(ee$List(lst)$get(-1))
  added <- img$add(previous)$
    set('system:time_start', img$get('system:time_start'))
  return(
    # ee$List(list(lst))$add(added)
    ee$List(list(lst))$add(ee$List(added))

  )
}


#' accumulate_ic_band
#' @description Creates a cumulative precipitation [rgee] `ee$ImageCollection` with each all pixels in each consecutive image containing the sum of all previous values
#' @param ic `ee$ImageCollection` object with user-defined start and end-date
#' @param band band to accumulate value from
#' @return returns `ee$ImageCollection` with each contained image containing the successive addition/accumulation of pixel-level values of chosen band.
#' @details user should carefully decide start and end date for accumulation. This is currently really only designed for assessing seasonal accumulations
#' @export

#' @examples \dontrun{
#'
#' # load libraries
#' library(easyrgee)
#' library(rgee)
#' ee_Initialize()
#'
#' chirps <-  ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$filterDate("2016-01-01","2016-12-31")
# # run run function
#' precip_cumulative<- ee_accumulate_band_ic(ic = chirps, band = "precipitation")
#'
#' }

ee_accumulate_band_ic <-  function(ic,band){
  # should think about "first" definition
  first <- ee$List(list(ee$Image(ic$select(band)$first())))
  accumulated_img_list <-  ic$iterate(accumulate_img_bands_to_list,first)
  ee$ImageCollection$fromImages(ee$List(accumulated_img_list)$flatten())
}


reduce_to_doy_img <- function(ic,thresh,band){
  ic$map(function(img){
    doy = img$date()$getRelative('day', 'year');
    doyBand = ee$Image$constant(doy)$uint16()$rename('doy')
    const1000 = ee$Image$constant(1000)$uint16()$rename('const1000')
    lt_thresh_mask = img$select(band)$lt(thresh)
    # gte_thresh_mask = img$select("precipitation")$gte(20)
    img_w_doy = img$addBands(doyBand$select("doy"))$addBands(const1000$select('const1000'))
    lets_see = img_w_doy$select("doy")$where(lt_thresh_mask, const1000)
    return(lets_see)
  }
  )$min()

}


