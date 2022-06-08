#' creates day of year image/raster based on an accumulation threshold
#' @param ic an ee ImageCollection
#' @param thresh accumulation threshold
#' @param band band to accumulate
#' @return and ee image/raster the contains the first date (DOY) the accumulation threshold was met per pixel.
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
#' #visualize
#' Map$addLayer(precip_threshold_doy,
#'              visParams = list(min=0,
#'                            max=365,
#'                              palette=c('#9ecae1,
#'                              #ffffff,
#'                               #ffeda0,
#'                               #feb24c,
#'                               #f03b20'))
#'              )
#'
#' }

ee_accum_reduce_to_doy <- function(ic, thresh,band){
  ic_cumulative <- ee_accumulate_band_ic(ic = ic,band = band)
  reduce_to_doy_img(ic = ic_cumulative,thresh = thresh,band = band)

}
