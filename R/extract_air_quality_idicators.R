#' Title
#'
#' @param image image from sentinel 5p sensor
#' @param threshold cloud fraction threshold (default= 0.05)
#'
#' @return `ee$Image` with pixels with cloud fraction less than threshold masked
#' @export
#'
#' @examples
cloud_mask_s5p <-  function(image, threshold) {
  return(image$updateMask(image$select("cloud_fraction")$lt(threshold)))
}



#' Title
#'
#' @param cloud_fraction_thresh
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' so2_daily <- s5p_so2_ic()
#' }
s5p_so2_ic <- function(cloud_fraction_thresh = 0.05) {
  ic <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_SO2")
  ic_cloud_masked = ic$select(
    list(
      "SO2_column_number_density",
      "SO2_column_number_density_amf",
      "SO2_column_number_density_15km",
      'cloud_fraction'
    )
  )$map(function(img) {
    cloud_mask_s5p(img, threshold = cloud_fraction_thresh)
  })

  as_tidyee(ic_cloud_masked) |>
    group_by(year, month) |>
    summarise(stat = "mean")
}

#' Title
#'
#' @param cloud_fraction_thresh
#'
#' @return ee$ImageCollection with NO2 bands
#' @export
#'
#' @examples \dontrun{
#' no2_daily <- s5p_no2_ic()
#' }
s5p_no2_ic <- function(cloud_fraction_thresh = 0.05) {
  ic <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")
  ic_cloud_masked = ic$select(
    list(
      "NO2_column_number_density",
      "tropospheric_NO2_column_number_density",
      'cloud_fraction'
    )
  )$map(function(img) {
    cloud_mask_s5p(img, threshold = cloud_fraction_thresh)
  })
  as_tidyee(ic_cloud_masked) |>
    group_by(year, month) |>
    summarise(stat = "mean")
}

#' @name s5p_air_quality
#' @title sentinel 5p air quality indicators
#' @return tidyee object `ee$ImageCollection` and S02 and No2 bands
#' @export
#'
#' @examples \dontrun{
#' library(rgee)
#' library(tidyrgee)
#' ee_Initialize()
#' daily_air_quality<-s5p_air_quality()
#' }

s5p_air_quality <- function() {
  so2_daily <- s5p_so2_ic()
  no2_daily <- s5p_no2_ic()
  so2_daily |>
    inner_join(no2_daily, by = "system:time_start")
}

#' Title
#'
#' @param geom_sf
#' @param yoi
#' @param moi
#' @param img_scale
#'
#' @return
#' @export
#'
#' @examples
extract_s5p_air_quality <- function(geom_sf,yoi=2022, moi=5, img_scale=111320){

  geom_ee <-  rgee::sf_as_ee(geom_sf)
  ic <- s5p_air_quality()
  img <- ic |>
    filter(year%in% yoi,month %in%moi) |>
    as_ee()

  fc_values <- img$
    sampleRegions(collection= geom_ee,
                  scale= img_scale)

  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values)
  }
  # df_values <- rgee::ee_as_sf(fc_values,maxFeatures = nrow(geom_sf))
  # pct_na <- geom_sf |>
  #   st_drop_geometry() |>
  #   left_join(df_values, by ="new_uid") |>
  #   janitor::tabyl(NO2_column_number_density_mean ) |>
  #   filter(is.na(valid_percent)) |> pull(percent)
  # if(pct_na>0.5){
  #   img_median <- ic |>
  #     filter(year%in% yoi,month %in%c(moi-2:moi)) |>
  #     summarise(stat="median") |>
  #     as_ee()
  #   fc_values <- img_median$
  #     sampleRegions(collection= geom_ee,
  #                   scale= img_scale)
  #   df_values <- rgee::ee_as_sf(fc_values,maxFeatures = nrow(geom_sf))
  # }
  return(df_values)
}

