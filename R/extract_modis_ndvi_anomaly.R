


#' Title
#'
#' @param tidyee \code{logical} return tidyee class or image/imageCollection
#'
#' @return
#' @export
#'
#' @examples
terra_aqua_merge <-  function(tidyee = F) {
  terra_ic <- ee$ImageCollection(surveyGEER:::get_modis_link("terra"))
  aqua_ic <- ee$ImageCollection(surveyGEER:::get_modis_link("aqua"))

  terra_aqua_merged <- terra_ic$merge(aqua_ic)
  if (tidyee) {
    terra_aqua_merged <- as_tidyee(terra_aqua_merged)
  }
  return(terra_aqua_merged)
}


#' modis ndvi anomaly time range
#'
#' @param baseline_years
#' @param yoi
#' @param date_range
#'
#' @return
#' @export
#'
#' @examples
modis_ndvi_anomaly <- function(baseline_years = 2000:2021,
                                  date_range = c("2021-06-20", "2021-09-26"),
                                  range_label = "growing_season"
                                  ) {
  terra_aqua_ic <- terra_aqua_merge()
  terra_aqua_masked <-
    surveyGEER:::cloud_scale_modis_ndvi(x = terra_aqua_ic,
                                        mask = "cloud&quality")
  ta_tidy <- as_tidyee(terra_aqua_masked)

  start_date <- as.Date(date_range[1])
  end_date <-  as.Date(date_range[2])
  yoi <-  lubridate::year(start_date)

  ta_tidy <- ta_tidy |>
    mutate(
      !!sym(range_label) := if_else(
        doy %in% lubridate::yday(x = start_date):lubridate::yday(x = end_date),
        T,
        F
      )
    )

  baseline <- ta_tidy |>
    filter(year %in% baseline_years) |>
    filter(!!sym(range_label)) |>
    summarise(stat = list("mean", "sd", "min", "max", "median"))
  baseline$ee_ob <- baseline$ee_ob$first()
  # in tidyrgee summarise resulting in 1 should cast to image.

  # time of interest
  toi <-  ta_tidy |>
    filter(date>=start_date,date<=end_date) |>
    summarise(stat = list("mean"))


  toi <- toi |>
    select(NDVI = "NDVI_mean")

  # create a unique, but common property to join the two image collection bands
  baseline$ee_ob <- baseline$ee_ob$
    set("month", ee$Date(baseline$ee_ob$get("system:time_start"))$format("MM"))


  toi$ee_ob <- toi$ee_ob$
    set("month", ee$Date(toi$ee_ob$get("system:time_start"))$format("MM"))


  toi_baseline <- toi |>
    inner_join(baseline, by = "month")

  toi_baseline_ic <- toi_baseline |>
    as_ee()

  ndvi_zscore <- toi_baseline_ic$map(function(img) {
    zscore <- img$expression(
      "float((NDVI-NDVI_median)/(NDVI_sd))",
      # "float((NDVI-NDVI_mean)/(NDVI_sd))",
      opt_map = list(
        NDVI = img$select("NDVI"),
        NDVI_median = img$select("NDVI_median"),
        # NDVI_mean= img$select("NDVI_mean"),
        NDVI_sd = img$select("NDVI_sd")
      )
    )$rename("NDVI_z_score")
    img$select("NDVI", "NDVI_mean", "NDVI_median", "NDVI_min", "NDVI_max")$addBands(zscore)
  })$first()


}


#' extract_ndvi_anomaly
#'
#' @param geom_sf
#' @param baseline_years
#' @param date_range
#' @param range_label
#' @param scale
#'
#' @return
#' @export
#'
#' @examples

extract_modis_ndvi_anomaly <-  function(geom_sf,
                                     baseline_years = 2000:2021,
                                     date_range = c("2021-06-20", "2021-09-26"),
                                     range_label = "growing_season",scale= 250){
  geom_ee <- rgee::sf_as_ee(geom_sf)
  ndvi_anomaly_img <- modis_ndvi_anomaly(baseline_years = baseline_years,
                                            date_range = date_range ,
                                            range_label = range_label )
  ndvi_anomaly_img <- ndvi_anomaly_img$select("NDVI_z_score")$rename("ndvi_z_growing_season_21")

  fc_values <- ndvi_anomaly_img$sampleRegions(
    collection= geom_ee,
    scale= scale
  )
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive") |>
      sf::st_drop_geometry()
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values) |>
      sf::st_drop_geometry()
  }
  return(df_values)
}
#' extract_ndvi_anomaly
#'
#' @param geom_sf
#' @param baseline_years
#' @param date_range
#' @param range_label
#' @param scale
#'
#' @return
#' @export
#'
#' @examples

extract_modis_ndvi_anomaly_urb_mask <-  function(geom_sf,
                                         # urban_mask= T,
                                         focal_radius = 4,
                                         baseline_years = 2000:2021,
                                         date_range = c("2021-06-20", "2021-09-26"),
                                         range_label = "growing_season",scale= 250){
  # load in landcover
  esa_ic <- ee$ImageCollection("ESA/WorldCover/v100")
  esa_img <- ee$Image(esa_ic$first())$rename("esa_lulc_10m")

  geom_ee <- rgee::sf_as_ee(geom_sf)
  ndvi_anomaly_img <- modis_ndvi_anomaly(baseline_years = baseline_years,
                                            date_range = date_range ,
                                            range_label = range_label )
  ndvi_anomaly_img <- ndvi_anomaly_img$select("NDVI_z_score")$
    rename("ndvi_z_growing_season_21_fs")
  ndvi_urban_masked <- ndvi_anomaly_img$ee_ob$updateMask(esa_img$neq(50))

  ndvi_urban_masked_focal_median <- ndvi_urban_masked$
    focal_median( radius = focal_radius,
                  kernelType= "circle",
                  units="pixels")$
    reproject(ndvi_anomaly_img$first()$projection())


  fc_values <- ndvi_urban_masked_focal_median$sampleRegions(
    collection= geom_ee,
    scale= scale
  )
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive") |>
      sf::st_drop_geometry()
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values) |>
      sf::st_drop_geometry()
  }
  return(df_values)
}
