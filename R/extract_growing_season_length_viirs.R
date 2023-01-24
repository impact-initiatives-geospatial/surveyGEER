

#' Title
#'
#' @param geom_sf
#' @param yoi
#' @param scale
#'
#' @return
#' @export
#'
#' @examples

extract_growing_season_length_viirs <-  function(geom_sf, yoi=2013:2020,scale=500){
  ic = ee$ImageCollection('NOAA/VIIRS/001/VNP22Q2')
  tidy_ic <-  as_tidyee(ic)

  tidy_ic_filtered <- tidy_ic |>
    select("Growing_Season_Length_1") |>
    filter(year %in% yoi)
  res <- tidy_ic_filtered |>
    ee_extract_tidy(y = geom_sf,stat = "median",via = "drive",sf = F,scale = scale)
  return(res)
}
