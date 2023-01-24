#' ee_spi calculate standard precipitation index
#'
#' @param x tidyee imageCollection
#' @param window window of time to calculate sPI
#' @param window_unit unit o ftime to calculate
#' @param ic_time_unit imagecollection time unit
#' @param band name of band
#' @param moi month of interest. (i.e if moi =4 and window is 3 months then the SPI is FMA)
#' @param use_tidyee default = T , no option for F at the moment.
#' @details By harnessing tidyee with `use_tidyee=T` we don't have any
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(rgee)
#' library(tidyrgee)
#' ee_Initialize()
#' spi_3 <- ee_spi(x=monthly_rainfall,
#'                window=3,
#'                window_unit="month",
#'                ic_time_unit= "month",
#'                band="precipitation_sum",
#'                moi=4,use_tidyee=T)
#'}

ee_spi <- function(x,
                   window=6,
                   window_unit="month",
                   ic_time_unit= "month",
                   band="precipitation_sum",
                   moi=c(3,4),use_tidyee=T
){

  assertthat::assert_that(window_unit %in% c("day","month"),
                          msg = "currently SPI must be formulated as in 'day' or 'month'" )

  assertthat::assert_that(ic_time_unit %in% c("day","month"),
                          msg = "imageCollection must be in 'day' or 'month' time steps for the SPI calculation ot take place" )
  new_spi_band_name<- glue::glue("{band}_{window}_{window_unit}_SPI")
  spi_mos <- glue::glue('2020-{moi}-01') |>
    lubridate::as_date() |>
    purrr::map(~glue::glue_collapse(lubridate::month(.x-months(1:window-1) |> rev(),label=T, abbr=T),sep="-"))
  if(length(spi_mos)>1){
    spi_mos <- glue::glue_collapse(spi_mos,sep = " and\n")
  }
  cat(glue::glue("calculating {window} month SPI with \n{spi_mos} \n"))

  rolling_ee<- ee_rolling_statistic2(x = x,
                                     stat = "sum",
                                     window = window,
                                     time_unit = window_unit,
                                     return_tidyee = F)

  # this could speed this up by skipping this step and using `rgee` + mapping on month
  # think I should doit both ways and compare?
  if(use_tidyee){
    rolling_tidyee <-  tidyrgee::as_tidyee(rolling_ee)

    baseline_rollling_tidyee <- rolling_tidyee |>
      group_by(month) |>
      summarise(stat= list("mean","sd"))

    current_and_historical_rolling <- rolling_tidyee |>
      filter(month%in%moi) |>
      inner_join(baseline_rollling_tidyee, by="month")
    tidyrgee:::vrt_band_names(current_and_historical_rolling)

    rolling_band_name <- glue::glue("{band}_rollsum{window}")

    ic_spi<- current_and_historical_rolling$ee_ob$map(
      function(img){
        zscore<- img$expression(
          "float((precip_current-precip_baseline_mean)/(precip_baseline_sd))",
          opt_map= list(precip_current= img$select(rolling_band_name),

                        precip_baseline_mean= img$select(glue::glue("{rolling_band_name}_mean")),
                        precip_baseline_sd= img$select(glue::glue("{rolling_band_name}_sd"))
          )
        )$rename(new_spi_band_name)
        img$select(rolling_band_name)$addBands(zscore)
      }

    )
    ic_spi <- ic_spi$select(new_spi_band_name)

  }
  return(as_tidyee(ic_spi))
}



#' ee_chirps_spi calculate standard precipitation index from chirps daily
#'
#' @param x tidyee ee$ImageCollection
#' @param window window to calculate spi over
#' @param window_unit time unit of window (month only)
#' @param band name of band
#' @param moi month of interest. (i.e if moi =4 and window is 3 months then the SPI is FMA)
#'
#' @return tidyee object containing SPI ee$ImageCollection and vrt
#' @export
#'
#' @examples \dontrun{
#' library(rgee)
#' library(tidyrgee)
#' ee_Initialize()
#'
#' spi_2_chirps<- ee_chirps_spi(x = chirps_tidy,window = 2,window_unit = "month")
#'}




ee_chirps_spi <- function(x=NULL,
                          window,
                          window_unit,
                          moi,
                          .load_chirps=T,
                          band="precipitation"
                          ){

  assertthat::assert_that(window_unit %in% c("day","month"),
                          msg = "currently SPI must be formulated as in 'day' or 'month'" )
  if(is.null(x)){
    cat("no tidyee object loaded, CHIRPS daily being loaded from GEE\n")
    .load_chirps <- T
  }
  if(.load_chirps){
    chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
    ic<- rgee::ee$ImageCollection(chirps_link)
    cat("loading tidyee object (takes a few seconds)\n")
    x <-  as_tidyee(ic)
  }

  # this grouping could be causing the samll difference with unosat values....
  x_year_month <-  x #|>
    # group_by(year, month) |>
    # summarise(
      # stat="sum"
    # )

  # if no sum, then need to remove it
  new_band_name <-  glue::glue("{band}")#_sum")

  cat("beginning SPI func - expect 2-3 minutes\n")
  spi_tidyee <- ee_spi(x=x_year_month,
                       window=window,
                       window_unit=window_unit,
                       ic_time_unit= "month",
                       band=new_band_name,
                       moi=moi,
                       use_tidyee=T)

  return(spi_tidyee)

}



extract_spi_to_values <-  function(geom_sf,
                                   mo_lags= list(1,3,6,9,12),
                                   moi=5){
  # target wrapper specific assertion
  assertthat::assert_that(length(moi)==1,
                          msg = "targets extract wrapper not tested with mois>1"
                          )

  cat("calculating SPIs on pixel-basis\n")
  moi_label <- lubridate::month(moi, label=T, abbr= T) |> as.character()
  spi_imgs <- mo_lags |>
    purrr::map(
      ~ee_chirps_spi(window = .x,window_unit = "month",moi = moi)
    )
  spi_imgs_2022 <- spi_imgs |>
    purrr::map(
      ~.x |> filter(year>=2022)
    )
  spis_2022 <- purrr:::reduce(.x=spi_imgs_2022,.f = inner_join, by ="month")
  # if this is an image we might be better of with sampleRegions
  cat(class(spis_2022$ee_ob))
  cat("extracting spi to points\n")
  spis_2022_img <- ee$Image(spis_2022$ee_ob$first())
  # spis_2022_img <- ee$Image(spis_2022_imgs$ee_ob$first())
  geom_ee <-  rgee::sf_as_ee(geom_sf)

  fc_values <- spis_2022_img$
    sampleRegions(collection= geom_ee,
                  scale= 5500)
  cat("converting extracting feature collection to data.frame")
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values)
  }
  res <- df_values |>
    rename_with(.cols = matches("^precipitation"),
                .fn = ~glue::glue("{moi_label}_spi{readr::parse_number(.x)}"))
  return(res)

  # res <- spis_2022 |>
    # ee_extract_tidy(y = geom_sf,stat = "median",scale = 5500,via = "gcs")
  # return(spis_2022)

}


#' extract_spi_to_values2 same as above, but adding yoi parameter.
#' @description made as new function so that i don't have to re-run all SPI targets...
#'  this is a good example of where `{targets}` and package dev are not playing to nice
#'
#'
#' @param geom_sf
#' @param mo_lags
#' @param moi
#' @param yoi
#'
#' @return
#' @export
#'
#' @examples
extract_spi_to_values2 <-  function(x,
                                    geom_sf,
                                    mo_lags= list(1,3,6,9,12),
                                    moi=5,
                                    yoi
                                   ){
  # target wrapper specific assertion
  assertthat::assert_that(length(moi)==1,
                          msg = "targets extract wrapper not tested with mois>1"
                          )

  if(inherits(x = x,what = "ee.imagecollection.ImageCollection")){
    x <- as_tidyee(x)
  }
  x_filt <- x |>
    filter(year<=yoi)


  cat("calculating SPIs on pixel-basis\n")
  moi_label <- lubridate::month(moi, label=T, abbr= T) |> as.character()
  spi_imgs <- mo_lags |>
    purrr::map(
      ~ee_chirps_spi(x =x_filt, window = .x,window_unit = "month",moi = moi,.load_chirps = F)
    )
  cat("filter image to yoi")
  spi_imgs_yoi <- spi_imgs |>
    purrr::map(
      ~.x |> filter(year==yoi)
    )
  cat("join img by month")
  spi_yoi <- purrr:::reduce(.x=spi_imgs_yoi,.f = inner_join, by ="month")
  # if this is an image we might be better of with sampleRegions
  # cat(class(spis_2022$ee_ob))
  cat("extracting spi to points\n")
  spi_yoi_img <- ee$Image(spi_yoi$ee_ob$first())
  # spis_2022_img <- ee$Image(spis_2022_imgs$ee_ob$first())
  geom_ee <-  rgee::sf_as_ee(geom_sf)

  fc_values <- spi_yoi_img$
    sampleRegions(collection= geom_ee,
                  scale= 5500)
  cat("converting extracting feature collection to data.frame")
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values)
  }
  res <- df_values |>
    rename_with(.cols = matches("^precipitation"),
                .fn = ~glue::glue("{moi_label}_spi{readr::parse_number(.x)}"))
  return(res)

  # res <- spis_2022 |>
    # ee_extract_tidy(y = geom_sf,stat = "median",scale = 5500,via = "gcs")
  # return(spis_2022)

}

