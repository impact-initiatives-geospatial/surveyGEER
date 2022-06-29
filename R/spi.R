#' ee_spi calculate standard precipitation index
#'
#' @param x
#' @param window
#' @param window_unit
#' @param ic_time_unit
#' @param band name of band
#' @param moi month of interest. (i.e if moi =4 and window is 3 months then the SPI is FMA)
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
                        precip_baseline_sd= img$select(glue::glue("{rolling_band_name}_stdDev"))
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
#' @param x
#' @param window
#' @param window_unit
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

  x_year_month <-  x |>
    group_by(year, month) |>
    summarise(
      stat="sum"
    )
  new_band_name <-  glue::glue("{band}_sum")

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
