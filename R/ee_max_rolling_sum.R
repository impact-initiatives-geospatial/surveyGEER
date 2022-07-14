
#' ee_max_rolling_sum
#'
#' @param x tidyee class object containing `ee$ImageCollection`
#' @param roll_window time window to calculate rolling statistic (currently only supports days)
#' @param roll_time_unit time unit (currently only supports days)
#' @param over_time over how many days do you want to calculate the maximum of the rolling sum pixels
#' @param from_when \code{character} date from which you want to end the rolling calculation (by default will use the late of last image)
#' @param return_tidyee \code{logical} if T will return `tidyee` class if F will return `ee$Image` (note: F will be faster)
#' @return `tidyee` or `ee$Image` object containing image with maximum rolling sum composite on a per pixel basis
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' library(rgee)
#' library(tidyverse)
#' library(tidyrgee)
#' ee_Initialize()
#' chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
#' chirpsIC <- ee$ImageCollection(chirps_link)
#'
#' # create tidyee class object
#' chirps_tidy <- as_tidyee(chirpsIC)
#'
#' # from "2022-05-31" I want to calculate the maximum 3 day precipitation event over the past 60 days
#'
#'   rainfall_3day_max_60days <- ee_max_rolling_sum(
#'       x = chirps_tidy,
#'       roll_window = 3,
#'       roll_time_unit = "day",
#'       from_when = "2022-05-31",
#'       over_time = 60,
#'       return_tidyee = F)
#' }

ee_max_rolling_sum <- function(x, roll_window,roll_time_unit="day",from_when=NULL,over_time,return_tidyee){
  assertthat::is.number(roll_window)
  assertthat::is.number(over_time)
  assertthat::assert_that(roll_time_unit=="day",msg="day is the only supported time unit (for now)")
  inherits(x,"tidyee")

  cat("creating rolling sum ImageCollection\n")
  if(!is.null(from_when)){
    start_filter <- rgee::eedate_to_rdate(
      ee$Date(from_when)$advance(ee$Number(over_time+roll_window)$multiply(-1),"day")
    ) |> lubridate::as_date() # add roll window so we have #s on first day onwards after rolling sum is calculated
    x <- x |> filter(date>=start_filter,date<=from_when)
    start_date <- ee$Date(from_when)$advance(ee$Number$parse(as.character(over_time))$multiply(-1), roll_time_unit)
  }

  rolling_sum_collection <- ee_rolling_statistic2(x = x,
                                                  stat = "sum",
                                                  window = roll_window,
                                                  time_unit = roll_time_unit,
                                                  return_tidyee = return_tidyee)
  cat("rolling sum finished")
  if(is.null(from_when)){
    if(return_tidyee){
      cat("getting last date in collection\n")
      from_when<- rolling_sum_collection$vrt$time_start |>
        max() |>
        lubridate::as_date() |>
        as.character()
      cat("finished with last date in collection\n")
    }
    if(!return_tidyee){
      cat("getting last date in collection\n")
      from_when <-
        rolling_sum_collection$
        sort(prop = "system:time_start",opt_ascending = FALSE)$
        first()$get("system:time_start")
      cat("finished with last date\n")
    }
    # kind of main output of is.null from_when?
    start_date <- ee$Date(last_available)$advance(ee$Number$parse(as.character(over_time))$multiply(-1), roll_time_unit)
  }

  if(!return_tidyee){
    rolling_sum_collection_filtered <- rolling_sum_collection$
      filterDate(
        ee$Date(start_date), ee$Date(from_when)$advance(1,"day") # advance 1 day to make end_date inclusive
      )

    max_rolling_sum <- rolling_sum_collection_filtered$
      max()$
      set('system:time_start',ee$Date(start_date))
  }
  if(return_tidyee){
    start_date_r <- start_date |> rgee::eedate_to_rdate() |> lubridate::as_date()
    max_rolling_sum <- rolling_sum_collection |>
      filter(date>=start_date_r,date<=last_available) |>
      summarise(
        stat="max"
      )
  }
  return(max_rolling_sum)
}



