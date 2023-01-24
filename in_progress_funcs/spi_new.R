

  #' Title
  #'
  #' @param x
  #' @param stat
  #' @param size \code{numeric}
  #' @param unit \code{character} month or days
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #'
  library(tidyverse)
  library(rgee)
  # ee_Initialize()
  ic<- rgee::ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")

  ee_roll_p <- function(x=ic,roll_from="2022-02-01",stat,size=3, unit="month"){
    start_date <- ee$Date(roll_from)$advance(-size,unit)
    x_window <- x$filterDate(start_date, roll_from)
    ee_reduce(x_window)
  }


  ee_roll_baseline <- function(x=ic,roll_from="2022-02-01",stat,size=3, unit="month",baseline_year=2000:2015){
    # get all years on record
    baseline_roll_from <- lubridate::as_date(glue::glue("{baseline_year}-{lubridate::month(roll_from)}-{lubridate::day(roll_from)}"))
    baseline_roll_from_ee <- ee$List(baseline_roll_from)
    ee$ImageCollection$fromImages(baseline_roll_from_ee$map(
      rgee::ee_utils_pyfunc(function(date){
        roll_from = ee$Date(baseline_roll_from_ee)
        start_date <- ee$Date(roll_from)$advance(-size,unit)
        x_window <- x$filterDate(start_date, roll_from)
        x_img <- ee_reduce(x_window)
        x_img$set("system:time_start",roll_form)$
          set("roll_start",start_date)
      })
    )
    )

  }


  # ee_reduce_date_range
  # baseline_from_range
