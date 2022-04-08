
#' date_to_range
#' @param date date (YMD)
#' @param make_range \code{character} forward, backward, or centered (default). Forward will produce a range where input date is the start date
#' input date + by units is the end_date. Backward will produce range where input date is end date and input date - by units is start_date.
#' Centered will put input date directly in the middle start_date= input_date - (by units)/2, end_date = input + (by units)/2
#' @param by \code{numeric} to center date by (i.e 4 signifies start date is 4 units before date input)
#' @param unit \code{numeric}  user can specify "month" or "day"
#' @description take user input and create a start and end date to define a range to filter - particularly for filtering `ee$ImageCollection`s
#' @return vector of length 2 with first entry equal to start date and 2nd entry equal to end date
#'
#' @examples \dontrun{
#' dat <- read_csv(here::here("assesssment_data.csv"))
#' date_range <- date_to_range(date = "2016-07-12",make_range = "center",by = 1,unit = "day")
#' }
#'


date_to_range <-  function(date,make_range="center", by, unit="day"){

  assertthat::assert_that(unit %in% c("day","month"),
                          msg = "unit must by day or month")

  assertthat::assert_that(make_range %in% c("forward","backward","center"),
                          msg = "make_range must be forward, forward, backward, or centered")

  time_unit <- switch(unit,
                      "day"=function(x){lubridate::days(x)},
                      "month"=function(x){lubridate::dmonths(x)},
                      "year"=function(x){lubridate::dyears(x)},
                      NULL
  )


  date <- lubridate::ymd(date)


  if(make_range=="center"){
    start_date <- lubridate::as_date(date - time_unit(by))
    end_date <- lubridate::as_date(date + time_unit(by))
  }
  if(make_range=="forward"){
    start_date <- date
    end_date <- lubridate::as_date(date + time_unit(by))
  }
  if(make_range=="backward"){
    start_date <-  lubridate::as_date(date - time_unit(by))
    end_date <-  date
  }


  c(start_date, end_date)

}
