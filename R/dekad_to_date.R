#' dekad_to_date
#'
#' @param df data.frame
#' @param year column containing year
#' @param month column containing month
#' @param dekad colum containing dekad
#'
#' @return data.frame with date
#'
#' @examples \dontrun{
#'  library(tidyverse)
#'  library(lubridate)
#'  example_data <- data.frame(year = rep(2021,3), month=rep(1,3),dekad=c(1,2,3))
#'  dekad_to_date(df=example_data)
#'
#' }
dekad_to_date <-  function(df=example_data,year="year", month="month", dekad="dekad"){
  df |>
    dplyr::mutate(
      month =formatC(month,width= 2,flag="0"),
      year_month = glue::glue("{year}-{month}"),
      day_1 = lubridate::ymd(glue::glue("{year}-{month}-01")),
      day= dplyr::case_when(
        dekad==1~"10",
        dekad==2~"20",
        dekad==3~as.character(lubridate::day(lubridate::ceiling_date(day_1, "month") - 1)),
        TRUE~NA_character_
      ),
      date= lubridate::ymd(glue::glue("{year_month}-{day}"))

    )
}
