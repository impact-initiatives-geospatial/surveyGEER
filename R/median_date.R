#' calculate median date
#' @param data.frame data frame
#' @param survey_date column names of survey date
#' @return  median survey date
#'
#' @examples \dontrun{
#' dat<- read_csv(here::here("assessment_data.csv"))
#' assessment_date <- median_survey_date(df = dat,survey_date = "survey_date")

#'
#' }


median_survey_date <- function(df, survey_date){
  # add checks
  median_doy =  median(lubridate::yday(lubridate::ymd(df[[survey_date]])))
  median_year= median(lubridate::year(lubridate::ymd(df[[survey_date]])))
  origin_date = glue::glue("{median_year}-01-01")

  lubridate::as_date(median_doy, origin=origin_date)


}

