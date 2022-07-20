#' fetch_msna - helper function to quickly pull msna coordinate files
#'
#' @param country_code \code{character} three letter country code (lower-case)
#' @description return msna data based on suplied country code
#' @return sf data.frame containing msna records
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' df <-  fetch_msna("com")
#' }

fetch_msna <- function(country_code="col"){
  msna_path<- here::here("data/msna")
  msna_files <- list.files(msna_path)
  file_rgx<- glue::glue("_{country_code}\\.rds$")
  # stringr::str_detect(string = msna_files,pattern = file_rgx)
  foi_name<- stringr::str_subset(string = msna_files,pattern = file_rgx)
  foi<- glue::glue("{msna_path}/{foi_name}")
  readr::read_rds(foi) |>
    dplyr::mutate(
      country_code=country_code
    )
}
