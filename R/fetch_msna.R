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
#' fetch_msna_path - helper function to quickly pull msna coordinate file path
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

fetch_msna_path <- function(country_code="col"){
  msna_path<- here::here("data/msna")
  msna_files <- list.files(msna_path)
  file_rgx<- glue::glue("_{country_code}\\.rds$")
  # stringr::str_detect(string = msna_files,pattern = file_rgx)
  foi_name<- stringr::str_subset(string = msna_files,pattern = file_rgx)
  foi<- glue::glue("{msna_path}/{foi_name}")
  return(foi)
}

#' Title
#'
#' @param country_code
#'
#' @return
#' @description redundant function I had to make when COL sent new data - did not want to edit `fetch_msna` as that
#' would caue me to have to create >90 targets ~5-10 hours... instead just make a new silly function. Probably using a method to monitor changes to input file could avoide this
#' @export
#'
#' @examples
fetch_col_msna <- function(country_code="col"){
  msna_path<- here::here("data/msna")
  foi <- file.path(msna_path,"20220916_coords_anonymized_col.rds")
  readr::read_rds(foi) |>
    dplyr::mutate(
      country_code=country_code
    )
}

#' Title
#'
#' @param country_code
#'
#' @return
#' @description redundant function I had to make when COL sent new data - did not want to edit `fetch_msna` as that
#' would caue me to have to create >90 targets ~5-10 hours... instead just make a new silly function. Probably using a method to monitor changes to input file could avoide this
#' @export
#'
#' @examples
fetch_nga_msna <- function(fp,country_code="nga"){
  # msna_path<- here::here("data/msna")
  # foi <- file.path(msna_path,"20220916_coords_anonymized_col.rds")

  readr::read_rds(fp) |>
    dplyr::mutate(
      country_code=country_code
    )
}
