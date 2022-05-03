
#' epsg_lookup
#'
#' @return epsg lookup table
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' epsg_lookup_table <-  epsg_lookup()
#' }
epsg_lookup <- function(){
  tibble::tribble(
    ~country_code_lookup, ~epsg_code,              ~wgs84_utm,
    "bgd",     32646L,                      NA,
    "som",     32639L, "WGS 84/UTM zone 39N",
    "eth",     32637L, "WGS 84/UTM zone 37N",
    "col",         NA,                      NA,
    "afg",         NA,                      NA,
    "irq",         NA,                      NA,
    "syr",         NA,                      NA,
    "leb",         NA,                      NA,
    "drc",         NA,                      NA,
    "car",         NA,                      NA,
    "ssd",         NA,                      NA,
    "nga",         NA,                      NA,
    "ukr",         NA,                      NA,
    "mli",     32629L, "WGS 84/UTM zone 29N"
  )}




#' reach_epsg
#'
#' @param country_code
#'
#' @return \code{numeric} epsg code
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' reach_epsg("bgd")
#' }

reach_epsg <- function(country_code){
  country_code <- tolower(country_code)
  lookup <- epsg_lookup()
  assertthat::assert_that(country_code %in% lookup$country_code_lookup,
                          msg = "country_code does not match db of country codes")
  lookup |>
    dplyr::filter(country_code_lookup==country_code) |>
    dplyr::pull(epsg_code) |>
    as.numeric()
}


#' reach_reproject_utm
#'
#' @param x sf or sfc object
#' @param country_code lower case country code (i.e "bgd","som")
#' @return x reprojected to specific WGS 84 UTM Zone
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' som_boundary <- surveyGEER::som_boundary
#' som_boundary |>
#'     reach_reproject_utm(country_code = "som")
#' }
reach_reproject_utm <- function(x,country_code){
  country_code <- reach_epsg(country_code)
  sf::st_transform(x,crs=country_code)
}
