
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
    "bgd",32646L,"WGS 84 / UTM zone 46N",
    "som",32639L,"WGS 84/UTM zone 39N",
    "eth",32637L,"WGS 84/UTM zone 37N",
    "col",32719L,"WGS 84 / UTM zone 19S",
    "afg",32642L,"WGS 84 / UTM zone 42N" ,
    "irq",32637L,"WGS 84 / UTM zone 37N",
    "syr",32636L,"WGS 84 / UTM zone 36N",
    "leb",32637L,"WGS 84 / UTM zone 37N",
    "drc",32633L,"WGS 84 / UTM zone 33N",
    "car",2633L,"WGS 84 / UTM zone 33N",
    "ssd",32636L,"WGS 84 / UTM zone 36N",
    "nga",32632L,"WGS 84 / UTM zone 32N",
    "ukr",32636L,"WGS 84 / UTM zone 36N",
    "mli",32629L,"WGS 84/UTM zone 29N",
    "ner",32633L,"WGS 84 / UTM zone 33N",
    "hti", 32618L, "WGS 84 / UTM zone 18N"
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
