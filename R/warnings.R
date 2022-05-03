aoi_or_grid_warning <-function(aoi=AOI_layer_name,
                               grid=AOI_grid_layer_name){

  if(is.null(aoi)&is.null(grid)){
    warning("You have not provided an AOI or GRID Layer - using data in package for example (SOM boundary)")
  }
}








#' load_aoi_grid
#'
#' @param aoi layer name of AOI
#' @param grid layer name of grid
#' @param hex_diameter \code{numeric} desired diameter of hexagon grid (in meter)
#' @param country_code \code{character} three letter country code (lower case)
#'
#' @return sf class grid
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' grid <- load_aoi_grid()
#' }

load_aoi_grid <-function(aoi=NULL,
                         grid=NULL,
                         hex_diameter= 15000,
                         country_code=NULL){

  if(is.null(aoi)&is.null(grid)){
    warning("You have not provided an AOI or GRID Layer - will use package data (SOM boundary)\n")
    country_code <-  "som"
    if(is.null(hex_diameter)){
      cat("No diameter specified - will used the default of 15 km\n")

      }
    aoi_temp <- surveyGEER::som_boundary|>
      reach_reproject_utm(country_code = country_code)
    grid_final <- sf::st_make_grid(aoi_temp,cellsize = hex_diameter,square = F) |>
      st_transform(crs=4326)
  }
  if(!is.null(grid)){
    cat("Nice you already created a grid - it is being loaded")
    grid_final <-  sf::st_read(paste0("data/",grid))
  }
  if(is.null(grid)&!is.null(aoi)){
    cat("Since you have just supplied an AOI - will load that and create a grid of",hex_diameter,"\n")
    aoi <- sf::st_read("data/",aoi) |>
      summarise() |>
      reach_reproject_utm(country_code = country_code)
    grid_final <- sf::st_make_grid(aoi,cellsize = hex_diameter,square = F) |>
      st_transform(crs=4326)
  }
  grid_final |>
    st_as_sf() |>
    dplyr::mutate(uid=dplyr::row_number()) |>
    dplyr::rename(geometry="x")


}



