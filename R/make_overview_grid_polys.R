#
#


#' batch_hex_grids
#' @description helper function to output all grids used in target reports as 1 file
#'  for later visualization. all params need to be of equal length.
#' @param pt_objects sf object with of class POINT/MULTIPOINT
#' @param country_codes 3 letter country code
#' @param hex_sizes size of hex diameter in meters.
#' @return sf spatial data frame of polygon hex grid with of the specified sizes
#' @export
#'
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' library(sf)

#' # load req objects
#' tar_load_everything()
#' # specific hex sizes
#' hexs <- c(75000,75000,20000,75000,75000,75000)
#' # country codes
#' sel_ccs <- c("col","irq","hti","nga","ner","som")

#' # specify the pt objects to  use in list
#' pt_files <- list(
#'   col_pt_data_clean,
#'   irq_pt_data_clean,
#'   hti_pt_data_clean,
#'   ner_pt_data_clean,
#'   nga_pt_data_clean,
#'   som_pt_data_clean)

#' #create hex's
#' hexs_compiled <- batch_hex_grids(pt_objects = pt_files,
#'                                  country_codes = sel_ccs,
#'                                  hex_sizes =hexs )
#' # hexs_compiled |>
#' #   write_rds("hex_compiled.rds")}
#'

batch_hex_grids <- function(pt_objects,country_codes,hex_sizes){
  purrr::pmap_dfr(list(pt_objects,country_codes,hex_sizes),
              \(pt,cc,hx){
                make_hex_grid(sf_geom = pt,country_code = cc,hex_size = hx) |>
                  mutate(country_code = cc)
              }
              )
}




