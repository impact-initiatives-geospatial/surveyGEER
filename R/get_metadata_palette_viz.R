#' helper to extract metadata for default viz (only for palette, not gamma)
#'
#' @param img image or image collection
#' @param bands bands to visualize
#'
#' @return visParams object ready to be added to `Map$addLayer()`
#' @description helper to extract palette and min/max values when layer comes with default values. I am sure there is a way to do this GEE, but I could not find it so I made my own. Pretty sure this is also available in geemap
#' @export
#'


get_metadata_palette_viz <-  function(img,bands){
  palette <- img$get("system:visualization_0_palette")
  min <-  img$get("system:visualization_0_min")
  max <-  img$get("system:visualization_0_max")
  return(list(min=min, max=max, palette=palette, bands=bands))
}
