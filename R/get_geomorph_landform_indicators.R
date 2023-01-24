
#' get_geomorph_landforms
#'
#' @return image or tidyee with image
#' @export
#'
#' @examples
get_geomorph_landform_indicators <-  function(return_tidyee=F){
  srtm_landforms = ee$Image("CSP/ERGo/1_0/Global/SRTM_landforms")
  alos_landforms = ee$Image("CSP/ERGo/1_0/Global/ALOS_landforms")

  res <- srtm_landforms$rename("srtm_landforms")$
    addBands(alos_landforms$rename("alos_landforms"))
  if(return_tidyee){
    res <- as_tidyee(res)
  }

  return(res)

}

extract_geomorph_landform_indicators<- function(geom_sf,img_scale){
  geom_ee <-  rgee::sf_as_ee(geom_sf)
  img <- get_geomorph_landform_indicators()
  fc_values <- img$sampleRegions(collection= geom_ee,scale= img_scale)
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values,maxFeatures = 5000)
  }
  return(df_values)


}



srtm_landform_lookup <- tibble::tribble(
  ~value,    ~color,         ~description,
  11L, "#141414",  "Peak/ridge (warm)",
  12L, "#383838",         "Peak/ridge",
  13L, "#808080",  "Peak/ridge (cool)",
  14L, "#EBEB8F",    "Mountain/divide",
  15L, "#F7D311",              "Cliff",
  21L, "#AA0000", "Upper slope (warm)",
  22L, "#D89382",        "Upper slope",
  23L, "#DDC9C9", "Upper slope (cool)",
  24L, "#DCCDCE", "Upper slope (flat)",
  31L, "#1C6330", "Lower slope (warm)",
  32L, "#68AA63",        "Lower slope",
  33L, "#B5C98E", "Lower slope (cool)",
  34L, "#E1F0E5", "Lower slope (flat)",
  41L, "#a975ba",             "Valley",
  42L, "#6f198c",    "Valley (narrow)"
)


alos_landform_lookup <- tibble::tribble(
  ~value,    ~color,         ~description,
  11L, "#141414",  "Peak/ridge (warm)",
  12L, "#383838",         "Peak/ridge",
  13L, "#808080",  "Peak/ridge (cool)",
  14L, "#EBEB8F",    "Mountain/divide",
  15L, "#F7D311",              "Cliff",
  21L, "#AA0000", "Upper slope (warm)",
  22L, "#D89382",        "Upper slope",
  23L, "#DDC9C9", "Upper slope (cool)",
  24L, "#DCCDCE", "Upper slope (flat)",
  31L, "#1C6330", "Lower slope (warm)",
  32L, "#68AA63",        "Lower slope",
  33L, "#B5C98E", "Lower slope (cool)",
  34L, "#E1F0E5", "Lower slope (flat)",
  41L, "#a975ba",             "Valley",
  42L, "#6f198c",    "Valley (narrow)"
)



recode_from_gee_lookup_table <- function(df,col, lookup){
  lookup_dict <- lookup[["description"]] |>
    rlang::set_names(lookup[["value"]])
  df |>
    mutate(
      {{col}} := recode({{col}},!!!lookup_dict)
    )
}

recode_srtm_alos_categorical <-  function(df){
  df |>
    recode_from_gee_lookup_table(col = alos_landforms,lookup = alos_landform_lookup) |>
    recode_from_gee_lookup_table(col=srtm_landforms,lookup=srtm_landform_lookup)
}

