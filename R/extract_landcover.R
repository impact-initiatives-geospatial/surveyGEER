extract_landcover_class <-  function(geom_sf,landcover=list("esa","esri")){
  assertthat::assert_that(length(landcover[!landcover%in%c("google","esa","esri")] )==0)
  landcover <- landcover |> rlang::set_names(landcover)

  get_lc_ic_link <- function(x){
    switch(x,
           "google"="GOOGLE/DYNAMICWORLD/V1",
           "esa"  = "ESA/WorldCover/v100",
           "esri" ="projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m",
    )
  }

  landcover_ics <- landcover |>
    purrr::map( ~ee$ImageCollection(get_lc_ic_link(.x) ))

  scales_list <- list()
  if("esa" %in% names(landcover_ics)){
    landcover_ics$esa <- ee$Image(landcover_ics$esa$first())$rename("esa_lulc_10m")
    scales_list$esa <-  10
    }
  if("esri" %in% names(landcover_ics)){
    landcover_ics$esri <- landcover_ics$esri$mosaic()$rename("esri_lulc_10m")
    scales_list$esri <- 10
  }
  y_ee <-  rgee::sf_as_ee(x = geom_sf)
  # interesting if we store fc list here as an object we get the tough reticulate bug
  # seems our work around only works directly on fcs/images not once converted to list class
  pt_df_values <- purrr::map(names(landcover_ics),
                               ~{
                                 fc_values <- landcover_ics[[.x]]$sampleRegions(
                                 collection=y_ee,
                                 scale=scales_list[[.x]]
                                 )
                                 # make this an ob so it only needs to go client side once...
                                 # could probably make faster by just accepting size arg which
                                 # we can get from the geom_sf, but using a targets workflow a one
                                 # time long calculation like this is not really a time-limiting step
                                 # to productivity
                                 fc_size_cli <- fc_values$size()$getInfo()
                                 cat(crayon::bgGreen(glue::glue("downloading {.x} landcover to points")),"\n")

                                   if(fc_size_cli>5000){
                                     df_values <-  rgee::ee_as_sf(fc_values,via="drive")
                                   }
                                   if(fc_size_cli<=5000){
                                     df_values <-  rgee::ee_as_sf(fc_values)
                                   }
                                   return(df_values)
                               })  |>
    rlang::set_names(names(landcover_ics))


  pt_values_recoded <- purrr::map(names(pt_df_values),
             ~recode_from_lookup(df = pt_df_values[[.x]],
                                  # something here
                                  df_value = str_subset(colnames(pt_df_values[[.x]]),"_lulc_"),
                                  lookup = ee_landcover_lookup(landcover = .x),
                                  lookup_value = "value",
                                  lookup_label = "category") |>
               # a little clean up - might as well do in this mapping
               sf::st_drop_geometry() |>
               # pretty janky, but if we keep adm_uid here current format functions later will work
               # numeric compilation needs something to pivot
               select(matches("new_uid|.+_lulc_.+$"))
               ) |> rlang::set_names(names(landcover_ics))

  purrr::reduce(pt_values_recoded, full_join, by = "new_uid")

}



recode_from_lookup <- function(df,df_value, lookup,lookup_value,lookup_label){
  lookup_dict <- lookup[[lookup_label]] |>
    rlang::set_names(lookup[[lookup_value]])
  df |>
    mutate(
      !!sym(df_value):= recode(!!sym(df_value),!!!lookup_dict)
    )
}

#' Title
#'
#' @param landcover
#'
#' @return
#' @export
#'
#' @examples

ee_landcover_lookup <- function(landcover){
  if(landcover=="esa"){
    lu_tbl <- tibble::tribble(
      ~value,                 ~category, ~hex,
      10L,                      "Trees","#006400",
      20L,                  "Shrubland","#ffbb22",
      30L,                  "Grassland","#ffff4c",
      40L,                   "Cropland","#f096ff",
      50L,                   "Built-up", "#fa0000",
      60L, "Barren / sparse vegetation","#b4b4b4",
      70L,               "Snow and ice","#f0f0f0",
      80L,                 "Open water","#0064c8",
      90L,         "Herbaceous wetland","#0096a0",
      95L,                  "Mangroves","#00cf75",
      100L,            "Moss and lichen","#fae6a0"
    )
  }
  if(landcover=="google"){
    lu_tbl <- tibble::tribble(
      ~value,           ~category,                                                                 ~description,
      0L,              "Water",                                        "Permanent and seasonal water bodies",
      1L,              "Trees", "Includes primary and secondary forests, as well as large-scale plantations",
      2L,              "Grass",                          "Natural grasslands, livestock pastures, and parks",
      3L, "Flooded vegetation",                                   "Mangroves and other inundated ecosystems",
      4L,              "Crops",                                          "Include row crops and paddy crops",
      5L,      "Shrub & Scrub",                       "Sparse to dense open vegetation consisting of shrubs",
      6L,         "Built Area",               "Low- and high-density buildings, roads, and urban open space",
      7L,        "Bare ground",                                                   "Deserts and exposed rock",
      8L,         "Snow & Ice",                                          "Permanent and seasonal snow cover"
    )
  }
  if(landcover=="esri"){
    lu_tbl <- tibble::tribble(
     ~value,            ~category, ~hex_code,
         1L,            "No Data",         "#FFFFFF",
         2L,              "Water",        "#1A5BAB",
         3L,              "Trees",        "#358221",
         4L,              "Grass",        "#A7D282",
         5L, "Flooded Vegetation",        "#87D19E",
         6L,              "Crops",        "#FFDB5C",
         7L,        "Scrub/Shrub",        "#EECFA8",
         8L,         "Built Area",        "#ED022A",
         9L,        "Bare Ground",        "#EDE9E4",
        10L,           "Snow/Ice",       "#F2FAFF",
        11L,             "Clouds",        "#C8C8C8"
     )
  }
  return(lu_tbl)

}

