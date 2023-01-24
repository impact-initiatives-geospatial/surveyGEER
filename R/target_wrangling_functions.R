wrangle_rs_targets_wide <-  function(...){
  list_of_targets <- dplyr::lst(...)

  list_of_targets$col_chirps_rainfall_intensity<- list_of_targets$col_chirps_rainfall_intensity |>
    mutate(
      period_of_interest = case_when (month(date)==5~"30 days",
                                      month(date)==4~"60 days",
                                      month(date)==3~"90 days")
    ) |>
    select(new_uid, parameter,period_of_interest, value) |>
    pivot_wider(names_from =c("parameter","period_of_interest"),values_from = "value")

  list_of_targets$col_prev_3mo_drought_modis_basea <- list_of_targets$col_prev_3mo_drought_modis_basea |>
    mutate(
      period_of_interest= glue::glue("{month(date,label=T,abbr=T)}_{year(date)}")
    ) |>
    select(new_uid, parameter,period_of_interest, value) |>
    pivot_wider(names_sep = ".",names_from =c("parameter","period_of_interest"),values_from = "value")

  list_of_targets[2:length(list_of_targets)] <- list_of_targets[2:length(list_of_targets)]  |>
    purrr::map(.f= function(x){
      if("sf" %in% class(x)){
        x |>
          st_drop_geometry()
      }else{
        x
      }

    })



  rm_cols <- base::setdiff(names(list_of_targets$col_pt_data_clean),"new_uid")

  purrr::map2(.x = list_of_targets,.y= names(list_of_targets), function(x,y){
    if(y!="col_pt_data_clean"){
      x |>
        select(-any_of(c(rm_cols)))
    }else{
      x
    }
  }) |>
    purrr::reduce(left_join,by="new_uid") |>
    select(-any_of("adm_uid"))



}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(targets)
#' tar_load_everything()
#' bla <- dplyr::lst(col_pt_data_clean,
#'     col_chirps_rainfall_intensity,
#'     col_prev_3mo_drought_modis_basea,
#'     col_chirps_spi,
#'     col_dist_to_coast,
#'     col_landforms_reclassified,
#'     col_oxford_access,
#'     col_npp,
#'     col_local_value_merged
#'     )
#'
#' #'
#' }
#'
#'

prep_rs_modis_target <-  function(df,uid_col="new_uid") {
  df |>
    mutate(name = glue::glue("{parameter}_{month(date,abbr=T,label=T)}{year(date)}")) |>
    dplyr::select(dplyr::all_of(c(uid_col, "name", "value")))
}



prep_rs_chirps_intensity_target <-  function(df,moi=5,uid_col="new_uid",...){
  name_suffix <-  lubridate::month(moi,label=T,abbr=T) |> tolower()
  df |>
    dplyr::mutate(
      period_of_interest = case_when (lubridate::month(date)==moi~"30_days",
                                      lubridate::month(date)==moi-1~"60_days",
                                      lubridate::month(date)==moi-2~"90_days")
    ) |>
    dplyr::mutate(
      name= glue::glue("rx{parse_number(parameter)}d_{parse_number(period_of_interest)}d_{name_suffix}")
    ) |>
    dplyr::select(dplyr::all_of(c(uid_col, "name", "value")))

}



compile_rs_numerics_long <- function(...) {
  res_list <- dplyr::lst(...)
  res_list_long <- res_list |>
    map2(
      .y = names(res_list),
      ~ {
        already_long_format<- str_detect(.y,"_prepped$")
        if ("sf" %in% class(.x)) {
        x_df <- .x |>
          st_drop_geometry()
      } else{
        x_df <- .x
      }


        if (already_long_format) {
          numeric_long <- x_df
        }
        if (!already_long_format) {
          numeric_wide <- x_df |>
            select(new_uid, where(is.numeric)) |>
            mutate(across(.cols = everything(), .fns = ~ as.numeric(.x)))
          length_numeric_vars <- numeric_wide |> select(-new_uid) |> colnames() |> length()
          if(length_numeric_vars>0){
          numeric_long <- numeric_wide |>
            pivot_longer(-new_uid)
          }
          if(length_numeric_vars==0){
            numeric_long <-  NULL
          }
        }
        return(numeric_long)
      }
    )
  res_list_long |>
    bind_rows()

}




#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
# not_fun <- compile_rs_categorical_long(col_pt_data_clean,
#                             col_chirps_rainfall_intensity_prepped,
#                             col_prev_3mo_drought_modis_basea_prepped,
#                             col_chirps_spi,
#                             col_dist_to_coast,
#                             col_landforms_reclassified,
#                             col_oxford_access,
#                             col_npp,
#                             col_local_value_merged)

compile_rs_categorical_long <- function(country_code,...){
  res_list <- dplyr::lst(...)
  # class(ee_tidy_ob)<-c("tidyee") # if the below works use class metod here
  res_list[[paste0(country_code,'_chirps_rainfall_intensity_prepped')]] <- NULL
  res_list[[paste0(country_code,'_prev_3mo_drought_modis_basea_prepped')]] <- NULL
  res_list_long <- res_list |>
    map(~{
      if("sf" %in% class(.x)){
        x_df <- .x |>
          st_drop_geometry()
      }else{
        x_df <- .x
      }
      # should build this into target, but dont feel like it
      categorical_wide <- x_df |>
        select(new_uid,where(is.character), where(is.factor)) |>
        mutate(across(.cols=everything(),.fns = ~as.character(.x)))
      return( categorical_wide |>
                pivot_longer(-new_uid)
              )

    })
  res_list_long |>
    bind_rows()
}
  # okay we should put the double pivot after cleaning!!!
  #|>

    # mutate(name=as.character(name)) |>
    # pivot_wider(id_cols = new_uid,
                # names_from = name,
                # values_from = value) |>
    # pivot_longer(-new_uid)





#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
# not_fun <- compile_rs_categorical_long(col_pt_data_clean,
#                                        col_chirps_rainfall_intensity_prepped,
#                                        col_prev_3mo_drought_modis_basea_prepped,
#                                        col_chirps_spi,
#                                        col_dist_to_coast,
#                                        col_landforms_reclassified,
#                                        col_oxford_access,
#                                        col_npp,
#                                        col_local_value_merged)
# clean_categorical_long(not_fun)
# df <- not_fun
clean_categorical_long <-  function(df) {
  df  |>
    filter(
      # remove cols that don't add value
      !str_detect(name, "^country_code"),
      !name %in% c(
      "parameter",
      "name", # this will get rid of the `prepped` ones that are from `ee_extract_tidy`
      "col_lc_2018.insumo",
      "col_lc_2018.confiabili",
      "col_lc_2018.cambio",
      "col_lc_2018.apoyo","id" # basemap)
    )
  ) |>
    pivot_wider(id_cols = new_uid,
                names_from = name,
                values_from = value) |>
    pivot_longer(-new_uid) |>
  mutate(
    name = paste0("rs_", name),
    category = case_when(
      str_detect(name, "landform") ~ "Landform",
      str_detect(name, "erosion") ~ "Erosion",
      str_detect(name, "col_lc_2018") ~ "Land Cover 2018",
      str_detect(name, "col_lulc_2017") ~ "LULC 2017",
      str_detect(name, ".+\\.lz.+$") ~ "Livelihood Zones",
      str_detect(name, "esa_lulc")~ "ESA LULC",
      str_detect(name, "esri_lulc")~ "ESRI LULC",
      TRUE ~ name
    )

  )
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#'
#' col_climate_risk.riesgo_cc numeric or cat
clean_numeric_long <-  function(df){

  df |>
    filter(str_detect(name,"\\.id$|\\.area|\\.codigo"))
  df |>
    filter(
      !name %in% c("adm_uid","col_climate_risk.cod_munici","value"),
      !str_detect(name,"\\.id$|\\.area|\\.codigo")
    ) |>
    # little double pivot trick to fill in values with NA that got pivoted out
    # could use `tidyr::complete()`, but i like this for now
    mutate(name=as.character(name)) |>

    pivot_wider(id_cols = new_uid,
                names_from = name,
                values_from = value) |>
    pivot_longer(-new_uid) |>

    mutate(
      new_uid= as.character(new_uid),
      name=paste0("rs_",name),
      category = case_when(
        str_detect(name,"^rs_precip")~ "Precipitation Intensity",
        str_detect(name,"^rs_rx")~ "Precipitation Intensity",
        str_detect(name,"^NDVI|^VCI")~"Drought MODIS",
        str_detect(name, "^rs_Npp")~ "NPP - Carbon",
        str_detect(name, "spi\\d")~ "Standard Precipitation Index",
        str_detect(name, "healthcare|city")~ "Health Care Accessibility",
        str_detect(name, "^rs_dist")~ "Distance to Coast (m)",
        str_detect(name, "^rs_col_climate_risk")~ "Climate Risk Metrics",
        TRUE~ name
      )
      )
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#'
#' rs_numeric_clean <- compile_clean_rs_numerics(col_pt_data_clean,
#'                                              col_chirps_rainfall_intensity,
#'                                              col_prev_3mo_drought_modis_basea,
#'                                              col_chirps_spi,
#'                                              col_dist_to_coast,
#'                                              col_landforms_reclassified,
#'                                              col_oxford_access,
#'                                              col_npp,
#'                                              col_local_value_merged)
#'

compile_clean_rs_numerics <- function(...){
  numerics_long <- compile_rs_numerics_long(...)
  clean_numeric_long(df = numerics_long)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples \dontrun{
# rs_categorical_clean <- compile_clean_rs_categoricals(col_pt_data_clean,
#'                                              col_chirps_rainfall_intensity,
#'                                              col_prev_3mo_drought_modis_basea,
#'                                              col_chirps_spi,
#'                                              col_dist_to_coast,
#'                                              col_landforms_reclassified,
#'                                              col_oxford_access,
#'                                              col_npp,
#'                                              col_local_value_merged)
#'}

compile_clean_rs_categoricals <- function(country_code,...){
  categorical_long <- compile_rs_categorical_long(country_code=country_code,...)
  clean_categorical_long(df = categorical_long)
}


#' Title
#'
#' @param ...
#'
#' @return
#' @description this output is more useful for my shiny or rmd reporting
#' @export
#'
#' @examples
# rs_indicators_long <- format_rs_indicators_long(
#   col_pt_data_clean,
#   col_chirps_rainfall_intensity_prepped,
#   col_prev_3mo_drought_modis_basea_prepped,
#   col_chirps_spi,
#   col_dist_to_coast,
#   col_landforms_reclassified,
#   col_oxford_access,
#   col_npp,
#   col_local_value_merged
# )

format_rs_indicators_long <- function(country_code,...){
  categoricals_long <- compile_clean_rs_categoricals(country_code=country_code,...)
  numerics_long <- compile_clean_rs_numerics(...)
  list(categorical =categoricals_long,numeric=numerics_long)

}
format_rs_indicators_long_hsmv <- function(country_code,...){
  data_list <- dplyr::lst(...)

  categoricals_long <- compile_clean_rs_categoricals(country_code=country_code,...)
  numerics_long <- compile_clean_rs_numerics(...)
  list(categorical =categoricals_long,numeric=numerics_long)

}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#'  rs_indicators_long <- format_rs_indicators_long(col_pt_data_clean,
#'                                                col_chirps_rainfall_intensity,
#'                                                 col_prev_3mo_drought_modis_basea,
#'                                                 col_chirps_spi,
#'                                                 col_dist_to_coast,
#'                                                 col_landforms_reclassified,
#'                                                 col_oxford_access,
#'                                                 col_npp,
#'                                                 col_local_value_merged)
#'
#' rs_indicators_wide <- format_rs_indicators_wide(rs_indicators_long)
#' }

format_rs_indicators_wide <- function(rs_long){


  cateogricals_wide <- rs_long$categorical |>
    select(-category) |>
    pivot_wider(id_cols = new_uid)

  numerics_wide <- rs_long$numeric |>
    select(-category) |>
    pivot_wider(id_cols = new_uid)
  left_join(numerics_wide,cateogricals_wide, by ="new_uid")

}


# load__hsmv_compiled <-  function(fp){
  # hsmv <-  read_rds(fp)
#   hsmv_split<- split(hsmv,hsmv$country)
#   hsmv_split |>
#     sf::st_as_sf(coords=c("longitude","latitude"))
#
# }
