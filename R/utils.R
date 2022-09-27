
#' quick_db_con
#' @description convenience function to load local postgres db connection
#' @return db connetion
#' @export
#'
#' @examples
quick_db_con <-  function(schema= "public"){
  search_path_prefix <-  "-c search_path="
  search_path <-  paste0(search_path_prefix,schema)

  DBI::dbConnect(RPostgres::Postgres(),
                 dbname = keyring::key_get("postgres_db_name"),
                 user  = keyring::key_get("postgres_user"),
                 password  = keyring::key_get("postgres_pw"),
                 port     = keyring::key_get("postgres_port"),
                 options =search_path
                 )
}


col_local_layer_lookup<- function(){tibble::tribble(
  ~layer_name,                 ~new_layer_name,
  "CAPACIDAD_100K_VF_1",               "col_soil_survey",
  "ag_100k_vocacion_uso_2017",                 "col_lulc_2017",
  "EA_TCNCC_Rs_100K_2017",        "col_climate_risk_index",
  "cobertura_tierra_clc_2018",                   "col_lc_2018",
  "Movimientos_en_masa",       "col_landslide_frequency",
  "Zonas_Amenaza_NSR_10_SGC",                  "col_geo_risk",
  "Suscept_deslizamientos_2010.tif", "col_landslide_susceptibilitiy",
  "Erosion", "col_erosion"
) |>
    mutate(new_layer_name= paste0("rs_",new_layer_name))
}


ee_as_sf_tidy <-  function(geom_sf){
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values)
  }
  return(df_values)
}



