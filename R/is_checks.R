
#' check if geo data has already been set up

is_geo_data_set <-  function(){
  file.exists(here::here("vault/lookup.rds"))| file.exists(here::here("data_share/coords_anonymized.rds.rds"))

}

#' check if data directory has already been set
is_dat_directory_set <-  function(){
  dir.exists(here::here("data_share"))|
    dir.exists(here::here("vault"))
}
