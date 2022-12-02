
#' check if geo data has already been set up

is_geo_data_set <-  function(country_code=NULL){
  if(is.null(country_code)){
    cond <- file.exists(here::here("vault/lookup.rds"))| file.exists(here::here("data_share/coords_anonymized.rds"))
  }
  if(!is.null(country_code)){
    lookup_file <- paste0("vault/",country_code,"_lookup.rds")
    coord_anon_file <- paste0("vault/",country_code,"_coords_anonymized.rds")
    cond <- file.exists(here::here(lookup_file))| file.exists(here::here(coord_anon_file))
  }
  return(cond)


}

#' check if data directory has already been set
is_dat_directory_set <-  function(){
  dir.exists(here::here("data_share"))|
    dir.exists(here::here("vault"))
}
