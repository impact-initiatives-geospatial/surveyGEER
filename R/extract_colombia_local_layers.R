load_colombia_local_layers <-  function(schema="col"){
  con <- quick_db_con(schema = schema)
  layer_names <- DBI::dbListTables(con)
  sf_obs <- layer_names |>
    purrr::map(
      ~{
        cat(crayon::bgBlue(glue::glue("loading {.x} layer"),"\n"))
        st_read(con,.x)
      }
    ) |>
    set_names(layer_names)
  return(sf_obs)
}

load_nga_local_layers <-  function(schema="public"){
  con <- quick_db_con(schema = schema)
  sf::st_read(con,"nga_lhz") |>
    dplyr::rename(geometry="geom") |>
    dplyr::select(nga.lzcode=lzcode,
           nga.lznameend= lznameen,
           nga.lzclass= class)

}
load_som_local_layers <-  function(schema="public"){
  con <- quick_db_con(schema = schema)
  sf::st_read(con,"som_lhz") |>
    dplyr::rename(geometry="geom") |>
    dplyr::select(som.lzcode=lzcode,
           som.lznameend= lznameen,
           som.lzclass= class,
           som.lzcode_lznameen= lzcode_lznameen)

}



clean_local_colombia_col_names <-  function(x){
  rm_cols <-  c("objectid","municipio","shape_length","shape_leng","shape_area","area_km2","area_ha","rule_id","ruleid" ,"departamen")
  layers_cleaned <- x |>
    # remove any problematic layers
    purrr::keep(.p = ~nrow(.x)>0) |>
    # rename any geoms to geometry -- seems to be better for EE
    purrr::map(~{
      if("geom" %in% colnames(.x)){
        x2 <- .x |>
          rename(
            geometry="geom"
          )
      }else{x2 <- .x}
      x2 |>
        select(-any_of(rm_cols))

    }
    )
  layer_names<- names(layers_cleaned)

  layers_renamed <- layers_cleaned |>
    purrr::map2(.y=layer_names,
                function(x,y){
                  x|> # negative look-around regex only works with perl = T
                    rename_with(.cols = matches("^(?!geometry).*$",perl = T),~glue::glue("{y}.{.x}"))
                }
    )
  return(layers_renamed)




}


join_col_pts_to_local_layers <- function(geom_sf,x, country_code){
  pts_utm <- geom_sf |>
    reach_reproject_utm(country_code= country_code)

  if("list" %in% class(x)){
    layers_utm <- x |>
      purrr::map(~.x |>
                   reach_reproject_utm(country_code = country_code))
    pts_joined <-  layers_utm |>
      purrr::map(~st_join(pts_utm, .x))
  }
  if(!"list" %in% class(x)){
    layers_utm <- x |>
      reach_reproject_utm(country_code = country_code)

    pts_joined <- st_join(pts_utm, layers_utm)
  }
  return(pts_joined)

}



extract_local_values_to_points <-  function(schema="col",country_code="col",geom_sf){
  if(schema=="col"&country_code=="col"){
    cat(crayon::bgGreen("loading layers from postgres\n"))
    layers <- load_colombia_local_layers(schema = schema)
    cat(crayon::bgGreen("cleaning layer names\n"))
    layers_clean <- clean_local_colombia_col_names(x = layers)
    cat(crayon::bgGreen("joining layers\n"))
    res <- join_col_pts_to_local_layers(geom_sf = geom_sf ,x=layers_clean,country_code = schema)
  }
  if(schema=="public" & country_code=="nga"){
    layers_clean <- load_nga_local_layers(schema = schema)
    schema <-  country_code
    res <- join_col_pts_to_local_layers(geom_sf = geom_sf ,x=layers_clean,country_code = country_code)
  }
  if(schema=="public" & country_code=="som"){
    layers_clean <- load_som_local_layers(schema = schema)
    schema <-  country_code
    res <- join_col_pts_to_local_layers(geom_sf = geom_sf ,x=layers_clean,country_code = country_code)
  }

  return(res)

}





merge_local_layers <-  function(local_layer){

  if("list" %in% class(local_layer)){
    dfs<-local_layer |>
      purrr::map(~.x |>
                   select(-any_of(c("adm_uid"))) |>
                   sf::st_drop_geometry()
      )
    res <- dfs |>
      purrr::reduce(left_join,by="new_uid")

  }else{
    res <- local_layer
  }
  return(res)
}
