extract_main_continent_poly <-  function(geom_sf){
  # load adm0 data
  con <-  quick_db_con()
  sf::sf_use_s2(use_s2 = T)
  adm0 <- sf::st_read(con, "world_adm0_no_lakes_natural_earth" )
  adm0 <- sf::st_make_valid(adm0)
  # dissolve by continent
  continents_dissolved <- adm0 |>
    group_by(continent) |>
    summarise()
  # fine continent where points are
  continent_name <- geom_sf |>
    st_join(continents_dissolved) |>
    st_drop_geometry() |>
    group_by(continent) |>
    summarise(sum=n()) |>
    filter(sum==max(sum,na.rm=T)) |>
    pull(continent)

  # extract continent polygon
  coi <- continents_dissolved |>
    filter(continent==continent_name)

  # convert to single poly
  coi_poly <- coi |>
    st_cast("POLYGON") |>
    mutate(uid= row_number())

  # extract single poly of interest
  poly_main_uid <- geom_sf |>
    st_join(coi_poly) |>
    st_drop_geometry() |>
    group_by(uid) |>
    summarise(sum=n()) |>
    filter(sum==max(sum,na.rm=T)) |>
    pull(uid)

  # return main polygon for continent of interest
  coi_poly_main <- coi_poly |>
    filter(uid==poly_main_uid)
  return(coi_poly_main)


}

poly_to_points <- function(poly, pt_density,  country_code){
  coi_poly_main_utm <- poly |>
    reach_reproject_utm(country_code)
  # convert poly to linestring
  coi_linestr<- coi_poly_main_utm |>
    st_cast("LINESTRING")
  # sample on line 1 point/100 m
  coi_multipt <- coi_linestr |>
    st_line_sample(density = 1/pt_density)

  coi_pt <- coi_multipt |>
    st_cast("POINT")
  return(coi_pt)
}


extract_dist_to_coast <- function(geom_sf, country_code,pt_density){
  # get polygon of continetn of interest
  coi_main_poly <- extract_main_continent_poly(geom_sf = geom_sf)

  pts_utm <- geom_sf |>
    reach_reproject_utm(country_code)

  coi_pts <- poly_to_points(poly = coi_main_poly,
                            pt_density = pt_density,
                            country_code=country_code)

  nearest_idx<- sf::st_nearest_feature(x = pts_utm,coi_pts)
  closest_distances <- st_distance(pts_utm,coi_pts[nearest_idx],by_element = T)
  pts_utm |>
    sf::st_drop_geometry() |>
    mutate(dist_coast=closest_distances)
}








