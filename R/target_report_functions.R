#' Title
#'
#' @param sf_geom
#' @param country_code
#' @param hex_size
#'
#' @return
#' @export
#'
#' @examples
make_overview_hex_map <-function(sf_geom,
                                 country_code,
                                 hex_size) {

  grid_wgs <- make_hex_grid(sf_geom = sf_geom,country_code = country_code,hex_size = hex_size)

  qpal <-
    leaflet::colorNumeric(
      RColorBrewer::brewer.pal(n = 7, name = "YlOrRd"),
      domain = grid_wgs$num_pts,
      na.color = NA
    )
  leaflet::leaflet() |>
    leaflet::addProviderTiles(provider = "CartoDB.DarkMatter",group = "carto_darkmatter") |>
    leaflet::addPolygons(
      data = grid_wgs,
      color = ~ qpal(num_pts),
      fillColor = ~ qpal(num_pts),
      fillOpacity = 0.7,group = "grid"
    )|>
    leaflet::addLegend(position = "bottomright",
                       pal=qpal,
                       values = grid_wgs$num_pts,
                       title = "# surveys")

}



cat_pct <- function(df,indicator){
  df |>
    filter(name==indicator) |>
    filter(!is.na(value)) |>
    group_by(value) |>
    summarise(count=n()) |>
    ungroup() |>
    mutate(pct=round(100*(count/sum(count)) ,1)) |>
    arrange(desc(pct))

}

make_hex_grid <-  function(sf_geom,
                           country_code,
                           hex_size){
  data_utm <- sf_geom |>
    reach_reproject_utm(country_code = country_code)

  grid_utm <- data_utm |>
    st_make_grid(cellsize = hex_size, square = F) |>
    st_as_sf() |>
    rename(geometry = x) |>
    mutate(uid = row_number())

  pts_per_grid <- data_utm |>
    st_join(grid_utm) |>
    st_drop_geometry() |>
    group_by(uid) |>
    summarise(num_pts = n(),)

  grid_wgs <-  grid_utm |>
    st_transform(crs = 4326) |>
    left_join(pts_per_grid, by="uid") |>
    filter(!is.na(num_pts))

}
