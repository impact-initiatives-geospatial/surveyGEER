
#' Title
#'
#' @param y
#' @param water_type
#' @param scale
#' @param tidy_extract
#'
#' @return
#' @export
#'
#' @examples
extract_nearest_water_pixel_distance <-  function(y,
                                                  water_type="permanent",
                                                  scale=30,
                                                  tidy_extract=T,
                                                  year_range=c(2017,2020),
                                                  via
                                                  ){
  start_date <- paste0(year_range, "-01-01")[1]
  end_date <- paste0(year_range, "-01-01")[2]
  name_suffix <- glue::glue("{lubridate::year(start_date)}{lubridate::year(end_date)}")

  water_yearly <- rgee::ee$ImageCollection("JRC/GSW1_3/YearlyHistory")
  water_yearly_recent <- water_yearly$filterDate(start_date,end_date)

  boolean_input <-  switch(water_type,
                           "all"=">=",
                           "seasonal"="=",
                           "permanent"="="
  )
  val_input <-  switch(water_type,
                       "all"=2,
                       "seasonal"=2,
                       "permanent"=3
  )
  res <- ee_closest_distance_to_val(x = water_yearly_recent,
                             y = y,
                             boolean = boolean_input,
                             val = val_input,
                             scale = scale,tidy_extract=tidy_extract,via=via)
  res |>
    group_by(new_uid) |>
    summarise(value=mean(value,na.rm=T)) |>
    mutate(name= glue::glue('avg_dist_perm_water_pix_{name_suffix}'))

}
