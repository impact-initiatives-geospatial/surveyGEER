#' setup geo data
#' @description set up survey data for RS extraction
#' @param df data.frame
#' @param uuid \code{character} uuid column name
#' @param lon \code{numeric} longitude column name
#' @param lat \code{numeric} latitude column name
#' @return a list containing a simple feature spatial object with a new unique ID (geo data)
#' @export
#'
#' @exampes \dontrun{
#' dat<- read_csv(here::here("dat/assessment_data.csv"))
#' setup_geo_data(df = dat,
#'               uuid = "_uuid",
#'               lon = "_gps_reading_longitude",
#'               lat = "_gps_reading_latitude")
#' }


setup_geo_data <- function(df,
                           uuid,
                           lon,
                           lat,
                           pw){
  # add some checks
  assertthat::assert_that(
    !is_geo_data_set(),
    msg = "geo data is already set. If you mean to re-run please remove lookup.rds and coords_anonymized.rds from the vault & data_share folders (or just delete the folders)"
    )


  num_records <-  nrow(df)

  df_new_uid <- df |>
    sample_n(size = num_records) |>
    mutate(
      new_uid = row_number()
    )

  lookup_table <- df_new_uid |>
    select(uuid, new_uid)

  geo_data <-  df_new_uid |>
    select(new_uid, lon, lat) |>
    sf::st_as_sf(coords=c(lon,lat), crs= 4326)

  if(!is_dat_directory_set()){
    cat(crayon::green("no 'data_share' or 'vault'  folder - creating them in your project directory\n"))
    dir.create(here::here("data_share"))
    dir.create(here::here("vault"))
  }

  if(!is_geo_data_set()){
    cat(crayon::green("writing anonymized coordinates to data_share folder and encrypted minimal lookup table to vault"))
    saveRDS(lookup_table,here::here("vault/lookup.rds"))
    saveRDS(geo_data,here::here("data_share/coords_anonymized.rds"))


  }


}
