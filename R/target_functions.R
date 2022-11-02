



#' Title
#'
#' @param country_code
#'
#' @return
#' @export
#'
#' @examples
load_clean_assessement_points <-  function(country_code) {
  assessment <-  fetch_msna(country_code)

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "global_gdb",
    user  = keyring::key_get("postgres_user"),
    password  = keyring::key_get("postgres_pw"),
    port     = 5432
  )

  adm0 <- sf::st_read(con, paste0(country_code, "_adm0"))

  adm0_buffered <- adm0 |>
    reach_reproject_utm(country_code = country_code) |>
    sf::st_buffer(dist = 5000) |>
    sf::st_transform(crs = 4326) |>
    dplyr::transmute(adm_uid = row_number())

  assessment_spatial_cleaned <- assessment |>
    sf::st_join(adm0_buffered) |>
    filter(!is.na(adm_uid))
  cat(
    glue::glue(
      "{nrow(assessment)-nrow(assessment_spatial_cleaned)} records removed as they fell > 5km from admin boundary"
    )
  )
  return(assessment_spatial_cleaned)

}
#' Title
#'
#' @param country_code
#' @description redundant function I had to make when COL sent new data - did not want to edit `fetch_msna` as that
#' would caue me to have to create >90 targets ~5-10 hours... instead just make a new silly function. Probably using a method to monitor changes to input file could avoide this
#' @return
#' @export
#'
#' @examples
load_clean_col_assessement_points <-  function(country_code) {
  assessment <-  fetch_col_msna(country_code)
  assessment <- assessment |>
    separate(geometry, into = (c("x","y")), sep =",") |>
    mutate(across(.cols=c("x","y"),~parse_number(.x))) |>
    st_as_sf(coords=(c("x","y")),crs=4326)
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "global_gdb",
    user  = keyring::key_get("postgres_user"),
    password  = keyring::key_get("postgres_pw"),
    port     = 5432
  )

  adm0 <- sf::st_read(con, paste0(country_code, "_adm0"))

  adm0_buffered <- adm0 |>
    reach_reproject_utm(country_code = country_code) |>
    sf::st_buffer(dist = 5000) |>
    sf::st_transform(crs = 4326) |>
    dplyr::transmute(adm_uid = row_number())

  assessment_spatial_cleaned <- assessment |>
    sf::st_join(adm0_buffered) |>
    filter(!is.na(adm_uid))
  cat(
    glue::glue(
      "{nrow(assessment)-nrow(assessment_spatial_cleaned)} records removed as they fell > 5km from admin boundary"
    )
  )
  return(assessment_spatial_cleaned)

}
#' Title
#'
#' @param load_clean_assessement_points2
#' @description redundant function I had to make when COL sent new data - did not want to edit `fetch_msna` as that
#' would caue me to have to create >90 targets ~5-10 hours... instead just make a new silly function. Probably using a method to monitor changes to input file could avoide this
#' @return
#' @export
#'
#' @examples
load_clean_assessement_points2 <-  function(fp,country_code) {
  assessment <-  readr::read_rds(fp) |>
    dplyr::mutate(
      country_code=country_code
    )

  if(!"sf" %in% class(assessment)){
    # kind of specific , but also frequently needed - should just make a func
    assessment <- assessment |>
      separate(geometry, into = (c("x","y")), sep =",") |>
      mutate(across(.cols=c("x","y"),~parse_number(.x))) |>
      st_as_sf(coords=(c("x","y")),crs=4326)
  }


  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "global_gdb",
    user  = keyring::key_get("postgres_user"),
    password  = keyring::key_get("postgres_pw"),
    port     = 5432
  )

  adm0 <- sf::st_read(con, paste0(country_code, "_adm0"))

  adm0_buffered <- adm0 |>
    reach_reproject_utm(country_code = country_code) |>
    sf::st_buffer(dist = 5000) |>
    sf::st_transform(crs = 4326) |>
    dplyr::transmute(adm_uid = row_number())

  assessment_spatial_cleaned <- assessment |>
    sf::st_join(adm0_buffered) |>
    filter(!is.na(adm_uid))
  cat(
    glue::glue(
      "{nrow(assessment)-nrow(assessment_spatial_cleaned)} records removed as they fell > 5km from admin boundary"
    )
  )
  return(assessment_spatial_cleaned)

}
