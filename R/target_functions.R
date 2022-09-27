



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
