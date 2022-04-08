# usethis::use_build_ignore(files = "dev_history.R")


library(sf)
dat <- read_csv(here::here("../../HH_HC_MSNA.csv"))

dat2 <- dat |>
  select(`_uuid`,lon = `_gps_reading_longitude`,lat= `_gps_reading_latitude`,informed_consent,survey_date, end_survey,electricity_grid,
         solar_light,illness_HH_count,`cooking_fuel/collected_firewood`,
         `income_source/agricultural_production_sale`  ,
         agricultural_land ,
         `employment_source/agricultural_casual`,
         `employment_source/non_agricultural_casual`,
         `employment_source/fishing` ) |>
  filter(informed_consent=="yes")

dat_sf <- dat2 |>
  st_as_sf(crs=4326,coords= c("lon","lat")) |>
  st_transform(crs=32646) |>
  st_jitter(amount = 150) |>
  st_transform(crs=4326 )

dat_df <- dat_sf |>
  st_drop_geometry() |>
  cbind(st_coordinates(dat_sf)) |>
  tibble() |>
  rename( `_gps_reading_latitude`=Y,
          `_gps_reading_longitude`=X  )
dat_df |> glimpse()
dat <- dat_df
usethis::use_data(dat,overwrite=T
)
write_csv(dat,"data/example_raw_data.csv")

usethis::use_git_ignore("data/dat.rda")
usethis::use_git_ignore("data/example_raw_data.csv")
usethis::use_build_ignore("data/example_raw_data.csv")
usethis::use_build_ignore("data/dat.rda")
