## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE)


# not actually going to `usethis::use_data` for all the data sets in here, but I thought it would still be nice to have spot to store data wrangling procedures that were done to create `.rds` files



# here is how HSMV data were compiled
library(tidyverse)

tab_names <- list(BFA="Clean Data+GPS",CAR="Clean Data+GPS",DRC= "Clean Data", SSD= "Clean Data+GPS")
hsmv <-
  purrr::map2_dfr(.x =list.files("data/hsmv"),
                  .y = tab_names,
                  \(filename, tab){
                    filepath <-  glue::glue("data/hsmv/{filename}")
                    clean_data <- readxl::read_excel(filepath, tab) |>
                      janitor::clean_names()
                    cat(filename , "has",nrow(clean_data),"records\n")
                    clean_data |>
                      select(contains("uuid"), contains("hdx"), matches("*longitude*|*latitude*")) |>
                      mutate(file_name = filename)
                  }
  )


hsmv_coords <- hsmv |>
  transmute(

    country = str_extract(file_name,"DRC|CAR|BFA|SSD"),
    uuid=paste0(country,"_",uuid),
    latitude= case_when(
      country %in% c("CAR")~gps_consolidation_kobo_hdx_latitude,
      country %in% c("SSD")~gps_consolidated_kobo_hdx_latitude,
      country %in% c("BFA")~point_gps_hdx_coordinates_latitude,
      country %in% c("DRC")~point_gps_latitude
    ),
    longitude= case_when(
      country %in% c("CAR")~gps_consolidation_kobo_hdx_longitude,
      country %in% c("SSD")~gps_consolidated_kobo_hdx_longitude,
      country %in% c("BFA")~point_gps_external_hdx_longitude,
      country %in% c("DRC")~point_gps_longitude
    )
  )

hsmv_coords_summary <- hsmv_coords  |>
  group_by(country) |>
  summarise(
    num_records= n(),
    missing_coordinates= sum(is.na(`longitude`)),
    pct_missing = missing_coordinates/num_records
  )


# hsmv_coords |>
#   filter(!is.na(latitude)) |>
#   write_rds("data/hsmv/hsmv_bfa_car_drc_ssd_coords.rds")


# I do wan't to see the deal with SSD data
dir("data/hsmv")

ssd_hsmv <- readxl::read_excel("data/hsmv/Data_Cleaning_GLO_2201_Kobo tool_HSM VS_C1_SSD_20221117_VALIDATED_gps_v2_xml.xlsm",
                   "Clean Data+GPS")

library(janitor)
library(sf)
ssd_sp <- ssd_hsmv |>
  clean_names() |>
  rename(
   longitude="gps_consolidated_kobo_hdx_longitude",
   latitude="gps_consolidated_kobo_hdx_latitude"
  ) |>
  select(longitude,latitude) |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords=c("longitude", "latitude"),crs=4326)
  # filter(!is.na(longitude)) |>
  # group_by() |>
  # distinct(info_state, info_county,info_settlement_final)
library(leaflet)


leaflet() |>
  addTiles() |>
  addCircleMarkers(data=ssd_sp)
