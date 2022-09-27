 # Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(rgee)
library(rlang)

rgee::ee_Initialize(drive=T)


# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
# tar_option_set(envir= getNamespace("surveyGEER"))
tar_option_set(
  packages = c("tidyverse",
               "rgee",
               "lubridate",
               # "rstudioapi",
               "here",
               "tidyrgee",
               "sf"
               ),
  # imports = "surveyGEER",
  envir = getNamespace("surveyGEER"),
  # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint
# cntry_code <- c("col")

# Replace the target list below with your own:
# Colombia Targets
list(

# Colombia ----------------------------------------------------------------

  tar_target(
    name = col_pt_data_clean,
    command = load_clean_assessement_points(country_code = "col")
    #   format = "feather" # efficient storage of large data frames # nolint
  ),
  tar_target(
    name=col_oxford_access,
    command= extract_oxford_access_indicators(geom_sf = col_pt_data_clean,img_scale = 928)
  ),
  tar_target(
    name= col_landforms,
    command = extract_geomorph_landform_indicators(col_pt_data_clean,img_scale=90)
  ),
  tar_target(
    name= col_landforms_reclassified,
    command= recode_srtm_alos_categorical(df = col_landforms)
  ),
  tar_target(
    name= col_chirps_rainfall_intensity,
    command= extract_chirps_rain_intensity(geom_sf=col_pt_data_clean,from_when="2022-05-31")
  ),
  tar_target(
    name= col_chirps_rainfall_intensity_prepped,
    command= prep_rs_chirps_intensity_target(col_chirps_rainfall_intensity,moi=5)
  ),
  tar_target(
    name= col_chirps_spi,
    command= extract_spi_to_values(geom_sf=col_pt_data_clean,moi=5)
  ),

  tar_target(
    name= col_npp,
    command= extract_npp_indicators(geom_sf = col_pt_data_clean,img_scale = 500)
  ),

  tar_target(
    # need to build in range to composte over for colombia... 1 month == too cloudy....3 months still 40 % clouds
    # might be worth looking at 6 months and a year.
    name= col_air_quality,
    command= extract_s5p_air_quality(geom_sf = col_pt_data_clean,yoi=2022, moi=5, img_scale=111320)
  ),
  tar_target(
    name= col_dist_to_coast,
    command= extract_dist_to_coast(geom_sf=col_pt_data_clean,country_code = "col",pt_density = 100)
  ),
  tar_target(
    name = col_prev_3mo_drought_modis_basea,
    command = extract_monthly_modis_drought(geom_sf=col_pt_data_clean,
                                            baseline_years = c(2000:2015),
                                            moi = c(3, 4, 5),
                                            yoi = c(2022),
                                            scale = 250,
                                            mask = "cloud&quality",
                                            satellite = "terra",
                                            TAC = T,
                                            temporal_interpolation = T)
  ),
  tar_target(
    name= col_prev_3mo_drought_modis_basea_prepped,
    command = prep_rs_modis_target(col_prev_3mo_drought_modis_basea)
  ),
  tar_target(name= col_local_value,
             command=extract_local_values_to_points(schema = "col",
                                                    country_code = "col",
                                                    geom_sf = col_pt_data_clean)
  ),
  tar_target(
    name= col_local_value_merged,
    command= merge_local_layers(col_local_value)
  ),
  tar_target(
    name = col_rs_indicators_long,
    command= format_rs_indicators_long(country_code = "col",
                                       col_pt_data_clean,
                                       col_chirps_rainfall_intensity_prepped,
                                       col_prev_3mo_drought_modis_basea_prepped,
                                       col_chirps_spi,
                                       col_dist_to_coast,
                                       col_landforms_reclassified,
                                       col_oxford_access,
                                       col_npp,col_air_quality,
                                       col_local_value_merged
    )
  ),
    tar_target(
    name = col_rs_indicators_wide,
    command= format_rs_indicators_wide(col_rs_indicators_long)
  ),

# Nigeria -----------------------------------------------------------------
tar_target(
  name = nga_pt_data_clean,
  command = load_clean_assessement_points(country_code = "nga")
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=nga_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = nga_pt_data_clean,img_scale = 928)
),
tar_target(
  name= nga_landforms,
  command = extract_geomorph_landform_indicators(nga_pt_data_clean,img_scale=90)
),
tar_target(
  name= nga_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = nga_landforms)
),
tar_target(
  name= nga_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=nga_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= nga_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(nga_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= nga_chirps_spi,
  command= extract_spi_to_values(geom_sf=nga_pt_data_clean,moi=5)
),

tar_target(
  name= nga_npp,
  command= extract_npp_indicators(geom_sf = nga_pt_data_clean,img_scale = 500)
),

tar_target(
  name= nga_air_quality,
  command= extract_s5p_air_quality(geom_sf = nga_pt_data_clean,yoi=2022, moi=4, img_scale=111320)
),
tar_target(
  name= nga_growing_season_lengths,
  command= extract_growing_season_length_viirs(geom_sf = nga_pt_data_clean,yoi=2013:2022,scale=500)
),
tar_target(
  name = nga_mo345_veg_basea,
  command = extract_monthly_modis_drought(geom_sf=nga_pt_data_clean,
                                          baseline_years = c(2000:2015),
                                          moi = c(3, 4, 5),
                                          yoi = c(2022),
                                          scale = 250,
                                          mask = "cloud&quality",
                                          satellite = "terra",
                                          TAC = T,
                                          temporal_interpolation = T)
),
tar_target(
  name= nga_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(nga_mo345_veg_basea)
),
# tar_target(
#   name = nga_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# should be sometheing inside chirps_spi/rolling_statistic
# ),
tar_target(name= nga_ndvi_growing_season_z,
           command=extract_modis_ndvi_anomaly(
             geom_sf=nga_pt_data_clean,
             baseline_years = 2000:2021,
             date_range = c("2021-06-20", "2021-09-26"),
             range_label = "growing_season",scale= 250

           )),
tar_target(name= nga_local_value,
           command=extract_local_values_to_points(schema = "public",country_code="nga",
                                                  geom_sf = nga_pt_data_clean)
),

### NGA Local ####

tar_target(
  name= nga_local_value_merged,
  command= merge_local_layers(nga_local_value)
),
tar_target(
  name = nga_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "nga",nga_pt_data_clean,
                                     nga_chirps_rainfall_intensity_prepped,
                                     nga_mo345_veg_basea_prepped,
                                     nga_chirps_spi,
                                     # nga_dist_to_coast,
                                     nga_landforms_reclassified,
                                     nga_oxford_access,
                                     nga_ndvi_growing_season_z,
                                     nga_npp,nga_air_quality,
                                     nga_local_value_merged
  )
),
tar_target(
  name = nga_rs_indicators_wide,
  command= format_rs_indicators_wide(nga_rs_indicators_long)
),
# Iraq -----------------------------------------------------------------
tar_target(
  name = irq_pt_data_clean,
  command = load_clean_assessement_points(country_code = "irq")
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=irq_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = irq_pt_data_clean |>
                                              select(-date_assessment),img_scale = 928)
),
tar_target(
  name= irq_landforms,
  command = extract_geomorph_landform_indicators(irq_pt_data_clean |>
                                                   select(-date_assessment),img_scale=90)
),
tar_target(
  name= irq_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = irq_landforms)
),
tar_target(
  name= irq_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=irq_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= irq_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(irq_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= irq_chirps_spi,
  command= extract_spi_to_values(geom_sf=irq_pt_data_clean |>
                                   select(-date_assessment))
),

tar_target(
  name= irq_npp,
  command= extract_npp_indicators(geom_sf = irq_pt_data_clean |>
                                    select(-date_assessment),
                                  img_scale = 500)
),

tar_target(
  # need to build in range to composte over for colombia... 1 month == too cloudy....3 months still 40 % clouds
  # might be worth looking at 6 months and a year.
  name= irq_air_quality,
  command= extract_s5p_air_quality(geom_sf = irq_pt_data_clean |>
                                     select(-date_assessment),yoi=2022, moi=5, img_scale=111320)
),
tar_target(
  name= irq_dist_to_coast,
  command= extract_dist_to_coast(geom_sf=irq_pt_data_clean |>
                                   select(-date_assessment),country_code = "irq",pt_density = 100)
),
tar_target(
  name = irq_mo345_veg_basea  ,
  command = extract_monthly_modis_drought(geom_sf=irq_pt_data_clean |>
                                            select(-date_assessment),
                                          baseline_years = c(2000:2015),
                                          moi = c(3, 4, 5),
                                          yoi = c(2022),
                                          scale = 250,
                                          mask = "cloud&quality",
                                          satellite = "terra",
                                          TAC = T,
                                          temporal_interpolation = T)
),
tar_target(
  name= irq_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(irq_mo345_veg_basea)
),
tar_target(
  name=irq_closest_water_pixel_perm_prepped,
  command = extract_nearest_water_pixel_distance(y = irq_pt_data_clean |>
                                                   select(-date_assessment),                                                 water_type = "permanent",scale = 30, via="drive")
),
tar_target(
  name = irq_landcover,
  command= extract_landcover_class(geom_sf = irq_pt_data_clean |>
                                     select(-date_assessment),landcover = list("esa","esri"))
),
# tar_target(
#   name = irq_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# should be sometheing inside chirps_spi/rolling_statistic
# ),

### Local ####


# tar_target(name= irq_local_value,
#            command=extract_local_values_to_points(schema = "public",country_code="irq",
#                                                   geom_sf = irq_pt_data_clean)
# ),
# tar_target(
#   name= irq_local_value_merged,
#   command= merge_local_layers(irq_local_value)
# ),
tar_target(
  name = irq_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "irq",irq_pt_data_clean |>
                                       select(-date_assessment),
                                     irq_chirps_rainfall_intensity_prepped,
                                     irq_mo345_veg_basea_prepped,
                                     irq_closest_water_pixel_perm_prepped,
                                     irq_chirps_spi,
                                     # irq_dist_to_coast,
                                     irq_landforms_reclassified,
                                     irq_oxford_access,
                                     irq_npp,
                                     irq_air_quality,
                                     irq_landcover
                                     # irq_local_value_merged
  )
),
tar_target(
  name = irq_rs_indicators_wide,
  command= format_rs_indicators_wide(irq_rs_indicators_long)
),

# SOM  -----------------------------------------------------------------
tar_target(
  name = som_pt_data_clean,
  command = load_clean_assessement_points(country_code = "som")
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=som_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = som_pt_data_clean,img_scale = 928)
),
tar_target(
  name= som_landforms,
  command = extract_geomorph_landform_indicators(som_pt_data_clean ,img_scale=90)
  ),

tar_target(
  name= som_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = som_landforms)
),
tar_target(
  name= som_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=som_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= som_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(som_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= som_chirps_spi,
  command= extract_spi_to_values(geom_sf=som_pt_data_clean,mo_lags= list(1,3,6,9,12),moi=5)
),

# tar_target(
#   name= som_npp,
#   command= extract_npp_indicators(geom_sf = som_pt_data_clean ,
#                                   img_scale = 500)
# ),

tar_target(
  name= som_air_quality,
  command= extract_s5p_air_quality(geom_sf = som_pt_data_clean ,yoi=2022, moi=5, img_scale=111320)
),
tar_target(
  name= som_dist_to_coast,
  command= extract_dist_to_coast(geom_sf=som_pt_data_clean ,country_code = "som",pt_density = 100)
),
tar_target(
  name = som_mo345_veg_basea  ,
  command = extract_monthly_modis_drought(geom_sf=som_pt_data_clean ,
                                          baseline_years = c(2000:2015),
                                          moi = c(3, 4, 5),
                                          yoi = c(2022),
                                          scale = 250,
                                          mask = "cloud&quality",
                                          satellite = "terra",
                                          TAC = T,
                                          temporal_interpolation = T)
),
tar_target(
  name= som_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(som_mo345_veg_basea)
),
tar_target(
  name=som_closest_water_pixel_perm_prepped,
  command = extract_nearest_water_pixel_distance(y = som_pt_data_clean,                                                 water_type = "permanent",scale = 30, via="drive")
),
tar_target(
  name = som_landcover,
  command= extract_landcover_class(geom_sf = som_pt_data_clean,landcover = list("esa","esri"))
),
# tar_target(
#   name = som_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# # should be sometheing inside chirps_spi/rolling_statistic
# # ),
#

#
#
tar_target(name= som_local_value,
           command=extract_local_values_to_points(schema = "public",country_code="som",
                                                  geom_sf = som_pt_data_clean)
),
tar_target(
   name= som_local_value_merged,
   command= merge_local_layers(som_local_value)
),
tar_target(
  name = som_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "som",
                                     som_pt_data_clean,
                                     som_chirps_rainfall_intensity_prepped,
                                     som_mo345_veg_basea_prepped,
                                     som_closest_water_pixel_perm_prepped,
                                     som_chirps_spi,
                                     som_dist_to_coast,
                                     som_landforms_reclassified,
                                     som_oxford_access,
                                     # som_npp,
                                     som_air_quality,
                                     som_landcover,
                                     som_local_value_merged
  )
),
tar_target(
  name = som_rs_indicators_wide,
  command= format_rs_indicators_wide(som_rs_indicators_long)
),
# NER  -----------------------------------------------------------------
tar_target(
  name = ner_pt_data_clean,
  command = load_clean_assessement_points(country_code = "ner") |> select(-today)
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=ner_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = ner_pt_data_clean,img_scale = 928)
),
tar_target(
  name= ner_landforms,
  command = extract_geomorph_landform_indicators(ner_pt_data_clean ,img_scale=90)
  ),

tar_target(
  name= ner_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = ner_landforms)
),
tar_target(
  name= ner_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=ner_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= ner_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(ner_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= ner_chirps_spi,
  command= extract_spi_to_values(geom_sf=ner_pt_data_clean,mo_lags= list(1,3,6,9,12),moi=5)
),

# tar_target(
#   name= ner_npp,
#   command= extract_npp_indicators(geom_sf = ner_pt_data_clean ,
#                                   img_scale = 500)
# ),

tar_target(
  name= ner_air_quality,
  command= extract_s5p_air_quality(geom_sf = ner_pt_data_clean ,yoi=2022, moi=5, img_scale=111320)
),
tar_target(
  name= ner_dist_to_coast,
  command= extract_dist_to_coast(geom_sf=ner_pt_data_clean ,country_code = "som",pt_density = 100)
),
tar_target(
  name = ner_mo345_veg_basea  ,
  command = extract_monthly_modis_drought(geom_sf=ner_pt_data_clean ,
                                          baseline_years = c(2000:2015),
                                          moi = c(3, 4, 5),
                                          yoi = c(2022),
                                          scale = 250,
                                          mask = "cloud&quality",
                                          satellite = "terra",
                                          TAC = T,
                                          temporal_interpolation = T)
),
tar_target(
  name= ner_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(ner_mo345_veg_basea)
),
tar_target(
  name=ner_closest_water_pixel_perm_prepped,
  command = extract_nearest_water_pixel_distance(y = ner_pt_data_clean,                                                 water_type = "permanent",scale = 30, via="drive")
),
tar_target(
  name = ner_landcover,
  command= extract_landcover_class(geom_sf = ner_pt_data_clean,landcover = list("esa","esri"))
),
# tar_target(
#   name = ner_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# # should be sometheing inside chirps_spi/rolling_statistic
# # ),
#

#
#
# tar_target(name= ner_local_value,
#            command=extract_local_values_to_points(schema = "public",country_code="ner",
#                                                   geom_sf = ner_pt_data_clean)
# ),
# tar_target(
#    name= ner_local_value_merged,
#    command= merge_local_layers(ner_local_value)
# ),
tar_target(
  name = ner_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "ner",
                                     ner_pt_data_clean,
                                     ner_chirps_rainfall_intensity_prepped,
                                     ner_mo345_veg_basea_prepped,
                                     ner_closest_water_pixel_perm_prepped,
                                     ner_chirps_spi,
                                     ner_dist_to_coast,
                                     ner_landforms_reclassified,
                                     ner_oxford_access,
                                     # ner_npp,
                                     ner_air_quality,
                                     ner_landcover
                                     # ner_local_value_merged
  )
),
tar_target(
  name = ner_rs_indicators_wide,
  command= format_rs_indicators_wide(ner_rs_indicators_long)
)







)