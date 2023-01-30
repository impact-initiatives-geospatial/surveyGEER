 # Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(rgee)
library(rlang)
library(dplyr)
# test gh

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
  # envir = getNamespace("surveyGEER"),
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
    command = load_clean_col_assessement_points(country_code = "col")
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
tar_target(nga_file,
           "data/msna/20221102_nga_coords_anonymized.rds", format = "file"
           ),
tar_target(
  name = nga_pt_data_clean,
  command = load_clean_assessement_points2(fp=nga_file,country_code = "nga")
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
tar_target(
  name= som_chirps_spi_june,
  command= extract_spi_to_values(geom_sf=som_pt_data_clean,mo_lags= list(1,3,6,9,12),moi=6)
),
tar_target(
  name= som_chirps_spi_july,
  command= extract_spi_to_values(geom_sf=som_pt_data_clean,mo_lags= list(1,3,6,9,12),moi=7)
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
  name = som_mo678_veg_basea  ,
  command = extract_monthly_modis_drought(geom_sf=som_pt_data_clean ,
                                          baseline_years = c(2000:2015),
                                          moi = c(6,7,8),
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
  name= som_mo678_veg_basea_prepped,
  command = prep_rs_modis_target(som_mo678_veg_basea)
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
                                     som_mo678_veg_basea_prepped,
                                     som_closest_water_pixel_perm_prepped,
                                     som_chirps_spi,
                                     som_chirps_spi_june,
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
),
# HTI  -----------------------------------------------------------------
tar_target(
  name = hti_pt_data_clean,
  command = load_clean_assessement_points(country_code = "hti")
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=hti_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = hti_pt_data_clean,img_scale = 928)
),
tar_target(
  name= hti_landforms,
  command = extract_geomorph_landform_indicators(hti_pt_data_clean ,img_scale=90)
  ),

tar_target(
  name= hti_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = hti_landforms)
),
tar_target(
  name= hti_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=hti_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= hti_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(hti_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= hti_chirps_spi,
  command= extract_spi_to_values(geom_sf=hti_pt_data_clean,mo_lags= list(1,3,6,9,12),moi=5)
),

tar_target(
  name= hti_npp,
  command= extract_npp_indicators(geom_sf = hti_pt_data_clean ,
                                  img_scale = 500)
),

tar_target(
  name= hti_air_quality,
  command= extract_s5p_air_quality(geom_sf = hti_pt_data_clean ,yoi=2022, moi=5, img_scale=111320)
),
tar_target(
  name= hti_dist_to_coast,
  command= extract_dist_to_coast(geom_sf=hti_pt_data_clean ,country_code = "som",pt_density = 100)
),
tar_target(
  name = hti_mo345_veg_basea  ,
  command = extract_monthly_modis_drought(geom_sf=hti_pt_data_clean ,
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
  name= hti_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(hti_mo345_veg_basea)
),
tar_target(
  name=hti_closest_water_pixel_perm_prepped,
  command = extract_nearest_water_pixel_distance(y = hti_pt_data_clean,                                                 water_type = "permanent",scale = 30, via="drive")
),
tar_target(
  name = hti_landcover,
  command= extract_landcover_class(geom_sf = hti_pt_data_clean,landcover = list("esa","esri"))
),
# tar_target(
#   name = hti_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# # should be sometheing inside chirps_spi/rolling_statistic
# # ),
#

#
#
# tar_target(name= hti_local_value,
#            command=extract_local_values_to_points(schema = "public",country_code="hti",
#                                                   geom_sf = hti_pt_data_clean)
# ),
# tar_target(
#    name= hti_local_value_merged,
#    command= merge_local_layers(hti_local_value)
# ),
tar_target(
  name = hti_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "hti",
                                     hti_pt_data_clean,
                                     hti_chirps_rainfall_intensity_prepped,
                                     hti_mo345_veg_basea_prepped,
                                     hti_closest_water_pixel_perm_prepped,
                                     hti_chirps_spi,
                                     hti_dist_to_coast,
                                     hti_landforms_reclassified,
                                     hti_oxford_access,
                                     hti_npp,
                                     hti_air_quality,
                                     hti_landcover
                                     # hti_local_value_merged
  )
),
tar_target(
  name = hti_rs_indicators_wide,
  command= format_rs_indicators_wide(hti_rs_indicators_long)
),

# HSMV - CAR --------------------------------------------------------------


tar_target(car_hsmv_file,
           fetch_msna_path(country_code = "car"), format = "file"
           ),
tar_target(
  name = car_hsmv_pt_data_clean,
  command = load_clean_assessement_points2(fp=car_hsmv_file,country_code = "car")
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=car_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = car_hsmv_pt_data_clean,img_scale = 928)
),
tar_target(
  name= car_landforms,
  command = extract_geomorph_landform_indicators(car_hsmv_pt_data_clean,img_scale=90)
),
tar_target(
  name= car_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = car_landforms)
),
tar_target(
  name= car_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=car_hsmv_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= car_chirps_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(car_chirps_rainfall_intensity,moi=5)
),
tar_target(
  name= car_chirps_spi,
  command= extract_spi_to_values(geom_sf=car_hsmv_pt_data_clean,moi=5)
),

tar_target(
  name= car_npp,
  command= extract_npp_indicators(geom_sf = car_hsmv_pt_data_clean,img_scale = 500)
),

tar_target(
  name= car_air_quality,
  command= extract_s5p_air_quality(geom_sf = car_hsmv_pt_data_clean,yoi=2022, moi=4, img_scale=111320)
),
tar_target(
  name= car_growing_season_lengths,
  command= extract_growing_season_length_viirs(geom_sf = car_hsmv_pt_data_clean,yoi=2013:2022,scale=500)
),
tar_target(
  name = car_mo345_veg_basea,
  command = extract_monthly_modis_drought(geom_sf=car_hsmv_pt_data_clean,
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
  name= car_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(car_mo345_veg_basea)
),
# tar_target(
#   name = car_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# should be sometheing inside chirps_spi/rolling_statistic
# ),
tar_target(name= car_ndvi_growing_season_z,
           command=extract_modis_ndvi_anomaly(
             geom_sf=car_hsmv_pt_data_clean,
             baseline_years = 2000:2021,
             date_range = c("2021-06-20", "2021-09-26"),
             range_label = "growing_season",scale= 250

           )),
tar_target(
  name = car_landcover,
  command= extract_landcover_class(geom_sf = car_hsmv_pt_data_clean,landcover = list("esa","esri"))
),
### car Local ####
# no local recieveid

tar_target(
  name = car_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "car",
                                     car_hsmv_pt_data_clean,
                                     car_chirps_rainfall_intensity_prepped,
                                     car_mo345_veg_basea_prepped,
                                     car_chirps_spi,
                                     # car_dist_to_coast,
                                     car_landcover,
                                     car_landforms_reclassified,
                                     car_oxford_access,
                                     car_ndvi_growing_season_z,
                                     car_npp,
                                     car_air_quality
                                     # car_local_value_merged
  )
),
tar_target(
  name = car_rs_indicators_wide,
  command= format_rs_indicators_wide(car_rs_indicators_long)
),
  # HSMV - Compiled -------------------------------------------------------------

tar_target(hsmv_compiled_file,
           "data_share/hsmv_compiled_coords_anonymized.rds", format = "file"
           ),
tar_target(
  name = hsmv_compiled_pt_data_clean,
  command =read_rds(hsmv_compiled_file)
  #   format = "feather" # efficient storage of large data frames # nolint
),
tar_target(
  name=hsmv_compiled_oxford_access,
  command= extract_oxford_access_indicators(geom_sf = hsmv_compiled_pt_data_clean,img_scale = 928)
),
tar_target(
  name= hsmv_compiled_landforms,
  command = extract_geomorph_landform_indicators(hsmv_compiled_pt_data_clean,img_scale=90)
),
tar_target(
  name= hsmv_compiled_landforms_reclassified,
  command= recode_srtm_alos_categorical(df = hsmv_compiled_landforms)
),
tar_target(
  name= hsmv_compiled_chirps_rainfall_intensity,
  command= extract_chirps_rain_intensity(geom_sf=hsmv_compiled_pt_data_clean,from_when="2022-05-31")
),
tar_target(
  name= hsmv_compiled_rainfall_intensity_prepped,
  command= prep_rs_chirps_intensity_target(hsmv_compiled_chirps_rainfall_intensity,moi=5)
),
# this is hardcoded to 2022
tar_target(
  name= hsmv_compiled_chirps_spi,
  command= extract_spi_to_values(geom_sf=hsmv_compiled_pt_data_clean,moi=5)
),
# made a new func to user input on year
tar_target(
  name= hsmv_compiled_chirps_spi_may21,
  command= extract_spi_to_values2(x =ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY"),
                                  geom_sf=hsmv_compiled_pt_data_clean,
                                  moi=5,
                                  mo_lags = c(1,2,3),
                                  yoi=2021)
),
tar_target(
  name= hsmv_compiled_chirps_spi_aug21,
  command= extract_spi_to_values2(x =ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY"),
                                  geom_sf=hsmv_compiled_pt_data_clean,
                                  moi=8,
                                  mo_lags = c(1,2,3),
                                  yoi=2021)
),
tar_target(
  name= hsmv_compiled_chirps_spi_july22,
  command= extract_spi_to_values2(x =ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY"),
                                  geom_sf=hsmv_compiled_pt_data_clean,
                                  moi=7,
                                  mo_lags = c(1,2,3),
                                  yoi=2022)
),

tar_target(
  name= hsmv_compiled_npp,
  command= extract_npp_indicators(geom_sf = hsmv_compiled_pt_data_clean,img_scale = 500)
),

tar_target(
  name= hsmv_compiled_air_quality,
  command= extract_s5p_air_quality(geom_sf = hsmv_compiled_pt_data_clean,yoi=2022, moi=4, img_scale=111320)
),
tar_target(
  name= hsmv_compiled_growing_season_lengths,
  command= extract_growing_season_length_viirs(geom_sf = hsmv_compiled_pt_data_clean,yoi=2013:2022,scale=500)
),
tar_target(
  name = hsmv_compiled_mo345_veg_basea,
  command = extract_monthly_modis_drought(geom_sf=hsmv_compiled_pt_data_clean,
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
  name= hsmv_compiled_mo345_veg_basea_prepped,
  command = prep_rs_modis_target(hsmv_compiled_mo345_veg_basea)
),
# tar_target(
#   name = hsmv_compiled_growing_season_mean_ndvi_z,
#   command = extract_ndvi_anomay(start_date, end_date, baseline,stat)
# should be sometheing inside chirps_spi/rolling_statistic
# ),
tar_target(name= hsmv_compiled_ndvi_growing_season21_z,
           command=extract_modis_ndvi_anomaly(
             geom_sf=hsmv_compiled_pt_data_clean,
             baseline_years = 2000:2021,
             date_range = c("2021-07-01", "2021-08-31"),
             range_label = "growing_season",scale= 250

           )),
tar_target(
  name = hsmv_compiled_landcover,
  command= extract_landcover_class(geom_sf = hsmv_compiled_pt_data_clean,landcover = list("esa","esri"))
),


tar_target(
  name = hsmv_compiled_rs_indicators_long,
  command= format_rs_indicators_long(country_code= "hsmv_compiled",
                                     # need to add one character col to not get the pivot_longer error
                                     # normall the `_pt_data_clean` files have country_code from the
                                     # start so it is not an issue, rather than re-running the whole
                                     # target i'll just add here as a shortcut.
                                     hsmv_compiled_pt_data_clean=hsmv_compiled_pt_data_clean |>
                                       mutate(country_code="hsmv_compiled",.after="new_uid"),

                                     hsmv_compiled_rainfall_intensity_prepped=hsmv_compiled_rainfall_intensity_prepped  |>
                                       mutate(country_code= "hsmv_compiled"),
                                     hsmv_compiled_mo345_veg_basea_prepped=hsmv_compiled_mo345_veg_basea_prepped |>
                                       mutate(country_code= "hsmv_compiled"),

                                     ## chirps
                                     hsmv_compiled_chirps_spi=hsmv_compiled_chirps_spi |>
                                       mutate(country_code= "hsmv_compiled") |>
                                       rename_with(.cols = starts_with("May"),
                                                   .fn = ~str_replace(.x,pattern =  "May",replacement = "May22")
                                       ),

                                     hsmv_compiled_chirps_spi_may21= hsmv_compiled_chirps_spi_may21 |>
                                       mutate(country_code= "hsmv_compiled") |>
                                       rename_with(.cols = starts_with("May"),
                                                   .fn = ~str_replace(.x,pattern =  "May",replacement = "May21")
                                       ),
                                     hsmv_compiled_chirps_spi_aug21= hsmv_compiled_chirps_spi_aug21 |>
                                       mutate(country_code= "hsmv_compiled") |>
                                       rename_with(.cols = starts_with("Aug"),
                                                   .fn = ~str_replace(.x,pattern =  "Aug",replacement = "Aug21")
                                       ),
                                     hsmv_compiled_chirps_spi_july22= hsmv_compiled_chirps_spi_july22 |>
                                       mutate(country_code= "hsmv_compiled") |>
                                       rename_with(.cols = starts_with("Jul"),
                                                   .fn = ~str_replace(.x,pattern =  "Jul",replacement = "Jul22")
                                       ),

                                     # hsmv_compiled_dist_to_coast,
                                     hsmv_compiled_landcover=hsmv_compiled_landcover |>
                                       mutate(country_code="hsmv_compiled"),
                                     hsmv_compiled_landforms_reclassified=hsmv_compiled_landforms_reclassified |>
                                       mutate(country_code= "hsmv_compiled"),
                                     hsmv_compiled_oxford_access=hsmv_compiled_oxford_access |>
                                       mutate(country_code="hsmv_compiled"),
                                     # hsmv_compiled_ndvi_growing_season_z=hsmv_compiled_ndvi_growing_season_z |>
                                     #   mutate(country_code="hsmv_compiled"),
                                     hsmv_compiled_npp=hsmv_compiled_npp |>
                                       mutate(country_code= "hsmv_compiled"),
                                     hsmv_compiled_air_quality=hsmv_compiled_air_quality |>
                                       mutate(country_code="hsmv_compiled")
                                     # hsmv_compiled_local_value_merged
  )
),
tar_target(
  name = hsmv_compiled_rs_indicators_wide,
  command= format_rs_indicators_wide(hsmv_compiled_rs_indicators_long)
)







)
