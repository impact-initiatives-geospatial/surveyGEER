

library(rgee)
library(sf)
ee_Initialize()


chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_link)

pt_mali <- data.frame(y= -5.292484,x= 14.289664) |>
  sf::st_as_sf(coords=c("y","x"), crs=4326)

pt_mali_buffered_5k <- pt_mali |>
  sf::st_transform(crs=32629) |>
  sf::st_buffer(dist = 5000) |>
  sf::st_transform(crs=4326)

rnd_pts <- sf::st_sample(x = pt_mali_buffered_5k,size = 5001)

rnd_pts_ee <- rgee::sf_as_ee(rnd_pts)


preicp_at_pts <-  rgee::ee_extract(x = chirps$first(),
                                  y = rnd_pts_ee,
                                  fun = ee$Reducer$median(),
                                  scale=5000,
                                  via = "drive"
                                  )
