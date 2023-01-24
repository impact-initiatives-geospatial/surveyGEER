library(rgee)
library(tidyrgee)
ee_Initialize()



## 1 reprex posted
# works fine with rgee
gldas <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")
sm <- gldas$select("SoilMoi0_10cm_inst")
sm_filt <- sm$filterDate("2004-01-01","2009-12-31")
sm_2004_2009_mean <- sm$filterDate("2004-01-01","2009-12-31")$mean()
sm_2004_2009_mean$bandNames()$getInfo()

# tidyversion throws error
sm_tidy <-  as_tidyee(sm)

sm_2004_2009_mean_tidy<-  sm_tidy|>
  filter(year %in% c(2004:2009)) |>
  summarise(stat="mean")
sm_2004_2009_mean_tidy$ee_ob$bandNames()$getInfo()


# 2 is it reducer? - no this works
gldas <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")
sm <- gldas$select("SoilMoi0_10cm_inst")
sm_2004_2009_filtered <- sm$filterDate("2004-01-01","2009-12-31")
ee_reducer <- tidyrgee:::stat_to_reducer_full("mean")
sm_2004_2009_reduced <- ee_reducer(sm_2004_2009_filtered)
sm_2004_2009_reduced$bandNames()$getInfo()

# 3 reprex attempt -1b - this works so not the problem
gldas <- ee$ImageCollection("NASA/GLDAS/V021/NOAH/G025/T3H")
sm <- gldas$select("SoilMoi0_10cm_inst")
sm_tidy <- as_tidyee(sm)
sm_tidy$ee_ob$first()$bandNames()$getInfo()

# 4
#debugonce(tidyrgee:::summarise_pixels)


# 5 - try another IC -- chirps
chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")
chirps <- chirps$select("precipitation")
chirps_filt <- chirps$filterDate("2004-01-01","2009-12-31")
chirps_2004_2009_mean <- chirps$filterDate("2004-01-01","2009-12-31")$mean()
chirps_2004_2009_mean$bandNames()$getInfo()

# Here it works with CHIRPS, but it does seem to
# have a considerable lag compared to above
chirps_tidy <-  as_tidyee(chirps)

chirps_2004_2009_mean_tidy<-  chirps_tidy|>
  filter(year %in% c(2004:2009)) |>
  summarise(stat="mean")

chirps_2004_2009_mean_tidy$ee_ob$bandNames()$getInfo()
