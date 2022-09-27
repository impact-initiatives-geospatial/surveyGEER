get_modis_link<- function(fun){switch(fun,
                                      "terra"="MODIS/061/MOD13Q1",
                                      "aqua"="MODIS/061/MYD13Q1"
)}


#' Title
#'
#' @param x
#' @param mask
#'
#' @return
#' @export

cloud_scale_modis_ndvi <- function(x, mask){
  assertthat::assert_that(mask %in% c("cloud","cloud&quality","none"),
                          msg = "mask must be `none`, `cloud`,or `cloud&quality`")

  if(mask=="cloud"){
    modisIC <- cloud_mask_modis(x)
  }
  if(mask=="cloud&quality"){
    modisIC <- cloud_and_quality_mask_modis(x)
  }
  # rescale
  modis_ndvi <- modisIC$
    select("NDVI")$
    map(
      ee_utils_pyfunc(
        function(x){x$
            multiply(0.0001)$
            copyProperties(x,x$propertyNames())
        }
      )
    )

  return(modis_ndvi)

}
#' get_modis_drought_indicators
#'
#' @param baseline_years
#' @param yoi
#'
#' @return tidyee containing imageCollection with drought indicators
#' @export
#'
#' @examples
get_modis_drought_indicators <-  function(
    baseline_years =c(2000:2015),
    yoi=c(2021:2022),
    mask="none",
    satellite= "terra",
    TAC=F,
    temporal_interpolation= F
    ){
  satellite <- tolower(satellite)
  assertthat::assert_that(satellite %in% c("terra","aqua"))
  assertthat::assert_that(mask %in% c("cloud","cloud&quality","none"),
                            msg = "mask must be `none`, `cloud`,or `cloud&quality`")

  modis_link <- get_modis_link(satellite)

  modisIC <- ee$ImageCollection(modis_link)
  modis_ndvi <- cloud_scale_modis_ndvi(x = modisIC,mask=mask)

  modis_ndvi_tidy <- as_tidyee(modis_ndvi)

  monthly_baseline <- modis_ndvi_tidy |>
    filter(year %in% baseline_years) |>
    group_by(month) |>
    summarise(stat=list("mean","sd","median","min","max"))

  ndvi_recent_monthly <- modis_ndvi_tidy |>
    filter(year %in% yoi) |>
    group_by(year,month) |>
    summarise(
      stat="mean"
    )
  ndvi_recent_renamed <- ndvi_recent_monthly |>
    select(NDVI="NDVI_mean")

  if(isTRUE(TAC)){
    gap_filling_sat <- c("terra","aqua") |> setdiff(satellite)
    gap_modis_link <-  get_modis_link(gap_filling_sat)
    gap_modisIC <- ee$ImageCollection(gap_modis_link)
    gap_modis_ndvi <- cloud_scale_modis_ndvi(x = gap_modisIC,mask=mask)
    gap_modis_ndvi_tidy <- as_tidyee(gap_modis_ndvi)

    #' so far a deliberate decision to not gap fill historical baseline.
    #' This is due to the fact that by composting over x years we are already gap filling.
    #' An improvement could potentially be to apply a conditional gap filling based on
    #' % masked pixels in each pixel-stack. If crosses the threshold could gap fill.

    # gap_monthly_baseline <- gap_modis_ndvi_tidy |>
    #   filter(year %in% baseline_years) |>
    #   group_by(month) |>
    #   summarise(stat=list("mean","sd","median","min","max"))
    gap_ndvi_recent_monthly <- gap_modis_ndvi_tidy |>
      filter(year %in% yoi) |>
      group_by(year,month) |>
      summarise(
        stat="mean"
      ) |>
      select(gap_NDVI= "NDVI_mean")

    # create a unique, but common property to join the two image collection bands
    gap_ndvi_recent_monthly$ee_ob <- gap_ndvi_recent_monthly$ee_ob$map(
      function(img){
        img$set("year_month",ee$Date(img$get("system:time_start"))$format("YYYY_MM"))
      }
    )
    ndvi_recent_renamed$ee_ob <- ndvi_recent_renamed$ee_ob$map(
      function(img){
        img$set("year_month",ee$Date(img$get("system:time_start"))$format("YYYY_MM"))
      }
    )


    terra_aqua_recent <- ndvi_recent_renamed |>
      inner_join(gap_ndvi_recent_monthly, by="year_month")

    # replace masked pixels of NDVI band with gap_NDVI values (if not masked)
    TAC_filled<- terra_aqua_recent$ee_ob$map(
      function(img){
        img$select("NDVI")$unmask(img$select("gap_NDVI"))
      }
    )
    if(temporal_interpolation){
      TAC_filled_interpolated <- interpolate_ic(TAC_filled,time_window = 40)$select("NDVI")
      ndvi_recent_renamed <- as_tidyee(TAC_filled_interpolated)
    }
    if(!temporal_interpolation){
      ndvi_recent_renamed <- as_tidyee(TAC_filled)
    }

  }
  if(temporal_interpolation & !TAC){
    ndvi_recent_renamed <- interpolate_ic(ndvi_recent_renamed$ee_ob,
                                          time_window = 40)$select("NDVI") |>
      as_tidyee()
  }

  ndvi_recent_and_baseline<- inner_join(x = ndvi_recent_renamed,
                                        y = monthly_baseline,
                                        by = "month")


  ndvi_recent_baseline_imageCol <- ndvi_recent_and_baseline |>
    as_ee()

  ndvi_zscore<- ndvi_recent_baseline_imageCol$map(
    function(img){
      zscore<- img$expression(
        # Q1: was using `NDVI-NDVI_mean`, but then we had large outliers.
        # plotting NDVI vs resulting NDVI_Z showed that there was a strong linear
        # relationship (as expected) however... very high Z-score outliers-- suggesting that
        # the historical mean was being warped by clouds -- trying with median now to see if that
        # reduces the cloudy outlier values in the historical mean

        # A1: trying with `NDVI_median` makes little difference - same outliers appear... but why
        # perhaps something weird in the Colombia data since it is mostly urban... Might
        # make sense to try in different contexct to see if we get same results before going
        # down a rabbit hole
        "float((NDVI-NDVI_median)/(NDVI_sd))",
        # "float((NDVI-NDVI_mean)/(NDVI_sd))",
        opt_map= list(NDVI= img$select("NDVI"),
                      NDVI_median= img$select("NDVI_median"),
                      # NDVI_mean= img$select("NDVI_mean"),
                      NDVI_sd= img$select("NDVI_sd")
        )
      )$rename("NDVI_z_score")
      img$select("NDVI","NDVI_mean","NDVI_median","NDVI_min","NDVI_max")$addBands(zscore)
    }

  )
  ndvi_pct_median<- ndvi_zscore$map(
    function(img){
      NDVI_pct_median<- img$expression(
        "float((NDVI)/(NDVI_median))",
        opt_map= list(NDVI= img$select("NDVI"),
                      NDVI_median= img$select("NDVI_median")

        )
      )$rename("NDVI_pct_median")
      img$select("NDVI","NDVI_min","NDVI_max","NDVI_z_score")$addBands(NDVI_pct_median)
    }

  )

  vci<- ndvi_pct_median$map(
    function(img){
      vci<- img$expression(
        "float((NDVI-NDVI_min)/(NDVI_max-NDVI_min))",
        opt_map= list(NDVI= img$select("NDVI"),
                      NDVI_min= img$select("NDVI_min"),
                      NDVI_max= img$select("NDVI_max")
        )
      )$rename("VCI")
      img$select("NDVI","NDVI_pct_median","NDVI_z_score")$addBands(vci)
    }

  )


  ndvi_indicators <- as_tidyee(vci)

  ndvi_indicators_pre_processed <- ndvi_indicators |>
    # filter(year>=2021) |>
    select(
      "NDVI_z_score",
      "NDVI_pct_median",
      "VCI","NDVI"
    )

}

extract_monthly_modis_drought <- function(geom_sf,
                                          baseline_years = c(2000:2015),
                                          moi = c(3, 4, 5),
                                          yoi = c(2022),
                                          scale = 250,
                                          mask = "cloud&quality",
                                          satellite = "terra",
                                          TAC = T,
                                          temporal_interpolation = T) {

  indicators <- get_modis_drought_indicators(
    baseline_years = baseline_years,
    yoi = yoi,
    mask = mask,
    satellite = satellite,
    TAC = TAC,
    temporal_interpolation = temporal_interpolation
  )
  # indicators <- indicators_ic
  indicators |>
    filter(month %in% moi) |>
    ee_extract_tidy(y= geom_sf, scale=scale, via="drive")

}


# get_modis_drought_indicators_gap_filled(TAC=T,
#                                         reference_satellite="terra",
#                                         temporal_interpolation=T,
#                                         mask
#                                         ){
#   get_gap_filling_sat<- function(fun){switch(fun,
#                        "terra"="aqua"
#                        )}
#   gap_filler <- get_gap_filling_sat("terra")
#
#
#   reference_ic<- get_modis_drought_indicators(
#     satellite = reference_satellite,
#     mask=mask
#     )
#
#   gap_filling_ic<- get_modis_drought_indicators(satellite = gap_filler,
#                                                     mask=mask)
#
#   terra_aqua_ndvi_joined <- terra_drought_cloud |>
#     select(terra_ndvi="NDVI") |>
#     inner_join(
#       aqua_drought_cloud |>
#         select(aqua_ndvi="NDVI")  , by ="month"
#     )
#
#
# }
