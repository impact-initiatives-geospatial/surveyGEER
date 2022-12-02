
extract_npp_indicators <-  function(geom_sf, img_scale= 500){
  cat("anomaly scanning")
  npp_indicators_img <-get_npp_indicators()
  geom_ee <-  rgee::sf_as_ee(geom_sf)
  cat("extracting indicators\n")
  fc_values <- npp_indicators_img$
    sampleRegions(collection= geom_ee,
                  scale= img_scale)
  if(nrow(geom_sf)<5000){
    # maxFeatures = nrow(geom_sf)*2 # some odd code in `rgee:::ee_fc_to_sf_getInfo_batch` makes it better to comment this out?
    res <- rgee::ee_as_sf(fc_values)
  }
  if(nrow(geom_sf)>5000){
    res <- rgee::ee_as_sf(fc_values,maxFeatures = nrow(geom_sf))
  }
  return(res)
}





get_npp_indicators <-  function(){
  modis_terra <- ee$ImageCollection("MODIS/006/MOD17A3HGF")$select("Npp")
  modis_terra <-  as_tidyee(modis_terra)

  # modis_terra$vrt$time_start
  historical <- modis_terra |>
    summarise(
      stat=list("mean","median","sd")
    )
  # historical$ee_ob$first()$bandNames()$getInfo()

  recent <- modis_terra |>
    filter(year %in% c(2020,2021)) |>
    as_ee()
  # recent$first()$bandNames()$getInfo()

  recent <- recent$map(function(img) img$set("month",1)) |>
    as_tidyee()
  # recent_ic$aggregate_array("month")$getInfo()
  # historical$ee_ob$aggregate_array("month")$getInfo()

  recent_historical<- recent |>
    inner_join(historical, by = "month")

  recent_historical_ic <- recent_historical|>
    as_ee()

  # recent_historical_ic$first()$bandNames()$getInfo()

  npp_z<- recent_historical_ic$map(
    function(img){
      zscore<- img$expression(
        "float((Npp-Npp_mean)/(Npp_sd))",
        opt_map= list(Npp= img$select("Npp"),
                      Npp_mean= img$select("Npp_mean"),
                      Npp_sd= img$select("Npp_sd")
        )
      )$rename("Npp_Z")
      img$select("Npp","Npp_mean","Npp_median")$addBands(zscore)
    }

  )

  npp_pct_median<- npp_z$map(
    function(img){
      Npp_pct_median<- img$expression(
        "float((Npp)/(Npp_median))",
        opt_map= list(Npp= img$select("Npp"),
                      Npp_median= img$select("Npp_median")

        )
      )$rename("Npp_pct_median")
      img$select("Npp","Npp_mean","Npp_median","Npp_Z")$addBands(Npp_pct_median)
    }

  )
  # npp_pct_median$toBands()$bandNames()$getInfo()
  bands <- c("Npp","Npp_mean","Npp_median","Npp_Z","Npp_pct_median")
  new_bandnames <- c(2020,2021) |>
    purrr::map(~glue::glue("{bands}_{.x}") |> as.character()) |>
    unlist()
  npp_indicators_img <- npp_pct_median$toBands()$rename(new_bandnames)
  return(npp_indicators_img)
}
