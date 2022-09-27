
#' combine_oxford_access_imgs
#'
#' @return multi-band image with
#' @export
#'
#' @examples

combine_oxford_access_imgs <- function(time_start="2022-01-01",return_tidyee=F){
  city_access <- ee$Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")
  healthcare_access <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")

  res <- city_access$
    select("accessibility")$
    rename("city_accessibility2015")$
    addBands(
      healthcare_access$select(list("accessibility","accessibility_walking_only"))$
        rename(list("healthcare_accessibility2019","healthcare_accessbility_walking_only2019"))
    )$
    set("system:time_start", ee$Date(time_start))
  if(return_tidyee){
    res <- as_tidyee(res)
  }
  return(res)
}


extract_oxford_access_indicators <- function(geom_sf,img_scale){
  img <- combine_oxford_access_imgs()
  geom_ee <-  rgee::sf_as_ee(geom_sf)
  fc_values <- img$sampleRegions(collection=geom_ee,scale= img_scale)
  if(nrow(geom_sf)>5000){
    df_values <-  rgee::ee_as_sf(fc_values,via="drive")
  }
  if(nrow(geom_sf)<=5000){
    df_values <-  rgee::ee_as_sf(fc_values)
  }
  return(df_values)

}
