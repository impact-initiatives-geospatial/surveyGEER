#' zonal stats from point sample
#'
#' @param x image/imageCollection
#' @param aoi sf or FC object representing area for extraction
#' @param size \code{numeric} number of sample
#' @param stratified \code{logical} does aoi contain zones to stratify sample by
#'
#' @return data.frame containing extracted values at points ... we should then
#' aggregate these to stats for the user.
#' @export
#'

zonal_stats_from_pt_sample <-  function(x,aoi,size, stratified=F){
  if(!stratified){
    pt_sample<- sf::st_sample(x,size)
  }
  if(stratified){
    pt_sample <- "do someting"
  }
  ee_extract_tidy(x = x,
                  y = pt_sample,
                  stat = "median",
                  scale=scale,
  )
}
