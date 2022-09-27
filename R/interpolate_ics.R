

#' interpolate_ic
#'
#' @param x ee$ImageCollection
#' @param time_window time range to look forward and backward for unmasked pixels
#'
#' @return ee$ImageCollection with bands interpolated
#' @details code from Ujaval Gandhi,2021
#' @export
#'
#' @examples
interpolate_ic <-  function(x, time_window){
  # add time band  masked in same way as other bands
  img_w_time_band_masked <- x$map(
    function(img){
      time_img = img$metadata("system:time_start")$rename("timestamp")
      time_img_masked = time_img$updateMask(img$mask()$select(0))
      return(img$addBands(time_img_masked))
    }
  )
  time_window_millis <-  ee$Number(time_window)$multiply(1000*60*60*24)

  # for every image we need to find the before & after image
  # use 2 joins + 3 fileters?
  max_diff_filter <-  ee$Filter$maxDifference(
    difference= time_window_millis,
    leftField= 'system:time_start',
    rightField= 'system:time_start'
  )
  lte_filter <-  ee$Filter$lessThanOrEquals(
    leftField = "system:time_start",
    rightField = "system:time_start"
  )
  gte_filter = ee$Filter$greaterThanOrEquals(
    leftField = "system:time_start",
    rightField = "system:time_start"
  )

  filter1 = ee$Filter$And(max_diff_filter,lte_filter)

  join1 = ee$Join$saveAll(
    matchesKey= "after",
    ordering = "system:time_start",
    ascending= FALSE
  )
  join1Result <-  join1$apply(
    primary= img_w_time_band_masked,
    secondary = img_w_time_band_masked,
    condition = filter1
  )
  # join1Result_ic <- ee$ImageCollection(join1Result)
  filter2 = ee$Filter$And(max_diff_filter, gte_filter)
  join2 = ee$Join$saveAll(
    matchesKey = "before",
    ordering = "system:time_start",
    ascending = TRUE
  )
  join2Result <-  join2$apply(
    primary = join1Result,
    secondary= join1Result,
    condition = filter2
  )


  interpolatedCol = ee$ImageCollection(
    join2Result$map(
      interpolateImages)
  )
  return(interpolatedCol)
}


interpolateImages <-  function(image) {
  image <- ee$Image(image)
  #We get the list of before and after images from the image property
  # Mosaic the images so we a before and after image with the closest unmasked pixel
  beforeImages <- ee$List(image$get('before'))
  beforeMosaic <- ee$ImageCollection$fromImages(beforeImages)$mosaic()
  afterImages <- ee$List(image$get('after'))
  afterMosaic <- ee$ImageCollection$fromImages(afterImages)$mosaic()
  #Get image with before and after times
  t1 <- beforeMosaic$select('timestamp')$rename('t1')
  t2 <- afterMosaic$select('timestamp')$rename('t2')
  t <- image$metadata('system:time_start')$rename('t')
  timeImage <- ee$Image$cat(list(t1, t2, t))

  timeRatio <- timeImage$expression(
    "float((t - t1) / (t2 - t1))",
    opt_map = list(
      t= timeImage$select("t"),
      t1= timeImage$select("t1"),
      t2= timeImage$select("t2")
    )
  )
  # Compute an image with the interpolated image y
  interpolated <- beforeMosaic$
    add((afterMosaic$subtract(beforeMosaic)$multiply(timeRatio)))
  # Replace the masked pixels in the current image with the average value
  result <- image$unmask(interpolated)
  return (result$copyProperties(image, list('system:time_start')))
}

