
#' Title
#'
#' @param boolean
#' @param val
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' ee_bool<- switch_boolean(boolean = ">",val = 25)
#' }
switch_boolean <-  function(boolean,val){switch(boolean,
                                                ">" = function(x)x$gt(val),
                                                ">=" = function(x)x$gte(val),
                                                "<" = function(x)x$lt(val),
                                                "<=" = function(x)x$lte(val),
                                                "=" = function(x)x$eq(val),
                                                NULL

)
}


#' ee_closest_distance_to_val
#'
#' @param x
#' @param y
#' @param boolean
#' @param val
#' @param scale
#' @param tidy_extract
#' @param via
#'
#' @return data.frame with closest distance value for each input feature
#' @export
#'
#' @examples
ee_closest_distance_to_val <-  function(x,
                                        y,
                                        boolean="=",
                                        val=2,
                                        scale,
                                        tidy_extract=T, via){
  UseMethod('ee_closest_distance_to_val')


}


#' @name ee_closest_distance_to_val
#' @export
ee_closest_distance_to_val.ee.imagecollection.ImageCollection <-  function(x,
                                                                           y,
                                                                           boolean="=",
                                                                           val=2,
                                                                           scale,
                                                                           tidy_extract=T, via
){


  assertthat:::assert_that(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  boolean_mask_cond<-switch_boolean(boolean=boolean,val=val)

    x_masked <- x$map(
    function(image){
      image_masked = boolean_mask_cond(image)$
        selfMask()
      return(ee$Image(image_masked$copyProperties(image,image$propertyNames())))

    }
  )

  cat(crayon::green("Generating distance raster(s)\n"))
  euclidean_distance_to_x <-  x_masked$
    map(
      function(image){
        distances = image$mask()$
          fastDistanceTransform({
            neighborhood=1024*3
          })$multiply(ee$Image$pixelArea())$sqrt()$rename("distance_to")$
          reproject(crs="EPSG:4326",scale=scale)
        return(ee$Image(distances$copyProperties(image,image$propertyNames())))

      }
    )
  cat(crayon::green("Extracting distance raster values to y\n"))

  if(tidy_extract){
  res <- ee_extract_tidy(x = euclidean_distance_to_x$select("distance_to"),
                         y = y,
                         scale = scale, via=via)
  }

  return(res)
}

#' @name ee_closest_distance_to_val
#' @export
ee_closest_distance_to_val.ee.image.Image<-  function(x,
                                                      y,
                                                      boolean="=",
                                                      val=2,
                                                      scale,
                                                      tidy_extract=T, via=via
){

  # stopifnot(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.image.Image"))
  boolean_mask_cond<-switch_boolean(boolean=boolean,val=val)

  cat(crayon::green("masking x image/imageCollection\n"))
  x_masked <-ee$Image(
    boolean_mask_cond(x)$
      selfMask()$
      copyProperties(x,x$propertyNames())
  )
  cat(crayon::green("Generating distance raster(s)\n"))

  FDT = x_masked$mask()$
    fastDistanceTransform({
      neighborhood=1024*3
    })

  distances= FDT$
    multiply(ee$Image$pixelArea())$
    sqrt()$
    rename("distance_to")$
    reproject(crs="EPSG:4326",scale=scale)

  euclidean_distance_to_x <-ee$Image(distances$copyProperties(x_masked,x_masked$propertyNames()))

  cat(crayon::green("Extracting distance raster values to y\n"))

  if(tidy_extract){
    res <- ee_extract_tidy(x = euclidean_distance_to_x$select("distance_to"),y=y,scale=30,via=via)
  }
  if(!tidy_extract){
    geom_ee<- rgee::sf_as_ee(sf)
    res_fc <- euclidean_distance_to_x$
      select("distance_to")$
      sampleRegions({
        list(
          collection= geom_ee,
          scale=scale
          )
    })
    res <- rgee::ee_as_sf(res_fc)
  }

  return(res)
}




