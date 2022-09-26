
#' ee_chirps_rain_intensity wrapper for processes to calculate rainfall intensity parameters
#'
#' @param convert_tidyee temporary parameter for testing
#'
#' @return ee$ImageCollection
#' @export
#'
ee_chirps_rain_intensity <-  function(convert_tidyee=F,from_when = "2022-05-31"){
  chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
  chirpsIC <- ee$ImageCollection(chirps_link)
  chirps_tidy <- as_tidyee(chirpsIC)
  chirps_time_filtered <- chirps_tidy |>
    filter(year==2022)

  # by defining the windows to calculate rolling sum as list I can iterate through each one
  roll_windows <- list(d30max=30,d60max=60,d90max=90)
  cat(crayon::green("calculating rolling sums for over 30, 60 , 90 days and 3,5,and 10 day intervals\n"))
  max_precip_3days <- purrr:::map(.x=roll_windows,
                                  .f=function(x){
                                    ee_max_rolling_sum(
                                      x = chirps_tidy,
                                      roll_window = 3,
                                      roll_time_unit = "day",
                                      from_when = from_when,
                                      over_time = x,
                                      return_tidyee = F
                                    )
                                  }
  )

  max_precip_5days <- purrr:::map(.x=roll_windows,
                                  .f=function(x){
                                    ee_max_rolling_sum(
                                      x = chirps_tidy,
                                      roll_window = 5,
                                      roll_time_unit = "day",
                                      from_when = from_when,
                                      over_time = x,
                                      return_tidyee = F
                                    )
                                  }
  )

  max_precip_10days <- purrr:::map(.x=roll_windows,
                                   .f=function(x){
                                     ee_max_rolling_sum(
                                       x = chirps_tidy,
                                       roll_window = 10,
                                       roll_time_unit = "day",
                                       from_when =from_when,
                                       over_time = x,
                                       return_tidyee = F
                                     )
                                   }
  )

  # this is a faster way to merge ics in this instance
  # however, we want them as tidyee classes anyways for the `inner_join`, so might as
  # take the time here
  ####################################################
  if(!convert_tidyee){
    cat("binding images to image collection and joining bands")
    max_precip_3days_ic <- ee$ImageCollection$fromImages(ee$List(max_precip_3days |> unname()))
    max_precip_5days_ic <- ee$ImageCollection$fromImages(ee$List(max_precip_5days |> unname()))
    max_precip_10days_ic <- ee$ImageCollection$fromImages(ee$List(max_precip_10days |> unname()))

    # lets see if the

    max_precip_bands_joined <- max_precip_3days_ic |>
      inner_join(max_precip_5days_ic, by = "month") |>
      inner_join(max_precip_10days_ic, by = "month")


  }
  if(convert_tidyee){
    cat(crayon::green("converting to tidyee & binding"))
    max_precip_3days_tidyee <- max_precip_3days|>
      purrr::map(~as_tidyee(.x)) |>
      bind_ics()

    max_precip_5days_tidyee <- max_precip_5days|>
      purrr::map(~as_tidyee(.x)) |>
      bind_ics()
    max_precip_10days_tidyee <- max_precip_10days |>
      purrr::map(~as_tidyee(.x)) |>
      bind_ics()

    cat(crayon::green("converting to tidyee & binding"))

    max_precip_bands_joined <- max_precip_3days_tidyee |>
      inner_join(max_precip_5days_tidyee, by = "month") |>
      inner_join(max_precip_10days_tidyee, by = "month")

  }
  return(max_precip_bands_joined)





}




#' extract_chirps_rain_intensity
#' @description wrapper for ee_chirps_rain_intensity for purpose of creating target
#' @param geom_sf geometric unit to extract to
#' @return data.frame with rainfall intensity for each unit of geom_sf
#' @export
#'
#' @examples
extract_chirps_rain_intensity <- function(geom_sf,from_when){
  cat("calculating rainfall intensity\n")
  chirps_rainfall_intensity<-ee_chirps_rain_intensity(convert_tidyee = T,from_when=from_when)
  cat("extracting rainfall intesnity\n")

  res <- chirps_rainfall_intensity |>
  ee_extract_tidy(y = geom_sf,stat = "median",scale = 5500,via = "drive")
  return(res)
}
