#' spi_bar_plot_faceted
#'
#' @param df data.frame
#' @param zone zone column to facet by
#'
#' @return typical SPI barplot faceted by zone of choice
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' library(rgee)
#' library(tidyrgee)
#' library(sf)
#' ee_Initialize() # initialize gee
#'
#' #calculate spi
#' spi_3<- ee_chirps_spi(window = 3,window_unit = "month",moi = 4)
#' run zonal stat
#' spi_3_adm1 <- spi_3 |>
#'  ee_extract_tidy(y= som_adm1,
#'                  stat = "median",
#'                  scale=5500,
#'                  via="drive")
#'  # create bar plot
#' spi_bar_plot_faceted(spi_3_adm1,zone ="admin1name")
#' }
#'

spi_bar_plot_faceted <-  function(df, zone){
  assertthat::assert_that(all(c("date","value",zone) %in% names(df)))
  plt<- spi_bar_plot(df)+
    facet_wrap(as.formula(paste("~", zone)),scales = "free_y")
  return(plt)

}



#' spi_bar_plot
#'
#' @param df
#'
#' @return typical SPI barplo
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' library(rgee)
#' library(tidyrgee)
#' library(sf)
#' ee_Initialize() # initialize gee
#'
#' #calculate spi
#' spi_3<- ee_chirps_spi(window = 3,window_unit = "month",moi = 4)
#' run zonal stat
#' spi_3_adm1 <- spi_3 |>
#'  ee_extract_tidy(y= som_adm1,
#'                  stat = "median",
#'                  scale=5500,
#'                  via="drive")
#'  # create bar plot
#' spi_bar_plot_faceted(
#'                      spi_3_adm1|>
#'                         filter(admin1name=="Awdal"), # filter to just 1 zone/admin boundary
#'                         zone ="admin1name"
#'                      )
#' }
#' }
spi_bar_plot <-  function(df=spi_3_adm1){
  assertthat::assert_that(all(c("date","value") %in% names(df)))

  df_classified <- df |>
    mutate(
      binary_class =if_else(value<=0,"Negative","Positive")
    )

  plt <- df_classified |>
    ggplot(aes(x=date, y= value, fill=binary_class)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("#EE5859","#0067A9"))+
    theme_bw()+
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle=90)
    )
  return(plt)

}
