% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_aoi_grid.R
\name{load_aoi_grid}
\alias{load_aoi_grid}
\title{load_aoi_grid}
\usage{
load_aoi_grid(
  aoi = NULL,
  grid = NULL,
  hex_diameter = 15000,
  country_code = NULL
)
}
\arguments{
\item{aoi}{layer name of AOI}

\item{grid}{layer name of grid}

\item{hex_diameter}{\code{numeric} desired diameter of hexagon grid (in meter)}

\item{country_code}{\code{character} three letter country code (lower case)}
}
\value{
sf class grid
}
\description{
load_aoi_grid
}
\examples{
\dontrun{
library(surveyGEER)
grid <- load_aoi_grid()
}
}
