% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_metadata_palette_viz.R
\name{get_metadata_palette_viz}
\alias{get_metadata_palette_viz}
\title{helper to extract metadata for default viz (only for palette, not gamma)}
\usage{
get_metadata_palette_viz(img, bands)
}
\arguments{
\item{img}{image or image collection}

\item{bands}{bands to visualize}
}
\value{
visParams object ready to be added to `Map$addLayer()`
}
\description{
helper to extract palette and min/max values when layer comes with default values. I am sure there is a way to do this GEE, but I could not find it so I made my own. Pretty sure this is also available in geemap
}
