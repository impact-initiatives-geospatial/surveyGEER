% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_msna.R
\name{fetch_msna}
\alias{fetch_msna}
\title{fetch_msna - helper function to quickly pull msna coordinate files}
\usage{
fetch_msna(country_code = "col")
}
\arguments{
\item{country_code}{\code{character} three letter country code (lower-case)}
}
\value{
sf data.frame containing msna records
}
\description{
return msna data based on suplied country code
}
\examples{
\dontrun{
library(surveyGEER)
df <-  fetch_msna("com")
}
}
