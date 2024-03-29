% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_to.R
\name{date_to_range}
\alias{date_to_range}
\title{date_to_range}
\usage{
date_to_range(date, make_range = "center", by, unit = "day")
}
\arguments{
\item{date}{date (YMD)}

\item{make_range}{\code{character} forward, backward, or centered (default). Forward will produce a range where input date is the start date
input date + by units is the end_date. Backward will produce range where input date is end date and input date - by units is start_date.
Centered will put input date directly in the middle start_date= input_date - (by units)/2, end_date = input + (by units)/2}

\item{by}{\code{numeric} to center date by (i.e 4 signifies start date is 4 units before date input)}

\item{unit}{\code{numeric}  user can specify "month" or "day"}
}
\value{
vector of length 2 with first entry equal to start date and 2nd entry equal to end date
}
\description{
take user input and create a start and end date to define a range to filter - particularly for filtering `ee$ImageCollection`s
}
\examples{
\dontrun{
dat <- read_csv(here::here("assesssment_data.csv"))
date_range <- date_to_range(date = "2016-07-12",make_range = "center",by = 1,unit = "day")
}

}
