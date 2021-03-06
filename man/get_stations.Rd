% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stations.R
\name{get_stations}
\alias{get_stations}
\title{Retrieve list of available stations from GIOS API}
\usage{
get_stations()
}
\description{
Retrieve list of available stations from GIOS API
}
\examples{
\dontrun{
stations <- get_stations()
subset(stations, city.name == "Wrocław")
}

}
