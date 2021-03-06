% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_aq_index.R
\name{get_aq_index}
\alias{get_aq_index}
\title{Retrieve air quality index for a given stations from GIOS API}
\usage{
get_aq_index(stationId)
}
\arguments{
\item{stationId}{id of a station}
}
\value{
data.frame air quality
}
\description{
Retrieve air quality index for a given stations from GIOS API
}
\examples{
\dontrun{
stations <- get_stations()
subset(stations, city.name == "Wrocław")
get_aq_index(129)
}

}
