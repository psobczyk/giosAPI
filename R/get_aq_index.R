#' Retrieve air quality index for a given stations from GIOS API
#'
#' @param stationId id of a station
#'
#' @return data.frame air quality
#'
#' @examples
#' \dontrun{
#' stations <- get_stations()
#' subset(stations, city.name == "Wrocław")
#' get_aq_index(129)
#' }
#'
#' @export
get_aq_index <- function(stationId){

  request = GET(url = paste("http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/", stationId, sep=""))
  if(status_code(request) != 200){
    stop(paste("Problem z API:\n", http_status(request)$message))
  }

  air_quality <- content(request)

  return(data.frame(t(unlist(air_quality))))
}
