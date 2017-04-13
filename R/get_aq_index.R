#' Retrieve air quality index for a given stations from GIOS API
#'
#' @param stationId id of a station
#'
#' @return data.frame air quality
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
