#' Retrieve list of available sensors for given station from GIOS API
#'
#' @param stationId id of a station
#'
#' @return data.frame all sensors available for a given station
#'
#' @export
get_sensors <- function(stationId){

  request = GET(url = paste("http://api.gios.gov.pl/pjp-api/rest/station/sensors/", stationId, sep=""))

  if(status_code(request) != 200){
    stop(paste("Problem z API:\n", http_status(request)$message))
  }

  sensors <- content(request)
  sensors <- lapply(sensors, function(x) t(data.frame(unlist(x))))
  sensors <- do.call(rbind.data.frame, sensors)
  rownames(sensors) <- NULL

  return(sensors)
}
