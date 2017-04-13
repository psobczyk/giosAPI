#' Retrieve measurment data for a given sensor from GIOS API
#'
#' @param sensorId id of a sensor
#'
#' @return data.frame last 24h data
#'
#' @examples
#' \dontrun{
#' stations <- get_stations()
#' subset(stations, city.name == "Wrocław")
#' get_sensors(129)
#' get_measurment_data(744)
#' }
#'
#' @export
get_measurment_data <- function(sensorId){


  request = GET(url = paste("http://api.gios.gov.pl/pjp-api/rest//data/getData/", sensorId, sep=""))
  if(status_code(request) != 200){
    stop(paste("Problem z API:\n", http_status(request)$message))
  }

  measurement <- content(request)
  key <- measurement$key
  measurement <- lapply(measurement$values, function(x) {
    x$value <- ifelse(is.null(x$value), NA, x$value)
    x
  })
  measurement <- do.call(rbind.data.frame, measurement)
  measurement$key <- key

  return(measurement)
}
