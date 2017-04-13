#' Retrieve list of available stations from GIOS API
#'
#' @export
get_stations <- function(){
  request <- GET(url = "http://api.gios.gov.pl/pjp-api/rest/station/findAll")
  if(status_code(request) != 200){
    stop(paste("Problem z API:\n", http_status(request)$message))
  }

  stations <- content(request)
  stations <- lapply(stations, unlist)
  col_names <- names(stations[[1]])
  stations <- do.call(rbind.data.frame, stations)
  colnames(stations) <- col_names

  return(stations)
}
