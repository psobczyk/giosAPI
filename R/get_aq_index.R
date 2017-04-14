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
  types <- gsub("(.*)CalcDate", "\\1", names(air_quality)[grepl(pattern = "CalcDate", names(air_quality))])

  aq <- lapply(types, function(type){
    temp <- air_quality[grepl(pattern = type, names(air_quality))]
    names(temp) <- gsub(type, "", names(temp))
    temp$IndexLevel.id <- ifelse(is.null(temp$IndexLevel$id), NA,  temp$IndexLevel$id)
    temp$IndexLevel.indexLevelName <-ifelse(is.null(temp$IndexLevel$indexLevelName), NA, temp$IndexLevel$indexLevelName)
    temp <- temp[-2]
    temp[sapply(temp, is.null)] <- NA
    data.frame(type = type, temp)
  })

  return(cbind(stationId = air_quality$id, do.call(rbind, aq)))
}
