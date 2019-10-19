
#' @title Get elevation data from a USGS web service
#' @description USGS APIs are used to determine the elevation associated with
#' the \code{longitude} and \code{latitude}.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param quiet Logical controlling the generate of progress messages.
#' @return Numeric elevation value.
#' @references \url{https://nationalmap.gov/epqs/}
#' @rdname getElevation_USGS
#' @export 
#' 
getElevation_USGS <- function(
  longitude = NULL,
  latitude = NULL,
  quiet = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "longitude and latitude must be single valuess"
    ))
  }
  
  # Be super careful
  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitude must be a valid value between -180 and 180")
  
  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitude must be a valid value between -180 and 180")
  
  # ----- Get USGS elevation data ----------------------------------------------
  
  # https://nationalmap.gov/epqs/pqs.php?x=-123.4&y=47.24&units=Meters&output=json
  
  # Create url
  url <- httr::parse_url("https://nationalmap.gov/epqs/pqs.php")
  
  url$query <- list(
    x= longitude,
    y = latitude,
    units = 'Meters',
    output = 'json'
  )
  
  # Get and parse the return
  r <- httr::GET(httr::build_url(url))
  if ( httr::http_error(r) ) {
    
    elevation <- as.numeric(NA)
    
    if ( !quiet ) {
      warning(sprintf(
        "USGS elevation service failed for URL %s", 
        httr::build_url(url)
      ))
    }
    
  } else {
    
    returnObj <- httr::content(r)
    eq <- returnObj$USGS_Elevation_Point_Query_Service$Elevation_Query
    
    if ( !is.null(eq) ) {
      
      # See https://nationalmap.gov/epqs/
      elevation <- ifelse(eq$Elevation < -999999, 0, eq$Elevation)
      
      # TODO:  If we were being careful we would check the returned x,y
      # TODO:  to see how much they differ from the requested lon,lat
      # TODO:  Initial tests show the results to be pretty good.
      
    } else {
      
      elevation <- as.numeric(NA)
      
      if ( !quiet ) {
        warning(sprintf(
          "USGS elevation service returned a NULL Elevation_Query object"
        ))
      }
      
    }
    
  }
  
  return(elevation)
  
}