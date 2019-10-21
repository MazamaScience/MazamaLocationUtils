#' @title Validate proper setup of MazamaSpatialUtils
#' @description The \pkg{MazamaSpatialUtils} package mus be properly installed
#' and initialized before using functions from the \pkg{MazamaLocationUtils} 
#' package. Functions can test for this 
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateMazamaSpatialUtils
#' @export 
#' 
validateMazamaSpatialUtils <- function() {
  
  if ( !exists("EEZCountries") ||
       !exists("OSMTimezones") ||
       !exists("NaturalEarthAdm1") ||
       !exists("USCensusCounties") ) {
    
    stop(paste0(
      "You must initialize MazamaSpatialUtils with:\n\n",
      "  library(MazamaSpatialUtils)\n",
      "  setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  loadSpatialData(\"EEZCountries\")\n",
      "  loadSpatialData(\"OSMTimezones\")\n",
      "  loadSpatialData(\"NaturalEarthAdm1\")\n",
      "  loadSpatialData(\"USCensusCounties\")\n"
    ))
    
  }
  
  return(invisible(TRUE))
  
}


#' @title Validate longitude and latitude vectors
#' @description Longitude and latitude vectors validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonsLats
#' @export 
#' 
validateLonsLats <- function(
  longitude = NULL,
  latitude = NULL
) {
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) != length(latitude) ) {
    stop(paste0(
      "longitude and latitude must have the same length"
    ))
  }
  
  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitudes must be valid values between -180 and 180")
  
  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitudes must be a valid values between -180 and 180")
  
  return(invisible(TRUE))
  
}


#' @title Validate longitude and latitude values
#' @description Longitude and latitude are validated to be parseable as numeric
#' and within the bounds -180:180 and -90:90. If validation fails, an error is
#' generated.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @return Invisibly returns \code{TRUE} if no error message has been generated.
#' @rdname validateLonLat
#' @export 
#' 
validateLonLat <- function(
  longitude = NULL,
  latitude = NULL
) {
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "longitude and latitude must be single values"
    ))
  }
  
  longitude <- as.numeric(longitude)
  if ( is.na(longitude) || longitude < -180 || longitude > 180 )
    stop("longitude must be a valid value between -180 and 180")
  
  latitude <- as.numeric(latitude)
  if ( is.na(latitude) || latitude < -180 || latitude > 180 )
    stop("latitude must be a valid value between -180 and 180")
  
  return(invisible(TRUE))
  
}