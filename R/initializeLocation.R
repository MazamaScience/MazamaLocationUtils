
#' @title  Create "known location" record with core metadata
#' @description Creates a "known location" record with the following columns
#' of core metadata:
#' \itemize{
#' \item{locationID}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{timezone}
#' }
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param stateDataset Name of spatial dataset to use for determining state
#' @param quiet Logical controlling the generate of progress messages.
#' @return Tibble with a single new "known location".
#' @rdname initializeLocation
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom MazamaSpatialUtils getCountryCode getStateCode getTimezone
#' @importFrom dplyr tibble
initializeLocation <- function(
  longitude = NULL,
  latitude = NULL,
  stateDataset = "NaturalEarthAdm1",
  quiet = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(stateDataset)
  
  # ----- Initialize MazamaSpatialUtils ----------------------------------------
  
  if ( !exists("EEZCountries") ||
       !exists("OSMTimezones") ||
       !exists("NaturalEarthAdm1") ) {
    
    stop(paste0(
      "You must initialize MazamaSpatialUtils with: \n",
      "  library(MazamaSpatialUtils)\n",
      "  setSpatialDataDir(\"YOUR_DATA_DIR\")\n",
      "  loadSpatialData(\"EEZCountries\")\n",
      "  loadSpatialData(\"OSMTimezones\")\n",
      "  loadSpatialData(\"NaturalEarthAdm1\")\n"
    ))
    
  } else if ( !exists(stateDataset) ) {
    
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
    
  }
  
  # ----- Elevation ------------------------------------------------------------

  elevation <- getElevation_USGS(
    longitude = longitude,
    latitude = latitude,
    quiet = quiet
  )  

  # ----- Country, State, Timezone, locationID ---------------------------------
  
  countryCode <- MazamaSpatialUtils::getCountryCode(
    lon = longitude,
    lat = latitude,
    dataset = "EEZCountries",
    useBuffering = FALSE
  )
  
  stateCode <- MazamaSpatialUtils::getStateCode(
    lon = longitude,
    lat = latitude,
    dataset = stateDataset,
    useBuffering = TRUE
  )
  
  timezone <- MazamaSpatialUtils::getTimezone(
    lon = longitude,
    lat = latitude,
    dataset = "OSMTimezones",
    useBuffering = TRUE
  )
  
  locationID <- createLocationID(
    longitude = longitude,
    latitude = latitude,
    countryCode = countryCode,
    stateCode = stateCode
  )
  
  # ----- Return ---------------------------------------------------------------
  
  locationTbl <- dplyr::tibble(
    "locationID" = locationID,
    "longitude" = longitude,
    "latitude" = latitude,
    "elevation" = elevation,
    "countryCode" = countryCode,
    "stateCode" = stateCode,
    "timezone" = timezone
  )
  
  return(locationTbl)
  
}
