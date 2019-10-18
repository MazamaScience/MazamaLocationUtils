
initializeLocationRecord <- function(
  longitude = NULL,
  latitude = NULL,
  stateDataset = "NaturalEarthAdm1"
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
    "countryCode" = countryCode,
    "stateCode" = stateCode,
    "timezone" = timezone
  )
  
  return(locationTbl)
  
}