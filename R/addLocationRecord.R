
addLocationRecord <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL,
  stateDataset = "NaturalEarthAdm1"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)
  MazamaCoreUtils::stopIfNull(stateDataset)
  
  # TODO:  Validate stateDataset
  
  # ----- Check for existing location ------------------------------------------
  
  # TODO:  Need to handle the return of multiple records.
  
  location <- getLocationRecord(
    longitude = longitude,
    latitude = latitude
  )
  
  if ( !is.null(location) ) {
    stop(sprintf(
      "An existing location was found within %d m of %f, %f: %s",
      radius, 
      round(longitude, 5),
      round(latitude, 5), 
      location$locationID
    ))
  }
  
  # ----- Add new record -------------------------------------------------------

  locationRecord <- initializeLocationRecord(
    longitude = longitude,
    latitude = latitude,
    stateDataset = stateDataset
  )
    
  additionalNames <- setdiff( names(locationTbl), names(locationRecord))
  
  # TODO:  Create additional spatial metadata by calling a function that
  # TODO:  handles everything
  
  for ( name in additionalNames ) {
    
    print(sprintf("Adding NA in place of actual metadata for %s", name))
    locationRecord[[name]] <- NA
    
  }
  
  locationTbl <- dplyr::bind_rows(locationTbl, locationRecord)
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}