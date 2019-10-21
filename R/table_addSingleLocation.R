
#' @title Adds a single new known location record to a table
#' @description Incoming \code{longitude} and \code{latitude} values are compared 
#' against the incoming \code{locationTbl} to see if the are already within
#' \code{radius} meters of an existing entry.  A new record is created for
#' if the location is not already found in \code{locationTbl}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @seealso \link{table_addLocation}
#' @seealso \link{table_addSingleLocation}
#' @seealso \link{table_removeLocation}
#' @rdname table_addSingleLocation
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
table_addSingleLocation <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL,
  stateDataset = "NaturalEarthAdm1",
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)
  MazamaCoreUtils::stopIfNull(stateDataset)
  
  if ( length(longitude) > 1 || length(latitude) > 1 ) {
    stop(paste0(
      "Please use the plural version of the funcion for adding multiple locations.\n",
      "  table_addLocation(...)\n"
    ))
  }
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  # ----- Check for existing location ------------------------------------------
  
  locationID <- table_getLocationID(locationTbl, longitude, latitude, radius)
  
  if ( !is.na(locationID) ) {
    stop(sprintf(
      "The known location %s already exists < %d meters from the requested location.",
      locationID, radius
    ))
  }
  
  # ----- Add new record -------------------------------------------------------

  singleRecordTbl <- location_initialize(
    longitude = longitude,
    latitude = latitude,
    stateDataset = stateDataset,
    verbose = verbose
  )
    
  additionalNames <- setdiff( names(locationTbl), names(singleRecordTbl))
  
  # TODO:  Create additional spatial metadata by calling a function that
  # TODO:  handles everything.
  
  for ( name in additionalNames ) {
    
    print(sprintf("Adding NA in place of actual metadata for %s", name))
    singleRecordTbl[[name]] <- NA
    
  }
  
  locationTbl <- dplyr::bind_rows(locationTbl, singleRecordTbl)
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}