
#' @title Adds new known location records to a table
#' @description Incoming \code{longitude} and \code{latitude} values are compared 
#' against the incoming \code{locationTbl} to see if the are already within
#' \code{radius} meters of an existing entry. A new record is created for
#' each location that is not already found in \code{locationTbl}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @seealso 
#'  \code{\link{addLocation}}
#' @rdname addLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
addLocations <- function(
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
  
  if ( length(longitude) != length(latitude) )
    stop("longitude and latitude must have the same length")
  
  if ( length(radius) != 1 )
    stop("radius must be of length 1")
  
  incomingLength <- length(longitude)
  
  # Remove location pairs if either is missing
  naMask <- is.na(longitude) | is.na(latitude)
  longitude <- longitude[!naMask]
  latitude <- latitude[!naMask]
  
  if ( verbose && any(naMask) ) {
    message(sprintf(
      "%d of the %d requested locations have NA values",
      length(which(naMask)), incomingLength
    ))
  }
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  # ----- Reduce to only new locations -----------------------------------------

  # No need to do anything if this is an empty locationTbl
  if ( nrow(locationTbl) > 0 ) {
    
    # Calculate a distance matrix where each row has all existing distances from
    # one of the incoming locations.
    distance <-
      geodist::geodist(
        x = cbind(
          "x" = longitude,
          "y" = latitude
        ),
        y = cbind(
          "x" = locationTbl$longitude,
          "y" = locationTbl$latitude
        )
      )
    
    existingLocationMask <- apply(distance, 1, function(x) { any(x < radius) })
    
    if ( verbose && any(existingLocationMask) ) {
      message(sprintf(
        "%d of the %d requested locations are already found in the locationTbl",
        length(which(existingLocationMask)), incomingLength
      ))
    }
    
    longitude <- longitude[!existingLocationMask]
    latitude <- latitude[!existingLocationMask]
    
  }
  
  # ----- Loop over new locations ----------------------------------------------

  # NOTE:  Yes, this is a loop!
  # NOTE:  But the task of generating spatial data takes many seconds for each
  # NOTE:  iteration so there is nothing to be gained by making the code less
  # NOTE:  readable with functional progamming style.
  
  for ( i in seq_along(longitude) ) {
    
    result <- try({
      
      if ( verbose ) {
        message(sprintf(
          "Working on %.7f, %.7f ...",
          longitude[i], latitude[i]
        ))
      }
      locationTbl <- addLocation(
        locationTbl = locationTbl,
        longitude = longitude[i],
        latitude = latitude[i],
        radius = radius,
        stateDataset = stateDataset,
        verbose = verbose
      )
      
    }, silent = TRUE)
    
    if ( "try-error" %in% result ) {
      # Warn but don't stop
      warning(sprintf(
        "Skipping with error: ", geterrmessage()
      ))
    }
    
    # Be _somewhat_ careful with memory
    if ( 1 %% 10 == 0 ) {
      gc()
    }
    
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
