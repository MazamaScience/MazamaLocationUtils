
#' @title Update a single known location record in a table
#' @description Information in the \code{locationList} is used to replace
#' existing information found in \code{locationTbl}. This function can be used
#' for small tweaks to an existing \code{locationTbl}. Wholesale replacement of
#' records should be performed with \code{location_removeLocation()} followed by
#' \code{location_addLocation()}. 
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param locationList List containing `locationID` and one or more named
#' columns whose values are to be replaced, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @seealso \link{table_addLocation}
#' @seealso \link{table_addSingleLocation}
#' @seealso \link{table_removeLocation}
#' @rdname table_updateSingleLocation
#' @export 
#' @importFrom rlang .data
table_updateSingleLocation <- function(
  locationTbl = NULL,
  locationList = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(locationList)

  invalidNames <- setdiff(names(locationList), names(locationTbl))
  if ( length(invalidNames) > 0 ) {
    invalidString <- paste0(invalidNames, collapse = ", ")
    stop(sprintf(
      "Invalid names found in locationList: %s", invalidString
    ))
  }
  
  if ( !locationList$locationID %in% locationTbl$locationID ) 
    stop(sprintf(
      "locationID %s is not found in locationTbl", locationList$locationID
    ))
  
  # ----- Update locationTbl ---------------------------------------------------
  
  row <- which(locationTbl$locationID == locationList$locationID)
  
  if ( length(row) > 0 ) {
    
    for ( columnName in names(locationList) ) {
      locationTbl[row, columnName] <- locationList[[columnName]]
    }
    
  } else {
    
    if ( verbose ) {
      warning(sprintf(
        "No location found for %s", locationID
      ))
    }
    
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
