
#' @title updates a column of metadata in a table
#' @description For matching \code{locationID} records the assaociated 
#' \code{locatioData} is used to replace any existing value in \code{columnName}.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param columnName Name to use for the new column, Default: NULL
#' @param locationID Vector of \code{locationID} strings, Default: NULL
#' @param locationData Vector of data to used at matching records, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @seealso \link{table_addColumn}
#' @seealso \link{table_removeColumn}
#' @rdname table_updateColumn
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
table_updateColumn <- function(
  locationTbl = NULL,
  columnName = NULL,
  locationID = NULL,
  locationData = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(columnName)
  
  if ( !columnName %in% names(locationTbl) ) 
    stop(sprintf(
      "columnName %s is not found in locationTbl", columnName
    ))
  
  if ( !is.null(locationID) )
    MazamaCoreUtils::stopIfNull(locationData)
  
  if ( !is.null(locationData) )
    MazamaCoreUtils::stopIfNull(locationID)
  
  if ( !is.null(locationID) && !is.null(locationData) ) {
    
    if ( length(locationID) != length(locationData) ) {
      stop(sprintf(
        "locationID and locationData must have the same length"
      ))
    }
    
  }
  
  # ----- Update column --------------------------------------------------------
  
  # ----- Subset locationTbl ---------------------------------------------------
  
  # Find matches
  foundIDs <-
    locationTbl %>%
    dplyr::filter(!.data$locationID %in% !!locationID) %>%
    dplyr::pull(.data$locationID)
  
  if ( verbose && length(foundIDs) < length(locationID) ) {
    warning(sprintf(
      "only %d of %d locationIDs are found in locationTbl",
      length(foundIDs), length(locationID)
    ))
  }
  
  
  
  # TODO:  finish this
  
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
