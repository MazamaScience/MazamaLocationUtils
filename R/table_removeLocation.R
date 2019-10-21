
#' @title Removes location records from a table
#' @description Incoming \code{locationID} values are compared 
#' against the incoming \code{locationTbl} and any matches are removed.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param locationID Vector \code{locationID} strings, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @seealso \link{table_addLocation}
#' @rdname table_removeLocation
#' @export 
#' @importFrom rlang .data
table_removeLocation <- function(
  locationTbl = NULL,
  locationID = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(locationID)

  # ----- Subset locationTbl ---------------------------------------------------
  
  locationTbl <-
    locationTbl %>%
    dplyr::filter(!.data$locationID %in% !!locationID)

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
