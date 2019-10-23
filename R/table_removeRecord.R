
#' @title Remove location records from a table
#' @description Incoming \code{locationID} values are compared 
#' against the incoming \code{locationTbl} and any matches are removed.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param locationID Vector of \code{locationID} strings, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @examples
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Get the locationID first
#' locationID <- table_getLocationID(locationTbl, lon, lat, radius = 500)
#' 
#' # Remove it
#' locationTbl <- table_removeRecord(locationTbl, locationID)
#' 
#' # Test
#' table_getLocationID(locationTbl, lon, lat, radius = 500)
#' @seealso \link{table_addLocation}
#' @seealso \link{table_addSingleLocation}
#' @seealso \link{table_updateSingleRecord}
#' @rdname table_removeRecord
#' @export 
#' @importFrom rlang .data
table_removeRecord <- function(
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