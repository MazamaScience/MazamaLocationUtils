
#' @title Create an empty "known location" table
#' @description Creates an empty "known location" tibble with the following columns:
#' \itemize{
#' \item{locationID}
#' \item{longitude}
#' \item{latitude}
#' }
#' and whatever extra \code{metadataNames} are passed in.
#' @param collectionName Character identifier for this table, Default: NULL
#' @param metadataNames Character names of supported spatial metadata, Default: c("countryCode", "stateCode", "timezone")
#' @return Empty "known location" tibble with the specified metadata columns.
#' @details TODO
#' @examples 
#' \dontrun{
#' emptyTbl <- createLocationTable(
#'   "myLocations",
#'   metadataNames = c("countryCode", "stateCode", "elevation")
#' )
#' }
#' @rdname createLocationTable
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter
#' @importFrom rlang .data
createLocationTable <- function(
  collectionName = NULL,
  metadataNames = c("countryCode", "stateCode", "timezone")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collectionName)
  MazamaCoreUtils::stopIfNull(metadataNames)
  
  # Compare against package supported metadata  
  invalidNames <- setdiff(metadataNames, validMetadataNames)
  if ( length(invalidNames) > 0 ) {
    invalidNamesString <- paste0(invalidNames, collapse = ", ")
    stop("metadataNames contains invalid names: \"", invalidNamesString, "\"")
  }
  
  dataDir <- getLocationDataDir()
  
  # ----- Create empty tibble --------------------------------------------------
  
  # TODO:  Is there a better way to do this?
  
  # Build up a tibble with a single record full of NAs
  locationTbl <- dplyr::tibble(
    "locationID" = as.character(NA),
    "longitude" = as.numeric(NA),
    "latitude" = as.numeric(NA)
  )
  
  for ( name in metadataNames ) {
    locationTbl[[name]] <- as.character(NA)
  }

  # Now search for an ID we won't find to end up with an empty tibble with 
  # the correct column names.
  locationTbl <-
    dplyr::filter(.data$locationID == "Rumplestiltskin")

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
