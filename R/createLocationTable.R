
#' @title Create an empty "known location" table
#' @description Creates an empty "known location" tibble with the following columns:
#' \itemize{
#' \item{locationID}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{timezone}
#' }
#' and whatever extra \code{spatialMetadata} are passed in.
#' @param spatialMetadata Vector of character names of supported spatial 
#' metadata, Default: NULL
#' metadata, Default: NULL
#' @return Empty "known location" tibble with the specified metadata columns.
#' @details TODO
#' @examples 
#' \dontrun{
#' emptyTbl <- createLocationTable(
#'   "myLocations",
#'   spatialMetadata = c("USCensusCounties")
#' )
#' }
#' @rdname createLocationTable
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter
#' @importFrom rlang .data
createLocationTable <- function(
  spatialMetadata = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Compare against package supported metadata  
  invalidNames <- setdiff(spatialMetadata, validMetadataNames)
  if ( length(invalidNames) > 0 ) {
    invalidNamesString <- paste0(invalidNames, collapse = ", ")
    stop("spatialMetadata contains invalid names: \"", invalidNamesString, "\"")
  }
  
  # ----- Create empty tibble --------------------------------------------------
  
  # TODO:  Is there a better way to do this?
  
  # Build up a tibble with a single record full of NAs
  locationTbl <- dplyr::tibble(
    "locationID" = as.character(NA),
    "longitude" = as.numeric(NA),
    "latitude" = as.numeric(NA),
    "elevation" = as.numeric(NA),
    "countryCode" = as.character(NA),
    "stateCode" = as.character(NA),
    "timezone" = as.character(NA)
  )
  
  for ( name in spatialMetadata ) {
    locationTbl[[name]] <- as.character(NA)
  }

  # Now search for an ID we won't find to end up with an empty tibble with 
  # the correct column names.
  locationTbl <-
    locationTbl %>%
    dplyr::filter(.data$locationID == "Rumplestiltskin")

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
