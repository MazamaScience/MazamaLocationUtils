
#' @title Create an empty known location table
#' @description Creates an empty known location tibble with the following 
#' columns of core metadata:
#' \itemize{
#' \item{locationID}
#' \item{locationName}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{county}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{zip}
#' }
#' 
#' and whatever extra \code{spatialMetadata} are passed in.
#' @param spatialMetadata Vector of character names of supported spatial 
#' metadata, Default: NULL
#' metadata, Default: NULL
#' @return Empty known location tibble with the specified metadata columns.
#' @details TODO
#' @examples 
#' \dontrun{
#' emptyTbl <- location_initializeTable(
#'   "myLocations",
#'   spatialMetadata = c("USCensusCounties")
#' )
#' }
#' @rdname location_initializeTable
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter
#' @importFrom rlang .data
location_initializeTable <- function(
  spatialMetadata = NULL
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  # Compare against package supported metadata  
  invalidNames <- setdiff(spatialMetadata, validMetadataNames)
  if ( length(invalidNames) > 0 ) {
    invalidNamesString <- paste0(invalidNames, collapse = ", ")
    stop("spatialMetadata contains invalid names: \"", invalidNamesString, "\"")
  }
  
  # ----- Create empty tibble --------------------------------------------------
  
  # Build up a tibble with a single record full of NAs
  locationTbl <- dplyr::tibble(
    "locationID" = as.character(NA),
    "locationName" = as.character(NA),
    "longitude" = as.numeric(NA),
    "latitude" = as.numeric(NA),
    "elevation" = as.numeric(NA),
    "countryCode" = as.character(NA),
    "stateCode" = as.character(NA),
    "county" = as.character(NA),
    "timezone" = as.character(NA),
    "houseNumber" = as.character(NA),
    "street" = as.character(NA),
    "city" = as.character(NA),
    "zip" = as.character(NA)
  )
  
  # TODO:  Is there a better way to do this?
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
