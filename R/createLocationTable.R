
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
    dplyr::filter(rlang::.data$locationID == "Rumplestiltskin")

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
