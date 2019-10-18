
loadLocationTable <- function(
  collectionName = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collectionName)
  
  dataDir <- getLocationDataDir()
  
  # ----- Load data ------------------------------------------------------------
  
  result <- try({
    
    fileName <- paste0(collectionName, ".rda")
    filePath <- file.path(dataDir, fileName)
    locationTbl <- get(load(filePath))
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
