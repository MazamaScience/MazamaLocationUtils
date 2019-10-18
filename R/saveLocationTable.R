
#' @title saveLocationTable
#' @description Save a "known location" tibble to the preferred directory.
#' @param locationTbl Tibble of "known locations", Default: NULL
#' @param collectionName Character identifier for this table, Default: NULL
#' @return File path of saved file.
#' @details TODO
#' @rdname saveLocationTable
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
saveLocationTable <- function(
  locationTbl = NULL,
  collectionName = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(collectionName)
  
  dataDir <- getLocationDataDir()
  
  # TODO: validate locationTbl
  
  # ----- Save data ------------------------------------------------------------
  
  result <- try({
    
    fileName <- paste0(collectionName, ".rda")
    filePath <- file.path(dataDir, fileName)
    save(list="locationTbl")
    
    # Assign a name and save the data
    assign(collectionName, locationTbl)
    save(list=c(collectionName), file = filePath)
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(filePath))
  
}
