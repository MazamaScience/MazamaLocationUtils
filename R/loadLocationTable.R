
#' @title Locat a known location table
#' @description Load a tibble of known locations from the preferred directory.
#' @param collectionName Character identifier for this table, Default: NULL
#' @return Tibble of known locations.
#' @details TODO
#' @seealso 
#'  \code{\link{setLocationDataDir}}
#' @rdname loadLocationTable
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
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
