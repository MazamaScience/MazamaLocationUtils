
#' @title Save a known location table
#' @description Save a tibble of known locations to the preferred directory.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param collectionName Character identifier for this table, Default: NULL
#' @param backup Logical specifying whether to save a backup version of any
#' existing tables sharing \code{collectionName}.
#' @return File path of saved file.
#' @details Backup files are saved with "YYYY-mm-ddTHH:MM:SS"
#' @rdname table_save
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom lubridate now
table_save <- function(
  locationTbl = NULL,
  collectionName = NULL,
  backup = TRUE
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

    # Assign a name
    assign(collectionName, locationTbl)
    
    # Save backups
    if ( backup && file.exists(filePath) ) {
      backupName <- paste0(
        collectionName, ".",
        strftime(lubridate::now(), "%Y-%m-%dT%H:%M:%S"),
        ".rda"
      )
      backupPath <- file.path(dataDir, backupName)
      file.rename(filePath, backupPath)
    }
    
    # Save the data
    save(list=c(collectionName), file = filePath)
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(filePath))
  
}
