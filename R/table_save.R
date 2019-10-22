
#' @title Save a known location table
#' @description Save a tibble of known locations to the preferred directory.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param collectionName Character identifier for this table, Default: NULL
#' @param backup Logical specifying whether to save a backup version of any
#' existing tables sharing \code{collectionName}.
#' @param outputType Output format, Default: 'rda'
#' @return File path of saved file.
#' @details Backup files are saved with "YYYY-mm-ddTHH:MM:SS"
#' @rdname table_save
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom lubridate now
table_save <- function(
  locationTbl = NULL,
  collectionName = NULL,
  backup = TRUE,
  outputType = "rda"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(collectionName)
  
  dataDir <- getLocationDataDir()
  
  validOutputTypes <- c("rda", "csv")
  validTypesString <- paste0(validOutputTypes, collapse = ", ")
  if ( !outputType %in% validOutputTypes ) {
    stop(sprintf(
      "outputType \"%s\" is not recognized. Please use one of \"%s\"",
      outputType, validTypesString
    ))
  }
  
  # TODO: validate locationTbl
  
  # ----- Save data ------------------------------------------------------------
  
  result <- try({
    
    fileName <- paste0(collectionName, ".", outputType)
    filePath <- file.path(dataDir, fileName)

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
    
    if ( outputType == "rda" ) {
      
      # Assign a name and save
      assign(collectionName, locationTbl)
      save(list=c(collectionName), file = filePath)
      
    } else if ( outputType == "csv" ) {
      
      readr::write_csv(locationTbl, path = filePath)
      
    }
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(filePath))
  
}
