
exportLocationTable <- function(
  locationTbl = NULL,
  outputType = "csv"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::setIfNull(outputType, "csv")
  
  validOutputTypes <- c("csv")
  outputType <- tolower(outputType)
  if ( !outputType %in% validOutputTypes )
    stop(paste0("outputType \"", outputType, "\" is not recognized"))
  
  dataDir <- getLocationDataDir()
  
  # ----- Create export --------------------------------------------------------
  
  result <- try({
    
    if ( outputType == "csv" ) {
      content <- readr::format_csv(locationTbl)
    }
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(content)
  
}
