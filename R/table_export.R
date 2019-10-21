
#' @title Export a known location table
#' @description Export a known location tibble as CSV or other format.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param outputType Output format, Default: 'csv'
#' @return Representation of a known location table in the desired format.
#' @details Loads
#' @rdname table_export
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull setIfNull
#' @importFrom readr format_csv
table_export <- function(
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
