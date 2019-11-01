
#' @title Initialize with MazamaScience standard directories
#' @description Convenience function to initialize spatial data. Wraps the 
#' following setup lines:
#' 
#' \preformatted{
#' MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
#' 
#' MazamaSpatialUtils::loadSpatialData("EEZCountries")
#' MazamaSpatialUtils::loadSpatialData("OSMTimezones")
#' MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
#' MazamaSpatialUtils::loadSpatialData("USCensusCounties")
#' }
#' @return No return value.
#' @rdname mazama_initialize
#' @export 
#' @importFrom MazamaSpatialUtils setSpatialDataDir loadSpatialData
#' 
mazama_initialize <- function() {
  
  # Is everything already initialized?
  result <- try({
    validateMazamaSpatialUtils() # swallow warning messages
  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {
    
    # Not initialized, so try to initialize
    result <- try({
      MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
      MazamaSpatialUtils::loadSpatialData("EEZCountries")
      MazamaSpatialUtils::loadSpatialData("OSMTimezones")
      MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
      MazamaSpatialUtils::loadSpatialData("USCensusCounties")
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      # Installation failed so spit out warning messages
      validateMazamaSpatialUtils()
    }
   
  }
  
}