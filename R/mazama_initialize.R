
#' @title Initialize with MazamaScience standard directories
#' @description Convenience function to initialize properly.
#' @return No return value.
#' @rdname mazama_initialize
#' @export 
#' @importFrom MazamaSpatialUtils setSpatialDataDir loadSpatialData
#' 
mazama_initialize <- function() {

  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("EEZCountries")
  loadSpatialData("OSMTimezones")
  loadSpatialData("NaturalEarthAdm1")
  
  setLocationDataDir("~/Data/KnownLocations")
  
}