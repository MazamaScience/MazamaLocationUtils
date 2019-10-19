
#' @title Returns "known locations"
#' @description Returns a subset tibble with all locations found in 
#' \code{locationTbl} that are within \code{radius} meters of the incoming 
#' locations.
#' @param locationTbl Tibble of "known locations", Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @param nearestIfMultiple Logical specifying whether to return the nearest
#' single location if multiple locations are found within \code{radius}, 
#' Default: FALSE
#' @return Tibble of "known locations"
#' @rdname getLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
getLocations <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL,
  nearestIfMultiple = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)
  
  radius <- round(radius)
  
  # ----- Calculate distances --------------------------------------------------
  
  distance <-
    geodist::geodist(
      x = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      y = cbind(
        "x" = locationTbl$longitude,
        "y" = locationTbl$latitude
      )
    )
  
  # ----- Subset ---------------------------------------------------------------
  
  subsetTbl <- locationTbl[which(distance <= radius),]
  
  if ( nrow(subsetTbl) > 1 ) {
    
    if ( nearestIfMultiple ) {
      subsetTbl <- locationTbl[which(distance == min(distance)),]
    }
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(subsetTbl)
  
}
