
#' @title Returns known locations
#' @description Returns a subset tibble with all locations found in 
#' \code{locationTbl} that are within \code{radius} meters of the incoming 
#' locations.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @param nearestIfMultiple Logical specifying whether to return the nearest
#' single location if multiple locations are found within \code{radius}, 
#' Default: FALSE
#' @return Tibble of known locations
#' @rdname getLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
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
        "x" = locationTbl$longitude,
        "y" = locationTbl$latitude
      ),
      y = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      paired = FALSE,
      sequential = FALSE,
      pad = FALSE,
      measure = "cheap"
    )
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Subset ---------------------------------------------------------------
  
  # For each incoming location, calculate mask, count and index
  
  foundLocationMask <- apply(distance, 2, function(x) { 
    any(x < radius)
  })
  
  foundLocationCount <- apply(distance, 2, function(x) { 
    length(which(x < radius)) 
  })
  
  closestLocationIndex <- apply(distance, 2, function(x) {
    which(x == min(x, na.rm = TRUE))
  })
  
  indices <- closestLocationIndex[foundLocationMask]
  
  # Deal with multiple found locations per incoming longitude.
  # This can happen when radius is large.
  if ( any(foundLocationCount > 1) &&
       !nearestIfMultiple ) {
    
    stop(paste0(
      "Multiple known locations found per incoming location.\n\n",
      "Try reducing the radius or setting \"nearestIfMultiple = TRUE\"."
    ))
    
  } else {
    
    subsetTbl <- locationTbl[indices,]
    
  }
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(subsetTbl)
  
}
