
#' @title Return distances to nearest known locations
#' @description Returns distances from known locations in \code{locationTbl},
#' one for each incoming location. If no known location is found within
#' \code{radius} meters for a particular
#' incoming location, that distance in the vector will be \code{NA}.
#' @param locationTbl Tibble of known locations.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param radius Radius in meters.
#' @return Vector of distances from known locations.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a radius will not find a match
#' table_getNearestDistance(locationTbl, lon, lat, radius = 50)
#' 
#' # Expanding the radius will find one
#' table_getNearestDistance(locationTbl, lon, lat, radius = 5000)
#' @rdname table_getNearestDistance
#' @export
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getNearestDistance <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaLocationUtils::validateLonsLats(longitude, latitude)
  MazamaCoreUtils::stopIfNull(radius)

  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  radius <- round(radius)

  # ----- Calculate distances --------------------------------------------------
  
  distance <-
    geodist::geodist(
      y = cbind(
        "x" = longitude,
        "y" = latitude
      ),
      x = cbind(
        "x" = locationTbl$longitude,
        "y" = locationTbl$latitude
      ),
      paired = FALSE,
      sequential = FALSE,
      pad = FALSE,
      measure = "geodesic"
    )
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Find locationIDs -----------------------------------------------------
  
  nearestDistance <- rep(as.numeric(NA), length(longitude))
  
  for ( index in seq_along(longitude) ) {
    
    if ( any(distance[,index] <= radius) ) {
      nearestDistance[index] <- min(distance[,index])
    }
    
  }
  
  
  # ----- Return ---------------------------------------------------------------

  return(nearestDistance)

}
