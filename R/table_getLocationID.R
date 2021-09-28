
#' @title Return IDs of known locations
#' @description Returns a vector of \code{locationID}s for the known locations
#' that each incoming location will be assigned to within the given. If more
#' than one known location exists within the given radius, the closest will be
#' assigned.  \code{NA} will be returned for each incoming that cannot be 
#' assigend to a known location in \code{locationTbl}.
#' @param locationTbl Tibble of known locations.
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param radius Radius in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' @return Vector of known \code{locationID}s.
#' @examples
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' 
#' # Too small a radius will not find a match
#' table_getLocationID(locationTbl, lon, lat, radius = 50)
#' 
#' # Expanding the radius will find one
#' table_getLocationID(locationTbl, lon, lat, radius = 5000)
#' @rdname table_getLocationID
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom geodist geodist
#' @importFrom rlang .data
table_getLocationID <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL,
  measure = "geodesic"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
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
      measure = measure
    )
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Find locationIDs -----------------------------------------------------
  
  locationID <- rep(as.character(NA), length(longitude))
  
  for ( index in seq_along(longitude) ) {
    
    if ( any(distance[,index] <= radius) ) {
      row <- which(distance[,index] == min(distance[,index]))
      locationID[index] <- locationTbl$locationID[row]
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}
