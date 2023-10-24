#'
#' @title Create one or more unique locationIDs
#' @description A unique locationID is created for each incoming
#' \code{longitude} and \code{latitude}. 
#' 
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:createLocationID]{createLocationID}} for details.
#' 
#' @note
#' This function calls \code{geohashTools::\link[geohashTools:gh_encode]{gh_encode}} with
#' \code{precision = 10} which results in a maximum error of 0.6 meters which 
#' is more than precise enough for environmental monitoring studies making use
#' of this package.
#' 
#' @param longitude Vector of longitudes in decimal degrees E.
#' @param latitude Vector of latitudes in decimal degrees N.
#' @param algorithm Algorithm to use -- either \code{"geohash"} or \code{"digest"}.
#' @return Vector of character locationIDs.
#' @examples
#' library(MazamaLocationUtils)
#' 
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' locationID <- location_createID(lon, lat)
#' print(locationID)
#' 
#' geohashID <- location_createID(lon, lat, algorithm = "geohash")
#' print(geohashID)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Decimal_degrees}
#' @references \url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
#' @rdname location_createID
#' @export 
#' 
location_createID <- function(
  longitude = NULL,
  latitude = NULL,
  algorithm = c("geohash", "digest")
) {
  
  algorithm <- match.arg(algorithm)

  returnVal <- MazamaCoreUtils::createLocationID(longitude, latitude, algorithm)
  return(returnVal)
  
}