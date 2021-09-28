#'
#' @title Finds overlapping locations in a known locations table.
#' 
#' @description Calculates distances between all locations within a known
#' locations table and returns a tibble with the row indices and separation 
#' distances of those records with overlapping locations.
#' 
#' It is useful when working with new metadata tables to identify overlapping
#' locations early on so that decisions can be made about the apporpriateness
#' of the specified \code{radius}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param radius Radius in meters.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' 
#' @return Tibble of row indices and distances for those locations which overlap. 
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' meta <- wa_airfire_meta
#' 
#' # Anything locations closer than 2 km? (diameter = 2*radius)
#' meta %>%
#'   table_findOverlappingLocations(radius = 1000) %>%
#'   dplyr::select(monitorID, siteName, timezone)
#' 
#' # How about 4 km?
#' meta %>%
#'   table_findOverlappingLocations(radius = 2000) %>%
#'   dplyr::select(monitorID, siteName, timezone)
#' 
#' @rdname table_findOverlappingLocations
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_findOverlappingLocations <- function(
  locationTbl = NULL,
  radius = NULL,
  measure = "geodesic"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(radius)
  
  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  # ----- Subset locationTbl ---------------------------------------------------
  
  overlappingTbl <- table_findOverlappingDistances(locationTbl, radius, measure)
  indices <- c(dplyr::pull(overlappingTbl, 1), dplyr::pull(overlappingTbl, 2))
  tooCloseTbl <- locationTbl[indices,]
  
  # ----- Return ---------------------------------------------------------------
  
  return(tooCloseTbl)
  
}
