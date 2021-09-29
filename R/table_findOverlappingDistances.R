#'
#' @title Find overlapping distances in a known locations table.
#' 
#' @description Calculate distances between all locations within a known
#' locations table and returns a tibble with the row indices and separation 
#' distances of those records with overlapping locations.
#' 
#' It is useful when working with new metadata tables to identify overlapping
#' locations early on so that decisions can be made about the apporpriateness
#' of the specified \code{radius}.
#' 
#' @param locationTbl Tibble of known locations.
#' @param radius Radius in meters. Locations are guaranteed to be separatde by 2*R.
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
#' table_findOverlappingDistances(meta, radius = 1000)
#' 
#' # How about 4 km?
#' table_findOverlappingDistances(meta, radius = 2000)
#' 
#' 
#' @rdname table_findOverlappingDistances
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_findOverlappingDistances <- function(
  locationTbl = NULL,
  radius = NULL,
  measure = "geodesic"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(radius)
  
  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  diameter <- 2 * round(radius)
  
  # ----- Calculate distances --------------------------------------------------

  # NOTE:  Suppress annoying message:
  #
  # Maximum distance is > 100km. The 'cheap' measure is inaccurate over such
  # large distances, you'd likely be better using a different 'measure'.
  
  suppressMessages({

    distance <-
      geodist::geodist(
        x = cbind(
          "x" = locationTbl$longitude,
          "y" = locationTbl$latitude
        ),
        paired = FALSE,
        sequential = FALSE,
        pad = FALSE,
        measure = measure
      )
    
  })
  
  # NOTE:  distance matrix is nrow(locationTbl) X length(longitude)
  
  # ----- Check for locations that are too close -------------------------------
  
  # Get distances that are less than the given diameter
  # NOTE: the distance between a location and itself is always zero
  distanceLessThan2R <- (distance != 0) & (distance < diameter )
  
  # Select the locations that are "too close".
  tooClose <- which(distanceLessThan2R > 0, arr.ind = TRUE)
  
  if ( nrow(tooClose) == 0 ) {
    
    # Return an empty tibble
    overlappingTbl <- 
      dplyr::tibble(
        row1 = 1,
        row2 = 1,
        distance = as.numeric(NA)
      ) %>% dplyr::filter(
        .data$row1 == -999
      )
    
  } else {
    
    # NOTE:  If location a and b are too close, two entries will be returned:
    # NOTE:        row  col
    # NOTE:   [#,]  a    b
    # NOTE:    ...
    # NOTE:   [#,]  b    a
    #
    # NOTE:  While often the case, there is no guarantee that complementary
    # NOTE:  rows will be sequential. The next couple of lines
    # NOTE:  find the rows that have the same indices and reduce the table to
    # NOTE:  only unique pairs.
    
    sortedMatrix <- t(apply(tooClose, 1, sort))
    tooClose <- sortedMatrix[!duplicated(sortedMatrix),]
    
    overlappingTbl <- dplyr::tibble(
      row1 = tooClose[,1],
      row2 = tooClose[,2],
      distance = as.numeric(NA)
    )
    
    for ( i in seq_len(nrow(tooClose)) ) {
      overlappingTbl$distance[i] <- 
        distance[overlappingTbl$row1[i], overlappingTbl$row2[i]]
    }
    
    overlappingTbl <- overlappingTbl %>% dplyr::arrange(.data$distance)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(overlappingTbl)
  
}
