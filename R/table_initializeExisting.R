
#' @title Converts an existing table into a known location table
#' 
#' @description An existing table may have much of the data that is needed
#' for a known location table. This function accepts an incoming table and
#' searches for required columns:
#' 
#' \itemize{
#' \item{locationID}
#' \item{locationName}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{county}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{zip}
#' }
#' 
#' The \code{longitude} and \code{latitude} columns are required but all others
#' are optional.
#' 
#' If any of these optional columns are found, they will be used and the often 
#' slow and sometimes slightly inaccurate steps to generate that information
#' will be skipped for locations with that data. Any additional columns of 
#' information not part of the required core metadata will be retained.
#' 
#' This method skips the assignment of columns like \code{elevation} and all
#' address related fields that require web service requests.
#' 
#' Compared to initializing a brand new table and populating one record at a
#' time, this is a much faster way of creating a known location table from a
#' pre-existing table of metadata.
#' 
#' @param locationTbl Tibble of known locations. This input tibble need not be a 
#' standardized "known location" with all required columns. They will be added.
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param countryCodes Vector of country codes used to optimize spatial
#' searching. (See ?MazamaSpatialUtils::getStateCode())
#' @param radius Radius in meters. 
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap" 
#' specifying desired method of geodesic distance calculation. See \code{?geodist::geodist}.
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Known location tibble with the specified metadata columns. Any 
#' locations whose circles (as defined by \code{radius}) overlap will generate
#' warning messages. 
#' 
#' It is incumbent upon the user to address these issue by one of:
#' 
#' \enumerate{
#' \item{reduce the radius until no overlaps occur}
#' \item{assign one of the overlapping locations to the other location}
#' }
#' 
#' @rdname table_initializeExisting
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_initializeExisting <- function(
  locationTbl = NULL,
  stateDataset = "NaturalEarthAdm1",
  countryCodes = NULL,
  radius = NULL,
  measure = "geodesic",
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(radius)
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  if ( "locationID" %in% names(locationTbl) )
    stop("Parameter 'locationTbl' already has a column named \"locationID\"")
  
  if ( !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  diameter <- 2 * round(radius)
  
  # ----- Create locationTbl ---------------------------------------------------
  
  locationTbl <- table_addCoreMetadata(locationTbl)
  
  # * locationID -----
  
  # locationID should have been added by table_add
  if (anyNA(locationTbl$locationID)) {
    locationTbl$locationID <- location_createID(
      longitude = locationTbl$longitude,
      latitude = locationTbl$latitude
    )
  }
  
  # * elevation -----
  
  # Slow web service so skip for now
  
  # * countryCode -----
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$countryCode))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$countryCode))
  
  if ( verbose ) 
    message(sprintf("Creating countryCodes for %d locations ...", nrow(tbl_2)))
  
  tbl_2$countryCode <- MazamaSpatialUtils::getCountryCode(
    lon = tbl_2$longitude,
    lat = tbl_2$latitude,
    dataset = "EEZCountries",
    countryCodes = countryCodes,
    useBuffering = FALSE
  )
  
  locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  # * stateCode -----
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$stateCode))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$stateCode))
  
  if ( verbose ) 
    message(sprintf("Creating stateCodes for %d locations ...", nrow(tbl_2)))
  
  tbl_2$stateCode <- MazamaSpatialUtils::getStateCode(
    lon = tbl_2$longitude,
    lat = tbl_2$latitude,
    dataset = stateDataset,
    countryCodes = countryCodes,
    useBuffering = TRUE
  )
  
  locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  # * locationName -----
  
  # NOTE:  The default locationName is intended to give folks a more memorable
  # NOTE:  handel than the locationID but is not guaranteed to be unique. It is 
  # NOTE:  expected that users will add their own, more relevant names 
  # NOTE:  appropriate for the community of practice using a particular
  # NOTE:  collectionName of known locations.
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$locationName))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$locationName))
  
  if ( verbose ) 
    message(sprintf("Creating locationNames for %d locations ...", nrow(tbl_2)))
  
  tbl_2$locationName <- paste0(
    tolower(tbl_2$countryCode), ".",
    tolower(tbl_2$stateCode), "_",
    stringr::str_sub(tbl_2$locationID, 1, 6)
  )
  
  locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  # * county -----
  
  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$county))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$county))
  
  if ( verbose ) 
    message(sprintf("Creating counties for %d locations ...", nrow(tbl_2)))
  
  tbl_2$county <- MazamaSpatialUtils::getUSCounty(
    lon = tbl_2$longitude,
    lat = tbl_2$latitude,
    dataset = "USCensusCounties",
    useBuffering = TRUE
  )
  
  locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  # * timezone -----

  tbl_1 <- dplyr::filter(locationTbl, !is.na(.data$timezone))
  tbl_2 <- dplyr::filter(locationTbl, is.na(.data$timezone))
  
  if ( verbose ) 
    message(sprintf("Creating timezones for %d locations ...", nrow(tbl_2)))
  
  tbl_2$county <- MazamaSpatialUtils::getTimezone(
    lon = tbl_2$longitude,
    lat = tbl_2$latitude,
    dataset = "OSMTimezones",
    useBuffering = TRUE
  )
  
  locationTbl <- dplyr::bind_rows(tbl_1, tbl_2)
  
  # * houseNumber -----
  
  # Slow web service so skip for now
  
  # * street -----
  
  # Slow web service so skip for now
  
  # * city -----
  
  # Slow web service so skip for now

  # * zip -----
  
  # Slow web service so skip for now

  # ----- Check for locations that are too close -------------------------------
  
  # Calculate distances between each location
  distances <- geodist::geodist(locationTbl, measure = "geodesic")
  
  # Get distances that are less than the given diameter
  # NOTE: the distance between a location and itself is always zero
  distancesLessThanR <- (distances != 0) & (distances < diameter )
  
  # Select the locations that are "too close".
  tooClose <- which(distancesLessThanR > 0, arr.ind = TRUE)
  
  if ( nrow(tooClose) > 0 ) {
    
    # NOTE:  If location a and b are too close, two entries will be returned:
    # NOTE:        row  col
    # NOTE:   [#,]  a    b
    # NOTE:    ...
    # NOTE:   [#,]  b    a
    #
    # NOTE:  While often the case, there is no guarantee that complementary
    # NOTE:  rows will be adjacent to eachother. The next couple of lines
    # NOTE:  find the rows that have the same indices and reduce the table to
    # NOTE:  only unique pairs.
    
    sortedMatrix <- t(apply(tooClose, 1, sort))
    tooClose <- sortedMatrix[!duplicated(sortedMatrix),]
    
    tooCloseCount <- nrow(tooClose)
    
    # Format the first line of the warning message
    firstLine <- sprintf(
      "%d locations have neighbors that are < %d m apart\n",
      round(tooCloseCount),
      diameter
    )
    
    # Create a vector of lines, on for each tooClose location pair
    tooCloseLines <- vector("character", length = tooCloseCount)
    for ( i in seq_len(nrow(tooClose)) ) {
      
      dist <- distances[tooClose[i, 1], tooClose[i, 2]]
      tooCloseLines[i] <- sprintf(
        "Distance: %6.1f -- rows %s %s",
        round(dist, 1),
        tooClose[i, 1],
        tooClose[i, 2]
      )
      
    }
    
    instructions <- "
The presence of locations closer than twice the specified radius invalidate the 
uniqueness of a 'known locations' table and should be rectified. There are two 
basic options:

  1) Reduce the radius to less than the minimum distance.
  2) Manually merge nearby locations to share the same longitude, latitude and
     locationID
     
Please review the returned locationTbl for the identified rows.

  "
    
    lines <- c(firstLine, tooCloseLines, instructions)
    
    # Paste the lines together
    warning(paste(lines, collapse = "\n"))
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
