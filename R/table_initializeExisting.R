
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
#' will be skipped. Any additional columns of information not part of the 
#' required list will be retained.
#' 
#' This method skips the assignment of columns like \code{elevation} and all
#' address related foe;ds that require web service requests.
#' 
#' Compared to initializing a brand new table and populating one record at a
#' time, this is a much faster way of creating a known location table from a
#' pre-existing table of metadata.
#' 
#' @param tbl Table of spatial locations that will be converted into a "known
#' location" table.
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param countryCodes Vector of country codes used to optimize spatial
#' searching. (See ?MazamaSpatialUtils::getStateCode())
#' @param radius Maximum distance in meters between two locations that would
#' be considered "too close"
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Known location tibble with the specified metadata columns.
#' 
#' @rdname table_initializeExisting
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_initializeExisting <- function(
  tbl = NULL,
  stateDataset = "NaturalEarthAdm1",
  countryCodes = NULL,
  radius = NULL,
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(tbl)
  
  if ( !"data.frame" %in% class(tbl) )
    stop("Parameter 'tbl' is not of class \"data.frame\".")
  
  if ( !"longitude" %in% names(tbl) )
    stop("Parameter 'tbl' does not have a 'longitude' column.")
  
  if ( !"latitude" %in% names(tbl) )
    stop("Parameter 'tbl' does not have a 'latitude' column.")
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  if ( "locationID" %in% names(tbl) )
    stop("Parameter 'tbl' already has a column named \"locationID\"")
  
  if ( !is.null(radius) && !is.numeric(radius) )
    stop("Parameter 'radius' must be a numeric value.")
  
  # ----- Create locationTbl ---------------------------------------------------
  
  tblColumns <- names(tbl)
  
  locationTbl <- tbl

  # * locationID -----
  
  locationTbl$locationID <- location_createID(
    longitude = locationTbl$longitude,
    latitude = locationTbl$latitude
  )
  
  # * elevation -----
  
  if ( !"elevation" %in% tblColumns ) {
    # Slow web service so skip for now
    locationTbl$elevation <- as.numeric(NA)
  }
  
  # * countryCode -----
  
  if ( !"countryCode" %in% tblColumns ) {
    
    if ( verbose ) 
      message("Searching for countryCodes...")

    locationTbl$countryCode <- MazamaSpatialUtils::getCountryCode(
      lon = locationTbl$longitude,
      lat = locationTbl$latitude,
      dataset = "EEZCountries",
      countryCodes = countryCodes,
      useBuffering = FALSE
    )
    
  }
  
  # * stateCode -----
  
  if ( !"stateCode" %in% tblColumns ) {
    
    if ( verbose ) 
      message("Searching for stateCodes...")
    
    locationTbl$stateCode <- MazamaSpatialUtils::getStateCode(
      lon = locationTbl$longitude,
      lat = locationTbl$latitude,
      dataset = stateDataset,
      countryCodes = countryCodes,
      useBuffering = TRUE
    )
    
  }

  # * locationName -----
  
  if ( !"locationName" %in% tblColumns ) {
    
    # NOTE:  The default locationName is intended to give folks a more memorable
    # NOTE:  handel than the locationID but is not guaranteed to be unique. It is 
    # NOTE:  expected that users will add their own, more relevant names 
    # NOTE:  appropriate for the community of practice using a particular
    # NOTE:  collectionName of known locations.
    
    locationTbl$locationName <- paste0(
      tolower(locationTbl$countryCode), ".",
      tolower(locationTbl$stateCode), "_",
      stringr::str_sub(locationTbl$locationID, 1, 6)
    )
    
  }
  
  # * county -----
  
  if ( !"county" %in% tblColumns ) {
    
    if ( verbose ) 
      message("Searching for counties...")
    
    locationTbl$county <- MazamaSpatialUtils::getUSCounty(
      lon = locationTbl$longitude,
      lat = locationTbl$latitude,
      dataset = "USCensusCounties",
      useBuffering = TRUE
    )
    
  }
  
  # * timezone -----
  
  if ( !"timezone" %in% tblColumns ) {
    
    if ( verbose ) 
      message("Searching for timezones...")
    
    timezone <- MazamaSpatialUtils::getTimezone(
      lon = locationTbl$longitude,
      lat = locationTbl$latitude,
      dataset = "OSMTimezones",
      countryCodes = countryCodes,
      useBuffering = TRUE
    )
    
  }
  
  # * houseNumber -----
  
  if ( !"houseNumber" %in% tblColumns ) {
    # Slow web service so skip for now
    locationTbl$houseNumber <- as.character(NA)
  }
  
  # * street -----
  
  if ( !"street" %in% tblColumns ) {
    # Slow web service so skip for now
    locationTbl$street <- as.character(NA)
  }
  
  # * city -----
  
  if ( !"city" %in% tblColumns ) {
    # Slow web service so skip for now
    locationTbl$city <- as.character(NA)
  }
  
  # * zip -----
  
  if ( !"zip" %in% tblColumns ) {
    # Slow web service so skip for now
    locationTbl$zip <- as.character(NA)
  }
  
  # ----- Reorganize locationTbl -----------------------------------------------
  
  requiredColumns <- c(
    "locationID", "locationName", 
    "longitude", "latitude", "elevation", 
    "countryCode", "stateCode", "county", "timezone", 
    "houseNumber", "street", "city", "zip"
  )
  
  extraColumns <- setdiff(tblColumns, requiredColumns)
  
  # This is the preferred order
  allColumns <- c(requiredColumns, extraColumns)
  
  # TODO:  This doesn't seem to reorder like I thought it should.
  locationTbl <- dplyr::select(locationTbl, all_of(allColumns))
  
  
  # ----- Check for locations that are too close -------------------------------
  
  if ( !is.null(radius) ) {
    
    # Calculate distances between each location
    distances <- geodist::geodist(locationTbl)
    
    # Get distances that are less than the given radius
    # NOTE: the distance between a location and itself is always zero
    distancesLessThanR <- (distances != 0) & (distances < radius)
    
    # Select the locations that are "too close".
    tooClose <- which(distancesLessThanR > 0, arr.ind = TRUE)
    
    # NOTE: If location a and b are too close, two entries will be returned:
    #        row  col
    #  [1,]  a    b
    #  [2,]  b    a
    #       Hence, we select every other entry:
    tooClose <- tooClose[seq(1, nrow(tooClose), 2), ]
    
    tooCloseCount <- nrow(tooClose)
    
    # Format the first line of the warning message
    firstLine <- sprintf(
      "%d locations have neighbors that are < %d m away",
      round(tooCloseCount),
      radius
    )
    
    # Create a warning line for each location pair
    lines <- c(firstLine)
    for ( i in seq(nrow(tooClose)) ) {
      
      dist <- distances[tooClose[i, 1], tooClose[i, 2]]
      newLine <- sprintf(
        "Entries %s %s. Distance: %s m",
        tooClose[i, 1],
        tooClose[i, 2],
        round(dist, 2)
      )
      
      lines <- append(lines, newLine)
      
    }

    # Print the warning message
    message(paste(lines, collapse = "\n"))
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
