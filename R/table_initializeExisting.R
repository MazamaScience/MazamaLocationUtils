
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
  loctionTbl <- dplyr::select(locationTbl, all_of(allColumns))
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
