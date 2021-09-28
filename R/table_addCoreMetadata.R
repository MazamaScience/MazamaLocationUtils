
#' @title Addes missing metadata columns a known location table
#' 
#' @description An existing table will be amended to guarantee that it includes
#' the following core metadata columns.
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
#' The \code{longitude} and \code{latitude} columns are required to exist in the
#' incoming tibble but all others are optional.
#' 
#' If any of these core metadata columns are found, they will be retained.
#' 
#' The \code{locationID} will be generated (anew if already found) from existing
#' longitude and latitude data.
#' 
#' Other core metadata columns will be filled with \code{NA} values of the 
#' proper type.
#' 
#' The result is a table with all of the core metadata columns. Theses columns
#' must then be filled in to create a usable "known locations" table.
#' 
#' @note No check is performed for overlapping locations. The returned tibble
#' has the structure of a "known locations" table and is a good starting place
#' investigation. But further work is required to produce a valid table of
#' "known locations" associated with a specific radius.
#' 
#' @param tbl Table of spatial locations that will be converted into a "known
#' location" table.
#' 
#' @return Tibble with the metadata columns required in a "known locations" table.
#' 
#' @rdname table_addCoreMetadata
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr tibble filter all_of
#' @importFrom rlang .data
#' 
table_addCoreMetadata <- function(
  tbl = NULL
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)

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
    locationTbl$elevation <- as.numeric(NA)
  }
  
  # * countryCode -----
  
  if ( !"countryCode" %in% tblColumns ) {
    locationTbl$countryCode <- as.character(NA)
  }
  
  # * stateCode -----
  
  if ( !"stateCode" %in% tblColumns ) {
    locationTbl$stateCode <- as.character(NA)
  }
  
  # * locationName -----
  
  if ( !"locationName" %in% tblColumns ) {
    locationTbl$locationName <- as.character(NA)
  }
  
  # * county -----
  
  if ( !"county" %in% tblColumns ) {
    locationTbl$county <- as.character(NA)
  }
  
  # * timezone -----
  
  if ( !"timezone" %in% tblColumns ) {
    locationTbl$timezone <- as.character(NA)
  }
  
  # * houseNumber -----
  
  if ( !"houseNumber" %in% tblColumns ) {
    locationTbl$houseNumber <- as.character(NA)
  }
  
  # * street -----
  
  if ( !"street" %in% tblColumns ) {
    locationTbl$street <- as.character(NA)
  }
  
  # * city -----
  
  if ( !"city" %in% tblColumns ) {
    locationTbl$city <- as.character(NA)
  }
  
  # * zip -----
  
  if ( !"zip" %in% tblColumns ) {
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
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
