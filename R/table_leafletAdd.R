#' @title Add to a leaflet interactive map for known locations
#'
#' @param map Leaflet map.
#' @param locationTbl Tibble of known locations.
#' @param extraVars Character vector of addition \code{locationTbl} column names
#' to be shown in leaflet popups.  
#' @param ... Additional arguments passed to \code{leaflet::addCircleMarker()}.
#'
#' @description This function adds interactive maps that will be displayed in
#'   RStudio's 'Viewer' tab.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'
#' @rdname table_leafletAdd
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom leaflet leaflet setView addProviderTiles addCircleMarkers

table_leafletAdd <- function(
  map = NULL,
  locationTbl = NULL,
  extraVars = NULL,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(map)
  
  if ( !"leaflet" %in% class(map) )
    stop("'map' is not a leaflet map")
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  
  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(locationTbl))
    if ( length(unrecognizedVars) > 0 ) {
      stop("variables in 'extraVars' not found in 'locationTbl'")
    }
  }

  # Filter out missing location data
  locationTbl <-
    locationTbl %>%
    dplyr::filter(!is.na(.data$latitude)) %>%
    dplyr::filter(!is.na(.data$longitude))
  
  hasCoreMetadata <- all(coreMetadataNames %in% names(locationTbl))
  
  # * argsList -----
  
  argsList <- list(...)
  
  argsList$map <- map
  argsList$lng <- locationTbl$longitude
  argsList$lat <- locationTbl$latitude
  
  # ----- Add circle markers ---------------------------------------------------
  
  # * weight -----
  
  if ( !"weight" %in% argsList ) 
    argsList$weight <- 1
  
  # * popup text -----
  
  # Initialize empty popupText
  popupText <- vector("character", nrow(locationTbl))
  
  # Create coreText
  
  if ( hasCoreMetadata ) {
    
    # Use guaranteed fields
    coreText <- paste0(
      "<b>", locationTbl$locationName, "</b><br>",
      "locationID = ", locationTbl$locationID, "<br>",
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>",
      "timezone = ", locationTbl$timezone, "<br>",
      "ISO = ", locationTbl$countryCode, ".", locationTbl$stateCode, "<br>",
      "county = ", locationTbl$countyName, "<br>",
      "address = ", locationTbl$houseNumber, ", ", locationTbl$street, ", ", locationTbl$city, ", ", 
      locationTbl$stateCode, ", ", locationTbl$zip, "<br>"
    )
    
    
  } else {
    
    # Use reasonable best guesses to create 3 lines of core metadata
    
    tblNames <- names(locationTbl)
    
    # Bold location identifier at the top (in preference order)
    if ( "locationName" %in% tblNames ) {
      coreText_1 <-  paste0("<b>", locationTbl$locationName, "</b><br>")
    } else if ( "siteName" %in% tblNames ) {
      coreText_1 <-  paste0("<b>", locationTbl$siteName, "</b><br>")
    } else if ( "AQSID" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$AQSID, "</b><br>")
    } else if ( "aqsID" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$aqsID, "</b><br>")
    } else if ( "aqsid" %in% tblNames ) {
      coreText_1 <- paste0("<b>", locationTbl$aqsid, "</b><br>")
    } else {
      coreText_1 <- ""
    }
    
    # US EPA AQSID if not already found
    if ( "AQSID" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$AQSID, "<br>")
    } else if ( "aqsID" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$aqsID, "<br>")
    } else if ( "aqsid" %in% tblNames ) {
      coreText_2 <-  paste0("AQSID = ", locationTbl$aqsid, "<br>")
    } else {
      coreText_2 <- ""
    }
    
    # Location
    coreText_3 <- paste0(
      "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>"
    )
    
    # Paste them together
    coreText <- paste0(coreText_1, coreText_2, coreText_3)
    
  }
  
  # Add extra vars
  for ( i in seq_along(popupText) ) {
    
    extraText <- vector("character", length(extraVars))
    for ( j in seq_along(extraVars) ) {
      var <- extraVars[j]
      extraText[j] <- paste0(var, " = ", locationTbl[i, var], "<br>")
    }
    extraText <- paste0(extraText, collapse = "")
    
    popupText[i] <- paste0(coreText[i], "<hr>", extraText)
    
  }
  
  locationTbl$popupText <- popupText
  
  argsList$popup <- popupText
  
  # * add markers -----
  
  map <- do.call(leaflet::addCircleMarkers, argsList)
  
  # ----- Return ---------------------------------------------------------------
  
  return(map)
  
}
