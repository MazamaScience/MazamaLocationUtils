#' @title Leaflet interactive map for known locations
#'
#' @param locationTbl Tibble of known locations.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#' @param extraVars Character vector of addition \code{locationTbl} column names
#' to be shown in leaflet popups.  
#' @param ... Additional arguments passed to \code{leaflet::addCircleMarker()}.
#'
#' @description This function creates interactive maps that will be displayed in
#'   RStudio's 'Viewer' tab.
#'
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile"
#'   names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'
#' @rdname table_leaflet
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom rlang .data
#' @importFrom leaflet leaflet setView addProviderTiles addCircleMarkers
#' 
#' @examples
#' \dontrun{
#' library(MazamaLocationUtils)
#' 
#' # A table with all core metadata
#' table_leaflet(wa_monitors_500
#'   
#' # A table missing some core metadata
#' table_leaflet(
#'   wa_airfire_meta,
#'   extraVars = c("stateCode", "countyName", "msaName")
#' )
#' 
#' # Customizing the map
#' table_leaflet(
#'   wa_airfire_meta,
#'   extraVars = c("stateCode", "countyName", "msaName"),
#'   radius = 6,
#'   color = "black",
#'   weight = 2,
#'   fillColor = "red",
#'   fillOpacity = 0.3
#' )
#' }

table_leaflet <- function(
  locationTbl = NULL,
  maptype = "terrain",
  extraVars = NULL,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = TRUE)
  MazamaCoreUtils::stopIfNull(maptype)
  
  if ( !is.null(extraVars) ) {
    unrecognizedVars <- setdiff(extraVars, names(locationTbl))
    if ( length(unrecognizedVars) > 0 ) {
      stop("Variables in 'extraVars' not found in 'locationTbl'")
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
  
  argsList$lng <- locationTbl$longitude
  argsList$lat <- locationTbl$latitude
  
  # ----- Create base map ------------------------------------------------------
  
  # * zoom -----
  
  # Extract view information
  lonRange <- range(locationTbl$longitude, na.rm = TRUE)
  latRange <- range(locationTbl$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange),diff(latRange), na.rm = TRUE)
  
  # Determine appropriate zoom level
  if (maxRange > 250) {
    zoom <- 2
  } else if (maxRange > 150) {
    zoom <- 2
  } else if (maxRange > 100) {
    zoom <- 3
  } else if (maxRange > 50) {
    zoom <- 4
  } else if (maxRange > 20) {
    zoom <- 5
  } else if (maxRange > 10) {
    zoom <- 6
  } else if (maxRange > 5) {
    zoom <- 7
  } else if (maxRange > 2) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else if (maxRange > 0.05) {
    zoom <- 12
  } else if (maxRange > 0.02) {
    zoom <- 13
  } else {
    zoom <- 14
  }

  # * providerTiles -----
  
  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }
  
  # * base map -----
  
  argsList$map <- 
    leaflet::leaflet(locationTbl) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles)
  
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
