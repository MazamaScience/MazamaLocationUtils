#' @title Leaflet interactive map for known locations
#'
#' @param locationTbl Tibble of known locations.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#' @param extraVars Character vector of addition \code{locationTbl} column names
#' to be shown in leaflet popups.  
#' @param color Named color to use for map icons.
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
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom leaflet leaflet setView addProviderTiles addCircleMarkers

table_leaflet <- function(
  locationTbl = NULL,
  maptype = "terrain",
  extraVars = NULL,
  color = "blue"
) {

  radius <- 10
  opacity <- 0.5

  # ----- Validate parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaCoreUtils::stopIfNull(maptype)
  
  if ( !is.null(extraVars) ) {
    if ( ! extraVars %in% names(locationTbl) ) {
      stop("Variables in 'extraVars' not found in 'locationTbl'")
    }
  }

  # ----- Create popup text ----------------------------------------------------

  # Create popupText
  popupText <- paste0(
    "<b>", locationTbl$locationName, "</b><br>",
    "locationID = ", locationTbl$locationID, "<br>",
    "longitude = ", locationTbl$longitude, ", ", "latitude = ", locationTbl$latitude, "<br>",
    "timezone = ", locationTbl$timezone, "<br>",
    "ISO = ", locationTbl$countryCode, ".", locationTbl$stateCode, "<br>",
    "county = ", locationTbl$county, "<br>",
    "address = ", locationTbl$houseNumber, ", ", locationTbl$street, ", ", locationTbl$city, ", ", 
    locationTbl$stateCode, ", ", locationTbl$zip, "<br>"
  )
  
  # Add extra vars
  for ( i in seq_along(popupText) ) {

    extraText <- vector("character", length(extraVars))
    for ( j in seq_along(extraVars) ) {
      var <- extraVars[j]
      extraText[j] <- paste0("<b>", var, " = ", locationTbl[i, var], "</b><br>")
    }
    extraText <- paste0(extraText, collapse = "")

    popupText[i] <- paste0(popupText[i], extraText)
  }

  locationTbl$popupText <- popupText
  
  # ----- Leaflet options ------------------------------------------------------
  
  # Extract view information
  lonRange <- range(locationTbl$longitude, na.rm = TRUE)
  latRange <- range(locationTbl$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange),diff(latRange), na.rm = TRUE)
  # Determine appropriate zoom level
  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # Convert locations to SpatialPointsDataFrame
  locationTbl <- locationTbl[!is.na(locationTbl$latitude),]
  SPDF <- sp::SpatialPointsDataFrame(coords = cbind(locationTbl$longitude,locationTbl$latitude),
                                     data = as.data.frame(locationTbl))

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

  # ----- Create leaflet map ---------------------------------------------------
  
  m <- leaflet::leaflet(SPDF) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius = radius,
      fillColor = col,
      fillOpacity = opacity,
      stroke = FALSE,
      popup = locationTbl$popupText,
      layerId = locationTbl$locationID
    )

  # ----- Return ---------------------------------------------------------------

  return(m)

}
