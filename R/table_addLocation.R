
#' @title Add new known location records to a table
#' 
#' @description Incoming \code{longitude} and \code{latitude} values are compared 
#' against the incoming \code{locationTbl} to see if the are already within
#' \code{radius} meters of an existing entry. A new record is created for
#' each location that is not already found in \code{locationTbl}.
#' 
#' @note This funciton is a vecorized version of \code{table_addSingleLocation()}.
#' 
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param longitude Vector of longitudes in decimal degrees E, Default: NULL
#' @param latitude Vector of latitudes in decimal degrees N, Default: NULL
#' @param radius Radius in meters, Default: NULL
#' @param stateDataset Name of spatial dataset to use for determining state
#' codes, Default: 'NaturalEarthAdm1'
#' @param elevationService Name of the elevation service to use for determining
#' the elevation. Default: NULL. Accepted values: "usgs".
#' @param addressService Name of the address service to use for determining
#' the street address. Default: NULL. Accepted values: "photon".
#' @param verbose Logical controlling the generation of progress messages.
#' 
#' @return Updated tibble of known locations.
#' 
#' @examples
#' \donttest{
#' library(MazamaLocationUtils)
#' 
#' # Set up standard directories and spatial data
#' spatialDataDir <- tempdir() # typically "~/Data/Spatial"
#' mazama_initialize(spatialDataDir)
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Coulee City, WA
#' lon <- -119.290904
#' lat <- 47.611942
#' 
#' locationTbl <- 
#'   locationTbl %>%
#'   table_addLocation(lon, lat, radius = 500)
#'   
#' dplyr::glimpse(locationTbl)
#' }
#' @seealso \link{table_addSingleLocation}
#' @seealso \link{table_removeRecord}
#' @seealso \link{table_updateSingleRecord}
#' @rdname table_addLocation
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
#' @importFrom dplyr bind_rows
#' 
table_addLocation <- function(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL,
  stateDataset = "NaturalEarthAdm1",
  elevationService = NULL,
  addressService = NULL,
  verbose = TRUE
) {
  
  validateMazamaSpatialUtils()
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)
  MazamaCoreUtils::stopIfNull(stateDataset)
  
  if ( length(longitude) != length(latitude) )
    stop("longitude and latitude must have the same length")
  
  if ( length(radius) != 1 )
    stop("radius must be of length 1")
  
  incomingCount <- length(longitude)
  
  # Remove location pairs if either is missing
  naMask <- is.na(longitude) | is.na(latitude)
  longitude <- longitude[!naMask]
  latitude <- latitude[!naMask]
  
  if ( verbose && any(naMask) ) {
    message(sprintf(
      "%d of the %d requested locations have NA values",
      length(which(naMask)), incomingCount
    ))
  }
  
  if ( !exists(stateDataset) ) {
    stop(paste0(
      "You must load \"stateDataset\" with: \n",
      "  loadSpatialData(\"", stateDataset, "\")\n"
    ))
  }
  
  if ( !is.null(addressService) ) {
    if ( !is.character(addressService) ) {
      stop("Currently, only the \"photon\" address service is supported.")
    } else {
      if ( tolower(addressService) != "photon" )
        stop("Currently, only the \"photon\" address service is supported.")
    }
  }
  
  if ( !is.null(elevationService) ) {
    if ( !is.character(elevationService) ) {
      stop("Currently, only the \"usgs\" address service is supported.")
    } else {
      if ( tolower(elevationService) != "usgs" )
        stop("Currently, only the \"usgs\" address service is supported.")
    }
  }
  
  # ----- Reduce to only new locations -----------------------------------------

  foundLocationID <- table_getLocationID(locationTbl, longitude, latitude, radius)
  existingCount <- sum(!is.na(foundLocationID))
  
  if ( verbose && existingCount > 0 ) {
    message(sprintf(
      "%d of the %d requested locations are already found in the locationTbl",
      existingCount, incomingCount
    ))
  }
  
  longitude <- longitude[is.na(foundLocationID)]
  latitude <- latitude[is.na(foundLocationID)]
  
  # ----- Loop over new locations ----------------------------------------------

  # NOTE:  Yes, this is a loop!
  # NOTE:  But the task of generating spatial data takes many seconds for each
  # NOTE:  iteration so there is nothing to be gained by making the code less
  # NOTE:  readable with functional progamming style.
  
  for ( i in seq_along(longitude) ) {
    
    result <- try({
      
      if ( verbose ) {
        message(sprintf(
          "Working on %.7f, %.7f ...",
          longitude[i], latitude[i]
        ))
      }
      locationTbl <- table_addSingleLocation(
        locationTbl = locationTbl,
        longitude = longitude[i],
        latitude = latitude[i],
        radius = radius,
        stateDataset = stateDataset,
        elevationService = elevationService,
        addressService = addressService,
        verbose = verbose
      )
      
    }, silent = TRUE)
    
    if ( "try-error" %in% result ) {
      # Warn but don't stop
      warning(sprintf(
        "Skipping with error: ", geterrmessage()
      ))
    }
    
    # Be _somewhat_ careful with memory
    if ( 1 %% 10 == 0 ) {
      gc()
    }
    
  }

  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
