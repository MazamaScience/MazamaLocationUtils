
#' @title Get an address from a Texas A&M web service
#' @description Texas A&M APIs are used to determine the address associated with
#' the \code{longitude} and \code{latitude}.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @param apiKey Texas A&M Geocoding requires an API key. The first 2500 requests
#' are free. Default: NULL
#' @return Numeric elevation value.
#' @examples 
#' \donttest{
#' # Wenatchee
#' lon <- -120.325278
#' lat <- 47.423333
#' apiKey <- apiKey
#' location_getSingleAddress_TexasAM(lon, lat, apiKey)
#' }
#' @references \url{https://geoservices.tamu.edu/Services/ReverseGeocoding/WebService/v04_01/HTTP.aspx}
#' @rdname location_getSingleAddress_TexasAM
#' @export 
#' 
location_getSingleAddress_TexasAM <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE,
  apiKey = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get USGS elevation data ----------------------------------------------
  
  # Create url
  url <- httr::parse_url("https://geoservices.tamu.edu/Services/ReverseGeocoding/WebService/v04_01/HTTP/default.aspx")
  
  url$query <- list(
    apikey = apiKey,
    version = "4.10",
    lat = latitude,
    lon = longitude,
    format = "json"
  )
  
  
  result <- try({
  # Get and parse the return
  r <- httr::GET(httr::build_url(url))
  })
  
  if ( "try-error" %in% result )
    stop(geterrmessage())
  
  if ( httr::http_error(r) ) {
    
    addressList <- list(
      houseNumber = as.numeric(NA),
      street = as.numeric(NA),
      city = as.numeric(NA),
      stateName = as.numeric(NA),
      zip = as.numeric(NA),
      countryName = as.numeric(NA)
    ) 
    
    if ( verbose ) {
      warning(sprintf(
        "Texas A&M reverse geocoding service failed for URL %s", 
        httr::build_url(url)
      ))
    }
    
  } else {
    
    returnObj <- httr::content(r) %>%
      jsonlite::fromJSON(returnObj)
    
    locationInfo <- returnObj$StreetAddresses[[1]] 
    address <- locationInfo$StreetAddress
    
    houseNumber <- as.numeric(stringr::str_extract(address, "(\\d)+"))
    
    street <- 
      address %>%
      stringr::str_extract_all(stringr::regex("[a-z]+", ignore_case = TRUE), simplify = TRUE) %>%
      stringr::str_c(collapse = " ")
    
    if (locationInfo$StreetAddress == "") {
      street <- as.character(NA)
      houseNumber <- as.numeric(NA)
    }
    
    if (locationInfo$City == "") {
      locationInfo$City <- as.character(NA)
    }
    
    addressList <- list(
      houseNumber = houseNumber,
      street = street,
      city = locationInfo$City,
      stateName = locationInfo$State,
      zip = locationInfo$Zip,
      countryName = "US" # Texas A&M ONLY works in US
    )
  }
  
  return(addressList)
  
}