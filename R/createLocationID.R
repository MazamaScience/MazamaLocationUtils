
createLocationID <- function(
  longitude = NULL,
  latitude = NULL,
  countryCode = NULL,
  stateCode = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(countryCode)
  MazamaCoreUtils::stopIfNull(stateCode)
  
  # countryCode
  countryCode <- toupper(countryCode)
  if ( !countryCode %in% countrycode::codelist[,"iso2c"] ) {
    stop(sprintf(
      "countryCode \"%s\" is not recognized. You must ISO 3166-1 alpha-2.",
      countryCode
    ))
  }
  
  # TODO:  We need to create an internal list of acceptable stateCodes
  
  # stateCode
  stateCode <- toupper(stateCode)
  if ( stringr::str_length(stateCode) > 2 ) {
    stop(sprintf(
      "stateCode \"%s\" is not recognized. You must ISO 3166-2 alpha-2.",
      stateCode
    ))
  }
  
  # ----- Create location hash -------------------------------------------------
  
  # meters per decimal degree:
  #  https://en.wikipedia.org/wiki/Decimal_degrees
  
  # Retain accuracy up to ~.1m
  locationString <- paste0(
    sprintf("%.7f", longitude),
    "_",
    sprintf("%.7f", latitude)
  )
  
  # Explanation of collision frequency:
  #   https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/
  #
  # > a hash function with range of size N can hash on the order of √N values 
  # > before running into collisions.
  #
  # A 32 bit hash will run into collisions at (2^32)^0.5 = 65,536
  # A 64 bit hash will run into collisions at (2^64)^0.5 = 4,294,967,296
  #
  # One can imagine a table with 60K known locations so it looks like a 32 bit 
  # hash is not quite safe enough.

  hashString <- digest::digest(locationString, algo = "xxhash64")
  
  # ----- Assemble locationID --------------------------------------------------
  
  # Here is our internal standard
  locationID <- paste0(
    tolower(countryCode), ".",
    tolower(stateCode), "_",
    hashString
  )

  # ----- Return ---------------------------------------------------------------
  
  return(locationID)
  
}