
#' @title Get census block data from the FCC API
#' @description The FCC Block API is used get census block, county, and state FIPS associated with
#' the \code{longitude} and \code{latitude}. The following list of data
#' is returned:
#' \itemize{
#' \item{\code{stateCode}}
#' \item{\code{county}}
#' \item{\code{censusBlock}
#' }
#' The data from this function should be considered to be the gold standard for state and county.
#' i.e. this information could and should be used to override information we get elsewhere.
#' @param longitude Single longitude in decimal degrees E, Default: NULL
#' @param latitude Single latitude in decimal degrees N, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return List of address components.
#' @examples 
#' \donttest{
#' lon <- -77.51
#' lat <- 38.26
#' 
#' censusList <- location_getCensusBlock(lon, lat)
#' str(censusList)
#' }
#' @references \url{https://geo.fcc.gov/api/census/#!/block/get_block_find}
#' @rdname location_getCensusBlock
#' @export 
#' @importFrom utils capture.output
#' @import httr
#' @importFrom stringr str_detect str_sub str_subset
#' 
location_getCensusBlock <- function(
  longitude = NULL,
  latitude = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  validateLonLat(longitude, latitude)  
  
  # ----- Get FCC API data ----------------------------------------------
  
  data <- GET("https://geo.fcc.gov/api/census/block/find",
              query = list(latitude=latitude, longitude=longitude, showall=FALSE,
                           format="json"))
  
  json <- content(data, "parsed")
  
  # ----- Create censusList --------------------------------------------
  
  censusList <- list(
    stateCode = json$State$code,
    county = json$County$name,
    censusBlock = json$Block$FIPS
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(censusList)
  
}