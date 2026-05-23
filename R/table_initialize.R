
#' @title Create an empty known location table
#' 
#' @description Creates an empty known location tibble with the following 
#' columns of core metadata:
#' 
#' \itemize{
#' \item{locationID}
#' \item{locationName}
#' \item{longitude}
#' \item{latitude}
#' \item{elevation}
#' \item{countryCode}
#' \item{stateCode}
#' \item{countyName}
#' \item{timezone}
#' \item{houseNumber}
#' \item{street}
#' \item{city}
#' \item{postalCode}
#' }
#' 
#' @return Empty known location tibble with the specified metadata columns.
#' 
#' @examples 
#' library(MazamaLocationUtils)
#' 
#' # Create an empty Tbl
#' emptyTbl <- table_initialize()
#' dplyr::glimpse(emptyTbl)
#' 
#' @rdname table_initialize
#' @export
#' @importFrom dplyr tibble
table_initialize <- function() {

  # ----- Create empty tibble --------------------------------------------------

  locationTbl <- dplyr::tibble(
    "locationID"   = character(0),
    "locationName" = character(0),
    "longitude"    = numeric(0),
    "latitude"     = numeric(0),
    "elevation"    = numeric(0),
    "countryCode"  = character(0),
    "stateCode"    = character(0),
    "countyName"   = character(0),
    "timezone"     = character(0),
    "houseNumber"  = character(0),
    "street"       = character(0),
    "city"         = character(0),
    "postalCode"   = character(0)
  )

  # ----- Return ---------------------------------------------------------------

  return(locationTbl)

}
