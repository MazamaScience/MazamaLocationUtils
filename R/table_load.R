
#' @title Load a known location table
#' @description Load a tibble of known locations from the preferred directory.
#' @param collectionName Character identifier for this table, Default: NULL
#' @return Tibble of known locations.
#' @examples
#' \donttest{
#' # Set up standard directories and spatial data
#' mazama_initialize()
#' 
#' locationTbl <- get(data("wa_monitors_500"))
#' 
#' # Save it
#' table_save(locationTbl, "my_table")
#' 
#' # Load it
#' my_table <- table_load("my_table")
#' 
#' # Modify and save again (backup of previous will be generated)
#' my_table %>%
#'   table_addColumn("my_column") %>%
#'   table_save("my_table")
#' }
#' @details TODO
#' @seealso 
#'  \code{\link{setLocationDataDir}}
#' @rdname table_load
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
table_load <- function(
  collectionName = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collectionName)
  
  dataDir <- getLocationDataDir()
  
  # ----- Load data ------------------------------------------------------------
  
  result <- try({
    
    fileName <- paste0(collectionName, ".rda")
    filePath <- file.path(dataDir, fileName)
    locationTbl <- get(load(filePath))
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  handle errors
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
