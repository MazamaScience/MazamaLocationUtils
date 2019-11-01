
#' @title Remove a column of metadata in a table
#' @description Remove the column matching \code{columnName}. This function 
#' can be used in pipelines.
#' @param locationTbl Tibble of known locations, Default: NULL
#' @param columnName Name of the colun to be removed, Default: NULL
#' @param verbose Logical controlling the generation of progress messages.
#' @return Updated tibble of known locations.
#' @examples
#' \donttest{
#' # Set up standard directories and spatial data
#' mazama_initialize()
#' 
#' # Set the directory for saving location tables
#' setLocationDataDir(tempdir())
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
#'   
#' # Change your mind
#' table_load("my_table") %>%
#'   table_removeColumn("my_column") %>%
#'   table_save("my_table")
#'   
#' list.files(getLocationDataDir(), pattern = "my_table")
#' }
#' @seealso \link{table_addColumn}
#' @seealso \link{table_removeColumn}
#' @rdname table_removeColumn
#' @export 
#' @importFrom MazamaCoreUtils stopIfNull
table_removeColumn <- function(
  locationTbl = NULL,
  columnName = NULL,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(columnName)
  
  if ( !columnName %in% names(locationTbl) ) 
    stop(sprintf(
      "columnName %s is not found in locationTbl", columnName
    ))
  
  if ( columnName %in% MazamaLocationUtils::coreMetadataNames )
    stop(sprintf(
      "columnName %s is part of the core metdata and cannot be removed", columnName
    ))
  
  
  # ----- Remove column --------------------------------------------------------
  
  locationTbl[[columnName]] <- NULL
  
  # ----- Return ---------------------------------------------------------------
  
  return(locationTbl)
  
}
