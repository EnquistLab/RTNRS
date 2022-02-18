#'Get metadata on current TNRS version
#'
#'Return metadata about the current TNRS version
#' @return Dataframe containing current TNRS version number, build date, and code version.
#' @param ... Additional parameters passed to internal functions
#' @export
#' @examples {
#' TNRS_version_metadata <- TNRS_version()
#' }
#' 
TNRS_version <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

  results <- TNRS_core(mode = "citations", ...)
  
  return(results)
  
}#TNRS version
