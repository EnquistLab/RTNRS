#' Get information on sources used by the TNRS
#'
#' Return metadata about the current TNRS sources
#' @return Dataframe containing information about the sources used in the current TNRS version.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param ... Additional parameters passed to internal functions
#' @export
#' @examples {
#'   sources <- TNRS_sources()
#' }
#'
TNRS_sources <- function(skip_internet_check = FALSE, ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  results <- TNRS_core(mode = "sources", ...)
  return(results)
} # TNRS sources
