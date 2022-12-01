#' Get metadata on current TNRS version
#'
#' Return metadata about the current TNRS version
#' @return Dataframe containing current TNRS version number, build date, and code version.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param ... Additional parameters passed to internal functions
#' @export
#' @examples {
#'   TNRS_version_metadata <- TNRS_version()
#' }
#'
TNRS_version <- function(skip_internet_check = FALSE, ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  results <- TNRS_core(mode = "meta", ...)

  return(results)
} # TNRS version
