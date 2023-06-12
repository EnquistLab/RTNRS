#' Get TNRS metadata
#'
#' Returns metadata on TNRS including version and citation information
#' @param bibtex_file Optional output file for writing bibtex citations.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @return List containing: (1) bibtex-formatted citation information, (2) information about TNRS data sources, and (3) TNRS version information.
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
#' @note This function is a wrapper that returns the output of the functions TNRS_citations, TNRS_sources, and TNRS_version.
#' @export
#' @examples {
#'   metadata <- TNRS_metadata()
#' }
#'
TNRS_metadata <- function(bibtex_file = NULL, skip_internet_check = FALSE) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  output <- list()

  output[[1]] <- TNRS_citations(skip_internet_check = skip_internet_check)
  output[[2]] <- TNRS_sources(skip_internet_check = skip_internet_check)
  output[[3]] <- TNRS_version(skip_internet_check = skip_internet_check)

  # If the internet API connection was successful, add names to the output

  if (length(output) == 3) {
    names(output) <- c("citations", "sources", "version")

    # Write the bibtex information if a file is specified

    if (!is.null(bibtex_file)) {
      writeLines(
        text = output$citations$citation,
        con = bibtex_file
      )
    }

    return(output)
  } # end if


  # If there was an issue connecting to the API, return NULL invisible

  return(invisible(NULL))
}
