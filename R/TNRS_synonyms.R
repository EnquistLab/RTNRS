"Get synonyms for a single (plant) taxonomic name
#"
#' Get synonyms for a single species
#' @param taxonomic_name Data.frame containing a single row and two columns: 1) Row number, 2) Taxonomic name to get synonyms of. Alternatively, a character vector of names can be supplied.
#' @param source Character. Taxonomic source (1) to use. Default is "wcvp". Options include "wfo", and "wcvp".
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note This function only handles a single source and a single taxonomic name at a time. This is by design.
#' @note wfo = World Flora Online, wcvp = World Checklist of Vascular Plants.
#' @import httr
#' @export
#' @importFrom jsonlite toJSON fromJSON
#' @examples{
#'
#' TNRS_synonyms(taxonomic_name = "Palicourea elata")
#'
#' }
TNRS_synonyms <- function(taxonomic_name,
                          source = "wcvp",
                          skip_internet_check = FALSE,
                          ...) {
  # Check for internet access

  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  # If taxonomic names are supplied as a character string, make them into a data.frame

  if (inherits(taxonomic_name, "character")) {
    taxonomic_name <- data.frame(ID = 1:length(taxonomic_name), taxonomic_name)
  }

  # Make sure only one name is supplied

  if (nrow(taxonomic_name) != 1) {
    message("This function can only take one taxonomic name at a time.")
    return(invisible(NULL))
  }

  # Make sure only one source is supplied

  if (length(source) != 1) {
    message("This function requires one (and only one) source be specified.")
    return(invisible(NULL))
  }

  # Check source validity

  if (!source %in% c("wcvp", "wfo")) {
    message("Source ", source, " is not a valid option. Please choose from wcvp or wfo")
    return(invisible(NULL))
  }

  # Convert the data to JSON

  data_json <- jsonlite::toJSON(unname(taxonomic_name))

  # Send data to core function

  results <- TNRS_core(
    data_json = data_json,
    sources = source,
    classification = NA,
    mode = "syn",
    matches = NA,
    accuracy = NA,
    ...
  )

  # Return results

  return(results)
}
