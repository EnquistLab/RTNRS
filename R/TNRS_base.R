"Resolve a single batch of (plant) taxonomic names
#"
#' Resolve a small batch of plant taxonomic names
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed).  Alternatively, a character vector of names can be supplied.
#' @param sources Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", and "wcvp".
#' @param classification Character. Family classification to use. Currently options include "wfo" (the default).
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note This function is primarily used as an internal function of TNRS and can only handle relatively small batches of names.
#' @note usda = United States Department of Agriculture, wfo = World Flora Online, wcvp = World Checklist of Vascular Plants.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @keywords Internal
#'
TNRS_base <- function(taxonomic_names,
                      sources = c("wcvp", "wfo"),
                      classification = "wfo",
                      mode = "resolve",
                      matches = "best",
                      accuracy = NULL,
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

  if (inherits(taxonomic_names, "character")) {
    taxonomic_names <- data.frame(ID = 1:length(taxonomic_names), taxonomic_names)
  }

  # Strip and "|" and issue warning

  if (any(grepl(
    pattern = "|",
    x = taxonomic_names[, 2],
    fixed = TRUE
  ))) {
    warning("A pipe was found in the supplied names. Removing it.")

    taxonomic_names[, 2] <- gsub(
      pattern = "|",
      replacement = "",
      x = taxonomic_names[, 2],
      fixed = TRUE
    )
  }



  # Check that accuracy makes sense

  if (!inherits(x = accuracy, what = c("NULL", "numeric"))) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }

  # Convert the data to JSON
  data_json <- jsonlite::toJSON(unname(taxonomic_names))

  # Send data to core function
  results <- TNRS_core(
    data_json = data_json,
    sources = sources,
    classification = classification,
    mode = mode,
    matches = matches,
    accuracy = accuracy,
    ...
  )

  # reformat score columns to numeric

  score_cols <- colnames(results)[grep(pattern = "_score$", x = colnames(results))]

  results[, score_cols] <- sapply(results[, score_cols], as.numeric)

  # Return results
  return(results)
}
