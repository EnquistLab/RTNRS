#' Resolve plant taxonomic names and re-do erroneous results
#'
#' Resolve plant taxonomic names robustly.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed). Note that these two columns must be in this order. Alternatively, a character vector of names can be supplied.
#' @param sources Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", "wcvp", and "cact". Use TNRS_sources() for more information.
#' @param classification Character. Family classification to use. Currently options include "wfo" (the default).
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned. If left NULL, the default threshold will be used.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param name_limit Numeric. The maximum number of names to check in one batch.  The default is 5000 and is usually the fastest option.  This cannot exceed 5000.
#' @param attempts Numeric. The number of times to re-try any erroneous results.  Default is 10, but usually only one is needed at most.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note wfo = World Flora Online, wcvp = World Checklist of Vascular Plants, cact = Cactaceae at Caryophyllales.org.
#' @note For queries of more than 5000 names, the function will automatically divide the query into batches of 5000 names and then run the batches one after the other. Thus, for very large queries this may take some time. When this is the case, a progress bar will be displayed.
#' @note IMPORTANT: Note that parallelization of queries is automatically handled by the API, and so there is no need to further parallelize in R (in fact, doing so may actually slow things down!).
#' @export
#' @examples \dontrun{
#' # Take a subset of the testfile to speed up runtime
#' tnrs_testfile <- tnrs_testfile[1:20, ]
#'
#' results <- TNRS_robust(taxonomic_names = tnrs_testfile)
#'
#' # Inspect the results
#' head(results, 10)
#' }
#'
TNRS_robust <- function(taxonomic_names,
                        sources = c("wcvp", "wfo"),
                        classification = "wfo",
                        mode = "resolve",
                        matches = "best",
                        accuracy = NULL,
                        skip_internet_check = FALSE,
                        name_limit = 5000,
                        attempts = 10,
                        ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  # If taxonomic names are supplied as a character string, make them into a data.frame

  if (inherits(x = taxonomic_names, what = "character")) {
    taxonomic_names <- as.data.frame(cbind(1:length(taxonomic_names), taxonomic_names))
  }


  # Specify the limit of names for the TNRS

  if (name_limit > 5000) {
    message("name_limit cannot exceed 5000, fixing")
    name_limit <- 5000
  }

  # Check that accuracy makes sense

  if (!inherits(x = accuracy, what = c("NULL", "numeric"))) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }

  # Check that sources are valid
  if (!all(sources %in% c("wfo", "wcvp", "cact"))) {
    message("Invalid source(s) specified. Current options are: wfo, wcvp, cact ")
    return(invisible(NULL))
  }


  # Check that classification is valid
  if (length(classification) != 1 | !classification %in% c("wfo")) {
    message("Invalid classification specified. Current options are: wfo ")
    return(invisible(NULL))
  }

  # Check that mode is valid
  if (length(mode) != 1 | !mode %in% c("resolve", "parse")) {
    message("Invalid mode specified. Current options are: resolve, parse ")
    return(invisible(NULL))
  }

  # Check that matches is valid
  if (length(matches) != 1 | !matches %in% c("best", "all")) {
    message("Invalid mode specified. Current options are: best, all ")
    return(invisible(NULL))
  }



  # Get the first set of names

  first_stab <- TNRS(
    taxonomic_names = taxonomic_names,
    sources = sources,
    classification = classification,
    mode = mode,
    matches = matches,
    accuracy = accuracy,
    skip_internet_check = skip_internet_check,
    ...
  )

  # first_stab <- TNRS(taxonomic_names=taxonomic_names,
  #                    sources = sources,
  #                    classification = classification,
  #                    mode = mode,
  #                    matches = matches,
  #                    accuracy = accuracy,
  #                    skip_internet_check = skip_internet_check)


  # If the query failed entirely there is nothing to check or re-do

  if (is.null(first_stab)) {
    return(invisible(NULL))
  }

  # Evaluate names for erroneous assignment.  A result is suspicious when the
  # whole submitted name was returned as unmatched, yet a score was assigned.
  # Note that IDs, not rows, are tracked: with matches = "all" a single name
  # yields several rows, and all of them must be re-done together.

  suspicious_ids <- function(x) {
    unique(x$ID[which(x$Name_submitted == x$Unmatched_terms &
      !is.na(x$Overall_score))])
  }

  bad_ids <- suspicious_ids(first_stab)

  if (length(bad_ids) == 0) {
    return(first_stab)
  }

  message("Detected ", length(bad_ids), " suspicious results. Re-doing.")

  # Set aside correctly assigned names, and hold the suspicious ones for retry

  results <- first_stab[!first_stab$ID %in% bad_ids, ]
  pending <- first_stab[first_stab$ID %in% bad_ids, ]

  # Try to re-do erroneous names

  attempt <- 0

  while (attempt < attempts && length(bad_ids) > 0) {
    revised_output <- TNRS(
      taxonomic_names = unique(pending[, c(1, 2)]),
      sources = sources,
      classification = classification,
      mode = mode,
      matches = matches,
      accuracy = accuracy,
      skip_internet_check = skip_internet_check,
      ...
    )

    # Keep the previous attempt's results rather than losing the names if the
    # API becomes unavailable partway through

    if (is.null(revised_output)) {
      break
    }

    bad_ids <- suspicious_ids(revised_output)

    results <- rbind(results, revised_output[!revised_output$ID %in% bad_ids, ])
    pending <- revised_output[revised_output$ID %in% bad_ids, ]

    attempt <- attempt + 1
  }

  # Retain any names that were still suspicious when the attempts ran out, so
  # that nothing is dropped from the output

  results <- rbind(results, pending)

  # Restore the order in which the names were submitted

  results <- results[order(match(results$ID, first_stab$ID)), ]
  rownames(results) <- NULL

  return(results)
}
