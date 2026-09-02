#' Resolve plant taxonomic names
#'
#' Resolve plant taxonomic names.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed). Note that these two columns must be in this order. Alternatively, a character vector of names can be supplied.
#' @param sources Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", "wcvp", and "cact". Use TNRS_sources() for more information.
#' @param classification Character. Family classification to use. Currently options include "wfo" (the default).
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric. Optional score threshold between 0 and 1. Note that the web service applies this across every component score at once, and discards a match only when all of them fall below it, so a match with a low overall score survives a high threshold if any one component clears it. Filtering on Overall_score afterwards is what most callers actually want. If left NULL, the service's own default of 0.53 is used.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param name_limit Numeric. The maximum number of names to check in one batch.  The default is 5000 and is usually the fastest option.  This cannot exceed 5000.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note wfo = World Flora Online, wcvp = World Checklist of Vascular Plants, cact = Cactaceae at Caryophyllales.org.
#' @note For queries of more than 5000 names, the function will automatically divide the query into batches of 5000 names and then run the batches one after the other. Thus, for very large queries this may take some time. When this is the case, a progress bar will be displayed.
#' @note IMPORTANT: Note that parallelization of queries is automatically handled by the API, and so there is no need to further parallelize in R (in fact, doing so may actually slow things down!).
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples \dontrun{
#' # Take a subset of the testfile to speed up runtime
#' tnrs_testfile <- tnrs_testfile[1:20, ]
#'
#' results <- TNRS(taxonomic_names = tnrs_testfile)
#'
#' # Inspect the results
#' head(results, 10)
#' }
#'
TNRS <- function(taxonomic_names,
                 sources = c("wcvp", "wfo"),
                 classification = "wfo",
                 mode = "resolve",
                 matches = "best",
                 accuracy = NULL,
                 skip_internet_check = FALSE,
                 name_limit = 5000,
                 ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }

  # Everything checkable is checked before anything is sent, because the web
  # service answers a malformed request with a status code and no explanation
  submitted <- tnrs_check_names(taxonomic_names)

  # Each distinct, non-blank name is sent once under an identifier of our own.
  # Upstream drops blank names, which shifts every later identifier, and
  # combines repeated names into one row whose identifier is the others pasted
  # together; sending the request this way avoids both, and the caller's rows
  # are rebuilt from the answer afterwards.
  taxonomic_names <- tnrs_request_frame(submitted)

  if (nrow(taxonomic_names) == 0) {
    message("No names to resolve: every name supplied was blank.")
    return(tnrs_reconcile_results(NULL, submitted, taxonomic_names))
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

  # reformat sources to match API input
  sources <- paste0(sources, collapse = ",")



  # If there are less than the max number of names allowable, send them to the base package
  if (nrow(taxonomic_names) <= name_limit) {
    results <- TNRS_base(
      taxonomic_names = taxonomic_names,
      sources = sources,
      classification = classification,
      mode = mode,
      matches = matches,
      accuracy = accuracy,
      skip_internet_check = skip_internet_check,
      ...
    )
    if (is.null(results)) {
      return(invisible(NULL))
    }
    return(tnrs_reconcile_results(results, submitted, taxonomic_names))
  } #

  # If there are more than the max number of records, divide them into chunks and process the chunks



  if (nrow(taxonomic_names) > name_limit) {
    nchunks <- ceiling(nrow(taxonomic_names) / name_limit)

    # set up progress bar
    pb <- txtProgressBar(
      min = 0, # Minimum value of the progress bar
      max = nchunks, # Maximum value of the progress bar
      style = 3, # Progress bar style (also available style = 1 and style = 2)
      # width = 50,   # Progress bar width. Defaults to getOption("width")
      char = "="
    ) # Character used to create the bar


    results <- NULL

    for (i in 1:nchunks) {
      # Rows covered by this batch.  The final batch may be a partial one.

      first_row <- ((i - 1) * name_limit) + 1
      last_row <- min(i * name_limit, nrow(taxonomic_names))

      results_i <- TNRS_base(
        taxonomic_names = taxonomic_names[first_row:last_row, ],
        sources = sources,
        classification = classification,
        mode = mode,
        matches = matches,
        accuracy = accuracy,
        skip_internet_check = skip_internet_check,
        ...
      )

      # A failed batch returns NULL.  Warn rather than dropping the names
      # silently, since the remaining batches may still succeed.

      if (is.null(results_i)) {
        warning(
          "No results returned for rows ", first_row, " to ", last_row,
          ". These names are missing from the output."
        )
      } else {
        results <- rbind(results, results_i)
      }

      setTxtProgressBar(pb, i)
    } # i loop

    close(pb)
  } # if more than name_limit

  if (is.null(results)) {
    return(invisible(NULL))
  }

  return(tnrs_reconcile_results(results, submitted, taxonomic_names))
} # fx
