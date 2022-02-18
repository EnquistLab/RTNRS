'Resolve a single batch of (plant) taxonomic names
#'
#'Resolve a small batch of plant taxonomic names
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed).  Alternatively, a character vector of names can be supplied.  
#' @param sources Character. Taxonomic sources to use. Default is c("tropicos","wcvp"). Options include "tropicos", "usda", "wfo", and "wcvp".
#' @param classification Character. Family classification to use. Currently options include "tropicos" (the default), which is equivalent to APGIII, and "wfo".
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned.
#' @param ... Addiitnal parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note This function is primarily used as an internal function of TNRS and can only handle relatively small batches of names. 
#' @note usda = United States Department of Agriculture, wfo = World Flora Online, wcvp = World Checklist of Vascular Plants.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @keywords Internal
#' 
TNRS_base <- function(taxonomic_names,
                       sources = c("tropicos","wcvp"),
                       classification = "tropicos",
                       mode = "resolve",
                       matches = "best",
                       accuracy = NULL,
                       ...
                       ){
  

  # Check for internet access
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  

  # If taxonomic names are supplied as a character string, make them into a data.frame
    
    if(class(taxonomic_names)=="character"){
      taxonomic_names <- data.frame(ID = 1:length(taxonomic_names), taxonomic_names)
    }

  # Check that accuracy makes sense

    if(!class(accuracy) %in% c("NULL", "numeric")) {
      stop("accuracy should be either numeric between 0 and 1, or NULL")
    }

  # Convert the data to JSON
    data_json <- jsonlite::toJSON(unname(taxonomic_names))

  # Send data to core function
    results <- TNRS_core(data_json = data_json,
                         sources = sources,
                         classification = classification,
                         mode = mode,
                         matches = matches,
                         accuracy = accuracy,
                         ...)

  # Return results
    return(results)

}

