#'Get citation information
#'
#'Returns information needed to cite the TNRS
#' @return Dataframe containing bibtex-formatted citation information
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
#' @param ... Additional parameters passed to internal functions
#' @export
#' @examples {
#' citation_info <- TNRS_citations()
#' }
TNRS_citations <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  results <- TNRS_core(mode = "citations", ...)
  
  results$citation <- gsub(pattern = "{{",replacement = "{",x = results$citation,fixed = T)
  results$citation <- gsub(pattern = "}}",replacement = "}",x = results$citation,fixed = T)
  
  return(results)
  
} # TNRS citations
