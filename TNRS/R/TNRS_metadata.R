#'Get TNRS metadata
#'
#'Returns metadata on TNRS including version and citation information
#' @param bibtex_file Optional output file for writing bibtex citations.
#' @return List containing: (1) bibtex-formatted citation information, (2) information about TNRS data sources, and (3) TNRS version information.
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
#' @note This function is a wrapper that returns the output of the functions TNRS_citations, TNRS_sources, and TNRS_version.
#' @export
#' @examples {
#' metadata <- TNRS_metadata()
#' }
#' 
TNRS_metadata <- function(bibtex_file=NULL){
  
  output <- list()
    
  output[[1]] <- TNRS::TNRS_citations()
  output[[2]] <-TNRS::TNRS_sources()
  output[[3]] <-TNRS::TNRS_version()
    
  names(output)<-c("citations","version","sources")

  #Write the bibtex information if a file is specified
  if(!is.null(bibtex_file)){writeLines(text = output$citations$citation, con = bibtex_file)}

  return(output)
  
}

