#'Get citation information
#'
#'Returns information needed to cite the TNRS
#' @return Dataframe containing bibtex-formatted citation information
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON 
#' @export
#' @examples {
#' citation_info <- TNRS_citations()
#' }
#' 
TNRS_citations <- function(){
  mode = "citations"
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php"
  url = "https://tnrsapi.xyz/tnrs_api.php"
  
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Combine the options and data into single JSON object
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the API request
  results_json <- RCurl::postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Convert JSON file to a data frame
  results <- jsonlite::fromJSON(results_json)
  results <- results$citations
  
  results$citation <- gsub(pattern = "{{",replacement = "{",x = results$citation,fixed = T)
  results$citation <- gsub(pattern = "}}",replacement = "}",x = results$citation,fixed = T)
  
  return(results)
  
}#TNRS citations
