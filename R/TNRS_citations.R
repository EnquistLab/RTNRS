#'Get citation information
#'
#'Returns information needed to cite the TNRS
#' @return Dataframe containing bibtex-formatted citation information
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the TNRS, as the TNRS couldn't exist without these sources!
#' @import httr
#' @importFrom jsonlite toJSON fromJSON 
#' @export
#' @examples {
#' citation_info <- TNRS_citations()
#' }
#' 
TNRS_citations <- function(){
  
  # Check for internet access
  if (!is.character(getURL("www.google.com"))) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  
  mode = "citations"
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" #dev
  url = "https://tnrsapi.xyz/tnrs_api.php" #production
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  
  # Re-form the options json again
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Make the options
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  # Send the API request
  results_json <- POST(url = url,
                       add_headers('Content-Type' = 'application/json'),
                       add_headers('Accept' = 'application/json'),
                       add_headers('charset' = 'UTF-8'),
                       body = input_json,
                       encode = "json")
  
  # Convert JSON results to a data frame
  results <- fromJSON(rawToChar(results_json$content)) 
  #results <- as.data.frame(results)
  results$citation <- gsub(pattern = "{{",replacement = "{",x = results$citation,fixed = T)
  results$citation <- gsub(pattern = "}}",replacement = "}",x = results$citation,fixed = T)
  
  return(results)
  
}#TNRS citations
