#'Get metadata on current TNRS version
#'
#'Return metadata about the current TNRS version
#' @return Dataframe containing current TNRS version number, build date, and code version.
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples{
#' TNRS_version_metadata <- TNRS_version()
#' }
#' 
TNRS_version <- function(){
  mode = "meta"
  
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
  results <- results$meta
  
  return(results)
  
}#TNRS version
