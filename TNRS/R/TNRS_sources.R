#'Get information on sources used by the TNRS
#'
#'Return metadata about the current TNRS sources
#' @return Dataframe containing information about the sources used in the current TNRS version.
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON 
#' @export
#' @examples {
#' sources <- TNRS_sources()
#' }
#' 
TNRS_sources <- function(){
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" #dev
  #url = "https://tnrsapi.xyz/tnrs_api.php"
  
  # Set sources mode
  mode <- "sources"		
  
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
  
  # Send the request again
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Display the results
  results <- jsonlite::fromJSON(results_json)
  return(results)
  
}#TNRS sources
