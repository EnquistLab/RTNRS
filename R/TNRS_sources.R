#'Get information on sources used by the TNRS
#'
#'Return metadata about the current TNRS sources
#' @return Dataframe containing information about the sources used in the current TNRS version.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON 
#' @export
#' @examples {
#' sources <- TNRS_sources()
#' }
#' 
TNRS_sources <- function(){
  
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }  
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" #dev
  url = "https://tnrsapi.xyz/tnrs_api.php" #production
  
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
  
  # Send the API request
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                       add_headers('Content-Type' = 'application/json'),
                                       add_headers('Accept' = 'application/json'),
                                       add_headers('charset' = 'UTF-8'),
                                       body = input_json,
                                       encode = "json"),
           error = function(e) {
             message("There appears to be a problem reaching the API.") 
             return(NULL)
           })
  
  #Return NULL if API isn't working
  if(is.null(results_json)){return(invisible(NULL))}
  
  
  # Convert JSON results to a data frame
  results <- fromJSON(rawToChar(results_json$content)) 
  #results <- as.data.frame(results)
  
  
  return(results)
  
}#TNRS sources
