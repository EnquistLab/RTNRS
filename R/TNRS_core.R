#'Handle API access and format
#'
#'Internal function for handling accessing the API,data formatting, and errors
#' @param url Server URL to use.  Defaults to the stable production version
#' @param mode API mode to use.  One of "collaborators",
#' @param data_json Either NULL (the default) or properly formatted json
#' @param batches NULL or Numeric.  Optional number of batches to divide the request into for parallel processing.
#' @importFrom jsonlite toJSON
#' @import httr
#' @keywords internal
TNRS_core <- function(url = "https://tnrsapi.xyz/tnrs_api.php", mode, data_json = NULL, batches = NULL){
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Set the opts
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  
  #Format the input
  if(is.null(data_json)){
    
    opts_json <- jsonlite::toJSON(opts)
    opts_json <- gsub('\\[','',opts_json)
    opts_json <- gsub('\\]','',opts_json)
    input_json <- paste0('{"opts":', opts_json, '}' )
    
    
  }else{
    
    if ( exists("batches") ){opts$batches <- batches}
    
    opts_json <-  jsonlite::toJSON(opts)
    opts_json <- gsub('\\[','',opts_json)
    opts_json <- gsub('\\]','',opts_json)
    
    # Combine the options and data into single JSON object
    input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
    
  }
  
  
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                       add_headers('Content-Type' = 'application/json'),
                                       add_headers('Accept' = 'application/json'),
                                       add_headers('charset' = 'UTF-8'),
                                       body = input_json,
                                       encode = "json"
  )
  ,
  error = function(e) {
    message("There appears to be a problem reaching the API.")
  })
  
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  #Check status, if it doesn't equal
  if(results_json$status_code != 200){
    
    message(paste("Problem with the API: HTTP Status" ,results_json$status_code))
    #fromJSON(rawToChar(results_json$content))
    
    return(invisible(NULL))
    
  }
  
  # Ensure that the results are properly formatted and return results or a message.
  tryCatch(expr = results_raw <- fromJSON(rawToChar(results_json$content)),
           error = function(e) {
             message(paste("There seems to be a problem with the query, which returned the following: \n",rawToChar(results_json$content)))
           })
  
  #Convert to data.frame if things worked
  
  if(!exists("results_raw")){
    return(invisible(NULL))
  }else{
    results <- as.data.frame(results_raw)  
  }
  
  
  return(results)
  
}
