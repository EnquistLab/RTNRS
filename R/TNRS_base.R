#'Resolve a single batch of (plant) taxonomic names
#'
#'Resolve a small batch of plant taxonomic names
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed).  Alternatively, a character vector of names can be supplied.  
#' @param sources Character. Taxonomic sources to use. Default is "tpl,tropicos,usda". Options include tpl,tropicos,usda,ncbi
#' @param classification Character. Family classification to use. Currently the only options is "tropicos", which is equivalent to APGIII.
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @return Dataframe containing TNRS results.
#' @note This function is primarily used as an internal function of TNRS and can only handle relatively small batches of names. 
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @keywords Internal
#' 
.TNRS_base <- function(taxonomic_names,
                       sources = "tpl,tropicos,usda",
                       classification = "tropicos",
                       mode = "resolve",
                       matches = "best"
){
  
  # URL for TNRS API
  #url = "https://tnrsapidev.xyz/tnrs_api.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:8975/tnrs_api.php" # Dev (vegbiendev)
  url = "https://tnrsapi.xyz/tnrs_api.php" #production

  
  #If taxonomic names are supplied as a character string, make them into a data.frame
  
  if(class(taxonomic_names)=="character"){
    taxonomic_names <- data.frame(ID = 1:length(taxonomic_names), taxonomic_names)
    }
  
  # Convert the data to JSON
  data_json <- jsonlite::toJSON(unname(taxonomic_names))
  
  # Convert the options to data frame and then JSON
  opts <- data.frame(c(sources),c(classification), c(mode), c(matches))
  names(opts) <- c("sources", "class", "mode", "matches")
  opts_json <-  jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  
  
  # Combine the options and data into single JSON object
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the API request
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Convert JSON results to a data frame
  results <-  jsonlite::fromJSON(results_json) #Error here
  
  
  
  return(results)
  
}
  
