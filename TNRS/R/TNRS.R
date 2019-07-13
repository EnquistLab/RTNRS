#'Resolve (plant) taxonomic names
#'
#'TNRS taxonomically resolved plant taxonomic names.
#' @param taxonomic_names 
#' @param sources Taxonomic sources to use, as a character string. Default is "tpl,gcc,ildis,tropicos,usda"
#' @param classification Family classification to use. Default is "tropicos".
#' @param mode Character.  Options include "resolve" and "parse". Default option is "resolve"
#' @return Dataframe containing TNRS results.
#' @import RCurl jsonlite
#' @export
#' @examples \dontrun{
#' fulldata <- 
#' read.csv("http://bien.nceas.ucsb.edu/bien/wp-content/uploads/2019/07/tnrs_testfile.csv", header=FALSE)
#'fulldata$V2 <-gsub(pattern = "'",replacement = "",x = fulldata$V2) #Currently we have an issue with cultivars, so this is a workaround
#'
#'results <- TNRS(taxonomic_names = fulldata)
#'   
#' # Inspect the results
#' head(results, 10)
#' }
#' 
TNRS <- function(taxonomic_names,
                 sources = "tpl,gcc,ildis,tropicos,usda",
                 classification = "tropicos",
                 mode = "resolve"
                 ){
  
  # URL for GNRS API
  url = "https://tnrsapidev.xyz/tnrs_api.php"
  
  # Convert the data to JSON
  data_json <- jsonlite::toJSON(unname(taxonomic_names))
  
  # Convert the options to data frame and then JSON
  opts <- data.frame(c(sources),c(classification), c(mode))
  names(opts) <- c("sources", "class", "mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Combine the options and data into single JSON object
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the API request
  results_json <- RCurl::postForm(url, .opts=list(postfields= input_json, httpheader=headers))

  #clean json results to prevent encoding issues
  

  # Convert JSON file to a data frame
  results <- jsonlite::fromJSON(results_json)

  #Clean up results

  results<-gsub(pattern = '"',replacement = "",x = results) #remove quotation marks used by the API
  results<-as.data.frame(results,stringsAsFactors = F) #convert to data.frame
  colnames(results) <- as.character(results[1,]) #add column names
  results <- results[-1,] #remove first row (which contained column names)
  rownames(results) <- NULL	# Reset row numbers
  
  return(results)
  
}
  
