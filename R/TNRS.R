#'Resolve plant taxonomic names
#'
#'Resolve plant taxonomic names.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed).  Alternatively, a character vector of names can be supplied.  
#' @param sources Character vector. Taxonomic sources to use. Default is c("tropicos","wcvp"). Options include "tropicos", "usda", "wfo", and "wcvp".
#' @param classification Character. Family classification to use. Currently options include "tropicos" (the default), which is equivalent to APGIII, and "wfo".
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned. If left NULL, the default threshold will be used.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note usda = United States Department of Agriculture, wfo = World Flora Online, wcvp = World Checklist of Vascular Plants.
#' @note For queries of more than 1000 names, the function will automatically divide the query into batches of 1000 names and then run the batches one after the other.  Thus, for very large queries this may take some time.
#' @export
#' @examples \dontrun{
#' #Take a subset of the testfile to speed up runtime                  
#' tnrs_testfile <- tnrs_testfile[1:20,]
#'  
#' results <- TNRS(taxonomic_names = tnrs_testfile)
#'   
#' # Inspect the results
#' head(results, 10)
#' }
#' 
TNRS <- function(taxonomic_names,
                 sources = c("tropicos","wcvp"),
                 classification = "tropicos",
                 mode = "resolve",
                 matches = "best",
                 accuracy = NULL,
                 ...
                 ){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }  
  
  #If taxonomic names are supplied as a character string, make them into a data.frame
  
  if(class(taxonomic_names) == "character"){
    taxonomic_names <- as.data.frame(cbind(1:length(taxonomic_names),taxonomic_names))
  }
  
  
  #Specify the limit of names for the TNRS
  name_limit <- 5000
  
  #Check that accuracy makes sense
  
  if(!class(accuracy) %in% c("NULL", "numeric")) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }
  
  #Check that sources are valid
  if(!all(sources %in% c("tropicos", "usda", "wfo","wcvp"))) {
    message("Invalid source(s) specified. Current options are: tropicos, usda, wfo, wcvp ")
    return(invisible(NULL))
  }
  
  
  #Check that classification is valid
  if(length(classification) != 1 | !classification %in% c("tropicos","wfo")    ) {
    message("Invalid classification specified. Current options are: tropicos, wfo ")
    return(invisible(NULL))
  }
  
  #Check that mode is valid
  if(length(mode) != 1 | !mode %in% c("resolve", "parse")    ) {
    message("Invalid mode specified. Current options are: resolve, parse ")
    return(invisible(NULL))
  }
  
  #Check that matches is valid
  if(length(matches) != 1 | !matches %in% c("best", "all")    ) {
    message("Invalid mode specified. Current options are: best, all ")
    return(invisible(NULL))
  }
  
  #reformat sources to match API input
  sources <- paste0(sources, collapse = ",")
  
  
  
  # If there are less than the max number of names allowable, send them to the base package 
        if(nrow(taxonomic_names) <= name_limit){
          
          return(TNRS_base(taxonomic_names = taxonomic_names, 
                            sources = sources,
                            classification = classification,
                            mode = mode,
                            matches = matches,
                            accuracy = accuracy ))
          
        }#
  
  # If there are more than the max number of records, divide them into chunks and process the chunks 
  
  
  
  if(nrow(taxonomic_names)>name_limit){
  
    nchunks <- ceiling(nrow(taxonomic_names)/name_limit)  
    
    
    for(i in 1:nchunks){
    
      #Use the first batch of results to set up the output file
      if(i==1){
            results <- TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(i*name_limit),],
                           sources = sources,
                           classification = classification,
                           mode = mode,
                           matches = matches,
                           accuracy = accuracy)
                
            #results<-matrix(nrow = nrow(taxonomic_names),ncol = ncol(results_i))
            #$results <- as.data.frame(results,stringsAsFactors = F)
            #colnames(results)<-colnames(results_i)
            #results[(((i-1)*name_limit)+1):(i*name_limit),]<-results_i
            #rm(results_i)
        
      }#for first batch
      
      
      #For last batch
      if(i==nchunks){
        
        
        
        results <- rbind(results,
                         TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(nrow(taxonomic_names)),],
                                                                     sources = sources,
                                                                     classification = classification,
                                                                     mode = mode,
                                                                     matches = matches,
                                                                     accuracy = accuracy)
                        )
        
        
          
        
        
      }#last batch
      
      
      #middle bits
      if(i != nchunks & i != 1){
        results <- rbind(results,
                TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(i*name_limit),],
                                                                       sources = sources,
                                                                       classification = classification,
                                                                       mode = mode,
                                                                       matches = matches,
                                                                       accuracy = accuracy)
          )
          
      
      
      
      
      }#middle bits  
      
    }#i loop
    
    
    
    
    
  }#if more than 10k
    
  
  
  return(results)
  
}#fx
