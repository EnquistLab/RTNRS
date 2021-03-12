#'Resolve plant taxonomic names
#'
#'Resolve plant taxonomic names.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed).  Alternatively, a character vector of names can be supplied.  
#' @param sources Character. Taxonomic sources to use. Default is "tpl,tropicos,usda". Options include tpl,tropicos,usda,ncbi
#' @param classification Character. Family classification to use. Currently the only options is "tropicos", which is equivalent to APGIII.
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @return Dataframe containing TNRS results.
#' @export
#' @examples {
#' tnrs_testfile <- 
#' read.csv(system.file("extdata", "tnrs_testfile.csv", package = "TNRS",
#'                       mustWork = TRUE),
#'          stringsAsFactors = FALSE)
#' results <- TNRS(taxonomic_names = tnrs_testfile)
#'   
#' # Inspect the results
#' head(results, 10)
#' }
#' 
TNRS <- function(taxonomic_names,
                      sources = "tpl,tropicos,usda",
                      classification = "tropicos",
                      mode = "resolve",
                      matches = "best"
){
  
  #If taxonomic names are supplied as a character string, make them into a data.frame
  
  if(class(taxonomic_names)=="character"){
    taxonomic_names <- as.data.frame(cbind(1:length(taxonomic_names),taxonomic_names))
  }
  
  
  #Specify the limit of names for the TNRS
  name_limit=1000
  
  
  # If there are less than the max number of names allowable, send them to the base package 
        if(nrow(taxonomic_names)<=name_limit){
          
          return(.TNRS_base(taxonomic_names = taxonomic_names, sources = sources, classification = classification, mode = mode, matches = matches ))
          
        }#
  
  # If there are more than the max number of records, divide them into chunks and process the chunks 
  
  
  
  if(nrow(taxonomic_names)>name_limit){
  
    nchunks <- ceiling(nrow(taxonomic_names)/name_limit)  
    
    
    for(i in 1:nchunks){
    
      #Use the first batch of results to set up the output file
      if(i==1){
            results <- .TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(i*name_limit),],
                           sources = sources,
                           classification = classification,
                           mode = mode,
                           matches=matches)
                
            #results<-matrix(nrow = nrow(taxonomic_names),ncol = ncol(results_i))
            #$results <- as.data.frame(results,stringsAsFactors = F)
            #colnames(results)<-colnames(results_i)
            #results[(((i-1)*name_limit)+1):(i*name_limit),]<-results_i
            #rm(results_i)
        
      }#for first batch
      
      
      #For last batch
      if(i==nchunks){
        
        
        
        results <- rbind(results,
                         .TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(nrow(taxonomic_names)),],
                                                                     sources = sources,
                                                                     classification = classification,
                                                                     mode = mode,
                                                                     matches = matches)
                        )
        
        
          
        
        
      }#last batch
      
      
      #middle bits
      if(i != nchunks & i != 1){
        results <- rbind(results,
                .TNRS_base(taxonomic_names = taxonomic_names[(((i-1)*name_limit)+1):(i*name_limit),],
                                                                       sources = sources,
                                                                       classification = classification,
                                                                       mode = mode,
                                                                       matches = matches)
          )
          
      
      
      
      
      }#middle bits  
      
    }#i loop
    
    
    
    
    
  }#if more than 10k
    
  
  
  return(results)
  
}#fx
