#'Check whether the internet is on
#'
#'Check for internet
#' @return TRUE if internet connection is detected, FALSE otherwise.
#' @importFrom httr GET
#' @keywords internal
check_internet <- function(){
  
  tryCatch(class(httr::GET("http://www.google.com/")) == "response",
           error = function(e) {
             return(FALSE)
           })
  
}

