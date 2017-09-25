#' Location of options file and project listing. Swap / for \\ in path expand. 
#' @param x file path, could be relative or ~
#' @return Full filepath to x
#' @details See path.expand() in base R. Can be controlled in R profile by adding adaprHomeDir option. See adaprHomeDir().
#' @export
#' @examples 
#'\dontrun{
#'  path.expand.2("~")
#'} 
path.expand.2 <- function(x){
  
  # Swap / for \\ in path expand
  
  temp <- path.expand(x)
  
  temp <- gsub("/~$","",temp)
  
  out <- gsub("\\\\","/",temp)
  
  if(length(adaprHomeDir())!=0){
    
    out <- adaprHomeDir()
    
  }
  
  return(out)
  
}
#' Identify adapr Home directory. Location of options file and project listing.
#' @return Full filepath to Adapr options and project listings directory.
#' @details This is automatically handled by defaultAdaprSetup(), but can be controlled in an "R profile" by adding adaprHomeDir R option ( e.g.,  options(adaprHomeDir="myPath")).
#' @export
#' @examples 
#'\dontrun{
#'  adaprHomeDir()
#'} 
#'
adaprHomeDir <- function(){
  
  return(options()$adaprHomeDir)
  
}
