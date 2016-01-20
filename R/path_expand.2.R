#' Swap / for \\ in path expand
#' @param x file path, could be relative or ~
#' @return Full filepath to x
#' @details See path.expand() in base R
#' @export
path.expand.2 <- function(x){
  
  # Swap / for \\ in path expand
  
  temp <- path.expand(x)
  
  temp <- gsub("/~$","",temp)
  
  out <- gsub("\\\\","/",temp)
  
  return(out)
  
}