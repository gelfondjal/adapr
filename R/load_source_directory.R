#' Runs all source files within the directory source.directory
#' @param source.directory is a directory with R source files to load
#' @return source file list
#' @details Looks for files with .R or .r suffixes. 
#' @export
#' @examples 
#'\dontrun{
#'  path <- get.project.path("adaprHome")
#'  path <- file.path(path,"Programs",project.directory.tree$support)
#'  load.source.directory(path)
#'} 

load.source.directory <- function(source.directory){

  
  source.file.list <- list.files(source.directory,pattern="(\\.R$)|(\\.r$)",full.names=TRUE,include.dirs=FALSE)
  
  print(source.file.list)
  
  out <- lapply(source.file.list, source)
  
  return(out)
  
}
