#' Makes HTML hyper link
#' @param files character vector of filenames
#' @param links description of links
#' @return link command vector
#' @details Used in making HTML files
#' @export
#' @examples 
#'\dontrun{
#'  makeHyperlink("myPath","click here to my path")
#'}
makeHyperlink <- function(files,links){
  
  link.command <- rep("",length(files))
  
  if(length(files)==0){stop("Error makeHyperlink: no files to link")}
  
  for(file.iter in 1:length(files)){
    link.command[file.iter] <- paste0("<a href=\"file:///",files[file.iter],"\">",links[file.iter],"</a>")
    
    
    
  }
  return(link.command)
  
}
