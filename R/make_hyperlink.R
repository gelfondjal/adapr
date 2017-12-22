#' Makes HTML hyper link
#' @param files character vector of filenames
#' @param links description of links
#' @return link command vector
#' @details Used in making HTML files
#' @export
#' @examples 
#'
#'  makeHyperlink("myPath","click here to my path")
#'
makeHyperlink <- function(files,links){
  
  link.command <- rep("",length(files))
  
  if(length(files)==0){stop("Error makeHyperlink: no files to link")}
  
  for(file.iter in 1:length(files)){
    link.command[file.iter] <- paste0("<a href=\"file:///",files[file.iter],"\">",links[file.iter],"</a>")
    
    
    
  }
  return(link.command)
  
}

#' Makes HTML hyper link relative to a directory by clipping off directory name
#' @param directory.to.clip character of directory to remove from file path
#' @param files character vector of filenames
#' @param links description of links
#' @return link command vector
#' @details Used in making HTML files
#' @export
#' @examples 
#'
#'  makeRelativeHyperlink("myPath","myPath/pathToFile","click here to my path")
#'

makeRelativeHyperlink <- function(directory.to.clip,files,links){
  
  files <- gsub("^/","",gsub(directory.to.clip,"",files,fixed=TRUE))
  
  link.command <- rep("", length(files))
  
  if(length(files)==0){stop("makeRelativeHyperlink in projectReportSend: Error no files to link")}
  
  for (file.iter in 1:length(files)) {
    link.command[file.iter] <- paste0("<a href=\"file:", 
                                      files[file.iter], "\">", links[file.iter], "</a>")
  }
  return(link.command)
  
}
