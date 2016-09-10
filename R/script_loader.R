#' Initializes dependency object source_info
#' @param script is the filename for the R script loaded 
#' @param projectID is the project id
#' @return value 1 if success
#' @export
#' 
scriptLoader <- function(projectID,script){
  
  # initialize dependency file R object, store filename
  # project.path is for git
  
  programPath <- file.path(get.project.path(projectID),project.directory.tree$analysis,script)
  
  resultsPath <- file.path(get.project.path(projectID),project.directory.tree$results,script)
  
  setwd(resultsPath)
  
  if(!file.exists(programPath)){stop("Program file ",programPath," doesn't exist")}
  
  if(!exists("source_info")){
    
    options(Rmdstart=TRUE)
    
    source(programPath,echo=FALSE)
   
    }
  
  
  return(1)
  
}	
