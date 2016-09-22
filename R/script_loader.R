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
  
  setwd(resultsPath)
  
  return(1)
  
}	

#' Checks whether interactive R markdown session is ON
#' @param changeOption logical to print out and change the Rmdstart option
#' @return value TRUE if R session in in interactive R markdown mode
#' @export
#' 
checkRmdMode <- function(changeOption=FALSE){
  
  test <- !is.null(options()$Rmdstart)
  
  if(test){test <- options()$Rmdstart==TRUE}
  
  if(changeOption){
    options(Rmdstart = !test)
    test <- !test
    print("To change use options(Rmdstart=FALSE/TRUE)")
    }
  
  return(test)
  
  
}


