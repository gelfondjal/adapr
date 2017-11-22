#' Saves R object to archivist repository in results directory of adapr Script
#' @param Robj R object to be written
#' @param description character description. Need for access with aRead
#' @return Value of object description
#' @details For use within R adapr script. Makes tags in archivist 'source:rscript' and 'aName:description'. Should be paired with arcRead().
#' @export
#' @examples 
#'\dontrun{
#' processed <- arcWrite(rnorm(100),"100 Gaussians")
#'} 
#'
#'
arcWrite <- function(Robj,description){
  
  suppressWarnings(hashFileName <-archivist::saveToLocalRepo(Robj,file.path(resultsDir(),"archivist"),userTags = c(paste0("source:",getSourceInfo()$file$file
                                                                                                                          ,paste0("aName:",description)))))
  fileName <- file.path("archivist","gallery",paste0(hashFileName,".rda"))
  
  file.exists(fileName)
  
  WriteTrack(fileName,description = paste("archivist",description))
  
}

#' Loads R object from archivist repository within the results directory of another adapr Script
#' @param rscript name of R script that loaded the function
#' @param description character description. Need for access with arcRead
#' @param project.id project to search within
#' @details For use within R adapr script. This complements the use of arcWrite. The description should match the description in arcWrite.
#' @export
#' @examples 
#'\dontrun{
#'# In Script that writes the archivist object:
#'# arcWrite(myplot,"xyplot")
#'
#'# In Script that reads the object:
#' processed <- arcRead("read_data.R","xyplot")
#'} 
#'
#'
arcRead <- function(rscript="read_data.R",description="xyplot",project.id = getProject()){
  
  suppressWarnings(hashFileName <- archivist::searchInLocalRepo(paste0("source:",rscript,
                                                            paste0("aName:",description)),
                                                     repo=file.path(getProjectPath(project.id),"Results",rscript,"archivist")))
  
  objectOut <- archivist::loadFromLocalRepo(hashFileName,repo=file.path(getProjectPath(),"Results",rscript,"archivist"),value=FALSE)
  
  fileName <- file.path(getProjectPath(project.id),"Results",rscript,"archivist","gallery",paste0(hashFileName,".rda"))
  
  file.exists(fileName)
  
  df.update <- data.frame(target.file=basename(fileName),
                          target.path=dirname(fileName),
                          target.description=description,
                          dependency="in",stringsAsFactors=FALSE)
  
  options()$adaprScriptInfo$dependency$update(df.update)
  
  return(objectOut)
  
}
  
  
