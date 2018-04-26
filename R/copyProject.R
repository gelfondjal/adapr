#' Copies project to a different directory
#' @param project.id character string name of project
#' @param targetDirectory character string specifying which directory to copy to
#' @param dataCopy logicial specifying whether to copy data
#' @param programsOnly logical indicating to copy only program folder
#' @param speedCopy logical indicating to not copy if file name, mod time, and file size are identical in target directory.
#' @return data frame
#' @details Defaults to copy to the publish directory
#' @export
#' 
#' 
copyProject <- function(project.id = getProject(),targetDirectory=getProjectPublishPath(project.id),dataCopy=TRUE,programsOnly = FALSE,speedCopy=TRUE){
  #
  # Description:  Copies all project files to publish directory
  #
  
  startDirectory <- getProjectPath(project.id)
  
  allfiles <- list.files(startDirectory,recursive=TRUE,full.names=FALSE)	
  
  if(programsOnly){allfiles <- allfiles[grep("^Programs",allfiles)]}
  if(dataCopy){allfiles <- allfiles[-grep("^Data",allfiles)]}
  
  
  #targetDirectory <- getProjectPublishPath(project.id)
  
  #file.copy(file.path(allfiles),targetDirectory,recursive=TRUE,overwrite=TRUE)
  
  x <- data.frame(file=sample(allfiles)[1])
  
  copyResults <- ddply(data.frame(file=allfiles),"file",function(x){
    
    copyOut <- FALSE
    
    file0 <- file.path(startDirectory,as.character(x$file))
    file1 <- file.path(targetDirectory,as.character(x$file))
    
    fileExists <- file.exists(file1)
    
    if(fileExists){
      if(!speedCopy){
        fileExists <- digest::digest(file=file0,serialize=FALSE,algo="md5")==digest::digest(file=file1,serialize=FALSE,algo="md5")
      }else{
        info0 <- file.info(file0)
        info1 <- file.info(file1)
        fileExists <- (info0$mtime==info1$mtime)&(info1$size==info0$size)
      }		
    }
    
    if(!fileExists){
      dir.create(dirname(file1),recursive=TRUE)
      copyOut <- file.copy(file0,dirname(file1),overwrite=TRUE,recursive=TRUE,copy.date=TRUE)
    }
    data.frame(copied=copyOut)
    
    
  },.progress=progress_text(char = "."))
  
  return(copyResults)
  
}# END function: copyProject
