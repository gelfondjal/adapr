#' Git commit of project.
#' @param commit.message message describing edits
#' @param project.id project to commit
#' @param addAll logical for adding every R script and support file to commit
#' @param checkSync logical for checking sync status
#' @return commit message
#' @details Need git option active. Uses git2r package. addAll = TRUE will increase compute time.
#' @export
#' @examples 
#' \dontrun{
#' commitProject("adaprHome","Did I change something?")
#'} 
#'
commitProject <- function(commit.message="",project.id=getProject(),addAll = FALSE,checkSync=TRUE){
  source_info <- pullSourceInfo(project.id)
  if(checkSync){
    test.sync0 <- syncTestSI(source_info)$synchronized
    synccheck <- ifelse(test.sync0,"SYNCHRONIZED","NOT SYNCd")
  }else{
    syncckeck <- "No sync test"
  }
  setwd(source_info$project.path)
  
  repo <- git2r::repository(source_info$project.path)
  
  analysis.dir <- file.path(source_info$project.path,project.directory.tree$analysis)
  
  if(addAll){
    all.programs <- matrix(list.files(analysis.dir,recursive=TRUE,full.names=TRUE))
    add <-  apply(all.programs,1,function(x){gitAdd(source_info$project.path,filename=x)})
  }
  
  committed <- git2r::commit(repo,message =paste(synccheck,commit.message))
  out <- paste("Git",commit2char(committed))
  return(out)
  
}
#' git2r commit class to character converter
#' @param commitclass Commit object
#' @return commit message
#' @details Uses git2r package.
#' @examples 
#' \dontrun{
#' committed <- git2r::commit(repo,message =commit.message)
#' out <- paste("Git",commit2char(committed))
#'} 
#'
commit2char <- function(commitclass){
  
  out <- paste(commitclass@sha,utils::capture.output(show(commitclass@author@when)),
               commitclass@author@name,commitclass@message)
  
  return(out)
  
}
