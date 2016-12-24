#' Git commit of project.
#' @param project.id project to graph
#' @param commit.message message describing edits
#' @return commit message
#' @details Need git option active. Uses git2r package.
#' @export
#' @examples 
#' \dontrun{
#' commit.project("adaprHome","Did I change something?")
#'} 
#'
commit.project <- function(project.id=get.project(),commit.message=""){

  source_info <- pull_source_info(project.id)

  test.sync0 <- sync.test.si(source_info)$synchronized
  synccheck <- ifelse(test.sync0,"SYNCHRONIZED","NOT SYNCd")

  setwd(source_info$project.path)
  
  repo <- git2r::repository(source_info$project.path)
  
  analysis.dir <- file.path(source_info$project.path,project.directory.tree$analysis)
  
  all.programs <- matrix(list.files(analysis.dir,recursive=TRUE,full.names=TRUE))
  
  add <-  apply(all.programs,1,function(x){git.add(source_info$project.path,filename=x)})
  
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