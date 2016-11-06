#' Identify git provenance of file within a project
#' @param project.id Project id to search for history within
#' @param filepath File that will be hashed and search within Git history, File choose dialogue if not specified
#' @return list of 1) filename, 2) Git commit including commit message, date, author and 2) file info
#' @details Requires a Git commit snapshot within the project
#' @export
#'  
git_provenance <- function(project.id,filepath=0){

  if(filepath==0){filepath <- file.choose()}
  
  filehash <- Digest(file=filepath)
  
  gitpath <- get.project.path(project.id)
  
  provenance <- git.history.search(gitpath,filehash)  
  
  si <- pull_source_info(project.id)
  
  files <- Condense.file.info(Harvest.trees(si$dependency.dir))
                              
  file.data <- subset(files,files$file.hash==filehash)   
  
  if(length(provenance)==0){provenance <- "Git history not found"}
  if(nrow(file.data)==0){file.data <- "Current file not found"}
  
  
  return(list(file=filepath,gitHistory=provenance,currentDescription=file.data))
}


