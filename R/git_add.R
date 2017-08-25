#' git add to stage the file
#' @param gitdir string with git directory
#' @param filename string of file to query
#' @param branch git branch
#' @param git_args string argument for git
#' @param git_binary location of git executable
#' @return git output from git add
#' @author Uses git2r package.
#' @export
#' @examples 
#'\dontrun{
#' si <- pullSourceInfo("adaprHome")
#' file0 <- file.path(si$project.path,project.directory.tree$analysis,"read_data.R")
#' gitAdd(si$project.path,file0) 
#'} 
#' 
gitAdd <- function(gitdir,filename,branch = NULL, git_args = character(), git_binary = NULL){
  
  # "add" filename in the git repository in gitdir
  
  repo <- git2r::repository(gitdir)
  git2r::add(repo,filename)    
  
#  git_binary_path <- git_path(git_binary)
#  args <- c("add",shQuote(filename))
#  setwd(gitdir)
#  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  
  
  return(git2r::status(repo))
  
}
