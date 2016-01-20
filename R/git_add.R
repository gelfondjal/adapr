#' git add to stage the file
#' @param gitdir string with git directory
#' @param filename string of file to query
#' @param branch git branch
#' @param git_args string argument for git
#' @param git_binary location of git executable
#' @return git output from git add
#' @export
#' 
git.add <- function(gitdir,filename,branch = NULL, git_args = character(), git_binary = NULL){
  
  # "add" filename in the git repository in gitdir
  
  git_binary_path <- git_path(git_binary)
  
  args <- c("add",shQuote(filename))
  
  setwd(gitdir)
  
  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  
  
  return(git.out)
  
}