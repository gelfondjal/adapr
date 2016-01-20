#' Retrieves the information from git about a file
#' @param gitdir string with git directory
#' @param filename string of file to query
#' @param branch git branch
#' @param git_args string argument for git
#' @param git_binary location of git executable
#' @return git log for filename
#' @export
#' 
git.info <- function(gitdir,filename,branch = NULL, git_args = character(), git_binary = NULL){
  
  # extract the git information related to a filename in the git repository in gitdir
  
  git_binary_path <- git_path(git_binary)
  
  args <- c('log', shQuote(filename), git_args)
  temp <- getwd()
  setwd(gitdir)
  
  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  #print(temp)
  
  return(git.out)
  
}	
