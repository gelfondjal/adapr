#' git commit
#' @param gitdir string with git directory
#' @param message for commit
#' @param branch git branch
#' @param git_args arguments to git
#' @param git_binary location of git executable
#' @return git commit output
#' @export


git.commit <- function(gitdir,message,branch = NULL, git_args = character(), git_binary = NULL){
  
  # git "commit" to the git repository in gitdir with message 
  
  git_binary_path <- git_path(git_binary)
  
  message <- shQuote(message)
  
  args <- c("commit -m",message)
  
  setwd(gitdir)
  
  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  
  
  return(git.out)
  
}

