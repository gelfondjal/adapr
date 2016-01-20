#' Initiate git
#' @param gitdir string with git directory
#' @param branch git branch
#' @param git_binary location of git executable
#' @return git init lockaout
#' @export

git.init <- function(gitdir,branch = NULL, git_binary = NULL){
  
  # extract the git information related to a filename in the git repository in gitdir
  
  git_binary_path <- git_path(git_binary)
  
  args <- c("init")
  
  setwd(gitdir)
  
  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  
  
  return(git.out)
  
}