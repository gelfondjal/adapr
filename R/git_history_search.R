#' Performes git history search
#' @param gitdir string with git directory
#' @param pattern to match in history
#' @param branch git branch
#' @param git_args arguments to git
#' @param git_binary location of git executable
#' @return git log output
#' @author Uses git_path adapted form devtools author Hadley Wickham
#' @export
#' @examples 
#'\dontrun{
#'  si <- pull_source_info("adaprHome")
#'  git.history.search(si$project.path,"read_data.R")
#'} 
#' 

git.history.search <- function(gitdir,pattern,branch = NULL, git_args = character(), git_binary = NULL){
  
  # git "commit" to the git repository in gitdir with message 
  
  git_binary_path <- git_path(git_binary)
  
  args <- c("log -S",shQuote(pattern))
  
  setwd(gitdir)
  
  git.out <- system2(git_binary_path, args, stdout = TRUE, stderr = TRUE)
  
  return(git.out)
  
}