#' Git ignore the library file
#' @param project.id to ignore Project's library
#' @return success
#' @details Libraries can be large and difficult to track with Git so we can ignore these.
#' @export

gitIgnoreLibrary <- function(project.id=getProject()){
  
  # check if .gitignore exists:
  
  gitIgnoreFile <- file.path(getProjectPath(project.id),".gitignore")
  
  path2library <- gsub(getProjectPath(project.id),"",getProjectLibrary(project.id))
  
  if(!file.exists(gitIgnoreFile)){
    write(path2library,gitIgnoreFile,append=TRUE)
  }else{
    scans <- scan(gitIgnoreFile,what=character(),sep="\n")
    
    if(sum(scans==path2library)==0){
      write(path2library,gitIgnoreFile,append=TRUE)
    }
    
  }
  repo <- git2r::repository(getProjectPath(project.id))
  gitout <- git2r::add(repo,gitIgnoreFile)
  
  return(gitout)
}