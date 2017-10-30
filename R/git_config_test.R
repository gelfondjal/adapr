#' Checks git configuration. Requires git installation
#' @param globalTF logical specifying global git configuration
#' @return output from git2r::config
#' @export
#' @author Uses git2r::config in git2r package.
#' @examples 
#'\dontrun{
#' #Requires git installation
#' gitConfigureTest()
#'} 
#' 
#' 
gitConfigureTest <-  function (globalTF=TRUE) {
    
    config.out  <- git2r::config(global=globalTF)
    
    #config.out <- 99
    # try({
    #   git_binary_path <- git_path(NULL)
    #   
    #   config.out  <- system2(git_binary_path, paste("config --global user.name"))
    # })
    
    return(config.out)
    
  }
