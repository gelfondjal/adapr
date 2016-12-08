#' Checks git configuration. Requires git installation
#' @return integer 0 if configured otherwise 99 if not
#' @export
#' @author Uses git_path adapted form devtools author Hadley Wickham
#' @examples 
#'\dontrun{
#' #Requires git installation
#' git.configure.test()
#'} 
#' 
#' 

git.configure.test <-  function () {

    config.out <- 99
    
    try({

      git_binary_path <- git_path(NULL)
      
      config.out  <- system2(git_binary_path, paste("config --global user.name"))

    })

    return(config.out)
    
  }

