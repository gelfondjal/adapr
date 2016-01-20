#' Checks git configuration
#' @return integer 0 if configured otherwise 99 if not
#' @export
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

