#' Returns the primary hub file with project location and id information
#' @param setoptions Logical specifying Execute Options
#' @return adaproptions
#' @export
#' 
get_adapr_options <- function(setoptions=FALSE){
    # 
  option.file <- "adapr_options.csv"
  
  options.site <- file.path(path.expand.2("~"),"ProjectPaths", option.file)  
  
  if(!file.exists(options.site)){
    
    options.site0 <- system.file(option.file,package='adapr')

    file.copy(options.site0,options.site)    
 
  }
  
  suppressWarnings(option.data <- read.csv(options.site,as.is=TRUE))
  
  adapr_options <- list()
  
  if(nrow(option.data)){  for(i in 1:nrow(option.data)){adapr_options[[option.data$option[i]]] <- option.data$value[i]}  }
                            
  if(setoptions){
    
    if(!(is.null(adapr_options[["PATH"]]))){Sys.setenv(PATH=adapr_options[["PATH"]])}
    
  }
  
  return(adapr_options)
}
