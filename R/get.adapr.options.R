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
    
    dir.create(dirname(options.site),recursive=TRUE)
    
    options.site0 <- system.file(option.file,package='adapr')

    file.copy(options.site0,options.site)    
 
  }
  
  suppressWarnings(option.data <- utils::read.csv(options.site,as.is=TRUE))
  
  adapr_options <- list()
  
  if(nrow(option.data)){  for(i in 1:nrow(option.data)){adapr_options[[option.data$option[i]]] <- option.data$value[i]}  }
                            
  if(setoptions){
    
    if(!(is.null(adapr_options[["PATH"]]))){Sys.setenv(PATH=adapr_options[["PATH"]])}
    
  }
  
  return(adapr_options)
}


#' Returns Modifies the primary adapr option file
#' @param optionname is name of option to modify
#' @param optionvalue is new value to give optionname
#' @return adaproptions
#' @export
#' 
set_adapr_options <- function(optionname="",optionvalue=""){
  # 
  
  options <- get_adapr_options(FALSE)

  if((as.numeric(version$major) +as.numeric(version$minor)/10)>=3.2){
  
  if(optionname=="project.path"){
    
    if(!dir.exists(optionvalue)){
      
      stop("Option project.path directory does not exist")
    }
    
  }
  
  if(optionname=="publish.path"){
    
    if(!dir.exists(optionvalue)){
      
      stop("Option publish.path directory does not exist")
    }
    
  }
  
  }#check dir exists if r version gt or = 3.2
  
  
  options[[optionname]] <- optionvalue
  
  dfout <- NULL
  
  for(i in 1:length(options)){
    
    dfout <- rbind(dfout,data.frame(option=names(options[i]),value=options[[i]]))
    
  }
  
  option.file <- "adapr_options.csv"
  
  options.site <- file.path(path.expand.2("~"),"ProjectPaths", option.file)  
  
  utils::write.csv(dfout,options.site,row.names=FALSE)
  
  return(options)
}










