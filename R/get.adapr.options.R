#' Returns the primary hub file with project location and id information
#' @param setoptions Logical specifying Execute Options
#' @return adaproptions
#' @export
#' @examples 
#'\dontrun{
#' opt <- get_adapr_options()
#' print(opt)
#'} 
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
#' @examples 
#'\dontrun{
#' opt <- get_adapr_options()
#' set_adapr_options("project.path",opt$project.path)
#' opt2 <- get_adapr_options()
#' identical(opt,opt2)
#'} 
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




#' Checks or changes the specified adapr project in R option "adaprProject"
#' @param project.id characters specifying project.id of working project
#' @param quickTest logical whether to check if project exists
#' @return value is specified project or default project
#' @details Default is adaprHome. Returns default if project does not exist.
#' @export
#'@examples 
#'\dontrun{
#'  set.project("adaprHome")
#'} 
#' 
set.project <- function(project.id="",quickTest=FALSE){
  
  defaultProject <- "adaprHome"
  
  test <- is.null(options()$adaprProject)
  
  if(test){
    if(project.id==""){
      options(adaprProject = defaultProject)
      return(defaultProject)
    }
  }
  
  
  if(quickTest){
    projects <- get_orchard()$project.id
  }else{
    projects <- project.id
  }
  
  if(project.id!=""){
    
    if(project.id %in% projects){
      options(adaprProject = project.id)
    }else{
      warning("adapr::set.project project.id does not exist")
      project.id <- defaultProject
      options(adaprProject = project.id)
    }
    
  }else{
    
    project.id <- options()$adaprProject
  }
  
  return(project.id)
  
}


#' Returns the  adapr project in R option "adaprProject"
#' @return Value is specified project or default project
#' @details Default is adaprHome. Returns default if project does not exist.
#' @export
#'@examples 
#' \dontrun{
#'  get.project()
#'} 
#' 
get.project <- function(){
  
  defaultProject <- "adaprHome"
  
  test <- is.null(options()$adaprProject)
  
  if(test){
      options(adaprProject = defaultProject)
      return(defaultProject)
  }else{
    
    return(options()$adaprProject)
  }
  
}




