#' Checks or changes the specified adapr project in R option "adaprProject"
#' @param project.id characters specifying project.id of working project
#' @param quickTest logical whether to check if project exists
#' @return value is specified project or default project
#' @details Default is adaprHome. Returns default if project does not exist.
#' @export
#'@examples 
#'\dontrun{
#'  setProject("adaprHome")
#'} 
#' 
setProject <- function(project.id="",quickTest=TRUE){
  
  defaultProject <- "adaprHome"
  
  test <- is.null(options()$adaprProject)
  
  if(project.id==""){
    
    projects <- listProjects()$project.id

    print(projects)
    
    n <- (readline("Which project (specify project or row number)?"))
    
    if(!(n %in% 1:length(projects))){
      project.id <- n
    }else{
  
      project.id <- projects[as.integer(n)]
      
      
      }
    
  }
  
  if(test){
    if(project.id==""){
      options(adaprProject = defaultProject)
      
      .libPaths(getProjectLibrary(defaultProject))
      
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
      .libPaths(getProjectLibrary(project.id))
    }else{
      warning("adapr::setProject project.id does not exist")
      project.id <- defaultProject
      options(adaprProject = project.id)
      .libPaths(getProjectLibrary(defaultProject))
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
#'  getProject()
#'} 
#' 
getProject <- function(){
  
  defaultProject <- "adaprHome"
  
  test <- is.null(options()$adaprProject)
  
  if(test){
    options(adaprProject = defaultProject)
    return(defaultProject)
  }else{
    
    return(options()$adaprProject)
  }
  
}
get.project <- getProject()
