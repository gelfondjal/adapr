#' Given Project name, Return project directory
#' @param project_name is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project directory
#' @export
#' @examples 
#'\dontrun{
#' get.project.path("adaprHome")
#'} 
#' 

get.project.path <- function(project_name=NULL){
  
  
  all.projects <- get_orchard()
  
  if(is.null(project_name)){return(all.projects)}
  
  project.out <- subset(all.projects,all.projects$project.id==project_name)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$project.path))
  
  
} #END get.project.path

#' Given Project name, Return project publish directory
#' @param project_name is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project directory
#' @export
#' @examples 
#'\dontrun{
#' get.project.publish.path("adaprHome")
#'} 
#' 

get.project.publish.path <- function(project_name=NULL){
  
  all.projects <- get_orchard()
  
  if(is.null(project_name)){return(all.projects)}
  
  project.out <- subset(all.projects,all.projects$project.id==project_name)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$swap.directory))
  
  
} #END get.project.publish.path


