#' Given Project id, Return project directory
#' @param project.id0 is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project directory
#' @export
#' @examples 
#'\dontrun{
#' getProjectPath("adaprHome")
#'} 
#' 
getProjectPath <- function(project.id0=getProject()){
  
  
  all.projects <- get_orchard()
  
  if(is.null(project.id0)){return(all.projects)}
  
  project.out <- subset(all.projects,all.projects$project.id==project.id0)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$project.path))
  
  
} #END getProjectLibrary

#' Given Project name, Return project library directory
#' @param project.id0 is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project library directory. Will return empty string if default library.
#' @details Will create directory if doesn't already exist.
#' @export
#' @examples 
#'\dontrun{
#' getProjectLibrary("adaprHome")
#'} 
#' 
getProjectLibrary <- function(project.id0=getProject()){

  all.projects <- get_orchard()
  
  project.out <- subset(all.projects,all.projects$project.id==project.id0)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  testLibrary <- ifelse(is.na(as.logical(project.out$project.library)),TRUE,!as.logical(project.out$project.library))

  if(testLibrary&(project.out$project.library!="packrat")){return(getAdaprOptions()$library)}
  
  if(testLibrary&(project.out$project.library=="packrat")){return(.libPaths())}
  
  
  pather <- as.character(project.out$library.path)
  
  if((is.na(pather))|(pather=="")){pather <- file.path(getProjectPath(),project.directory.tree$support,project.directory.tree$library.bank)}
  
  
  rversion <- gsub("\\.","_",paste0(R.Version()$major,"_",R.Version()$minor))
  
  pather <- file.path(pather,rversion,.Platform$OS.type,gsub("\\.","_",make.names(utils::sessionInfo()$platform)))
  
  libprevious <- .libPaths()[1]
  
  if(!dir.exists(pather)){
    dir.create(pather,recursive = TRUE)
    .libPaths(pather)
    adaprInstall(library.location=pather)
    }
  
  .libPaths(libprevious)
  
  return(pather)
  
  
} #END getProjectLibrary


#' Given Project name, Return project publish directory
#' @param project_name is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project directory
#' @export
#' @examples 
#'\dontrun{
#' getProjectPublishPath("adaprHome")
#'} 
#' 
getProjectPublishPath <- function(project_name=NULL){
  
  all.projects <- get_orchard()
  
  if(is.null(project_name)){return(all.projects)}
  
  project.out <- subset(all.projects,all.projects$project.id==project_name)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$swap.directory))
  
  
} #END getProjectPublishPath
