#' Given Project name, Return project directory
#' @param project_name is string with project name 
#' @details Reads "~/ProjectPaths/projectid_2_diretory.csv" into dataframe
#' @return string containing project directory
#' @export

get.project.path <- function(project_name=NULL){
  
  
  all.projects <- read.csv(file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv"),as.is=TRUE)
  
  if(is.null(project_name)){return(all.projects)}
  
  project.out <- subset(all.projects,project.id==project_name)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$project.path))
  
  
} #END get.project.path


