#' Retrieve project swap directory 
#' @param project_name Project ID
#' @return file path for swap directory
#' @export
#' 
get.project.swap.directory <-function(project_name=NULL){
  
  
  all.projects <- read.csv(file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv"),as.is=TRUE)
  
  if(is.null(project_name)){return(all.projects)}
  
  project.out <- subset(all.projects,project.id==project_name)
  
  if(nrow(project.out)!=1){stop("Project.id cannot be used to resolve project path")}
  
  return(as.character(project.out$swap.directory))
  
  
}
