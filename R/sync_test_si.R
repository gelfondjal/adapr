#' Tests the synchrony of files in dependency tree
#' @param source_info source_info containing project information 
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' 
#' 
#' 
sync.test.si <- function(source_info,plotl0=FALSE){
  
  project_info <- get.project.info.si(source_info)
  
  return(Sync.test(project_info$graph,project_info$tree,plotl=plotl0))}

