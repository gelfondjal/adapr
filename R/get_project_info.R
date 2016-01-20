#' Acquire all dependencies related to a project,
#' Generate graph of project dependencies,
#' Get all file information related to project
#' 
#' @param dependency.dir is the string location of dependency files
#' @return list with stacked dependency files, graph of dependencies, and condensed file information
#' @export
#' 
get.project.info <- function(dependency.dir){
  
  # get project object
  
  trees <- Harvest.trees(dependency.dir)
  
  g.all <- Make.summary.graph(dependency.dir=NULL,dependency.object=trees,plot.graph=FALSE)
  
  file.info.object <- Condense.file.info(trees)
  
  return(list("tree"=trees,"graph"=g.all,"all.files"=file.info.object))
  
}