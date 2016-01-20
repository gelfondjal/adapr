#' Given source_info object, retrieves project information
#' @param source_info is list with source information
#' @return list with stacked dependency files, graph of dependencies, and condensed file information
#' @export
#' 
get.project.info.si <- function(source_info){
  
  # get project object
  
  dependency.dir <- source_info$dependency.dir
  
  trees <- Harvest.trees(dependency.dir)
  
  g.all <- Make.summary.graph(dependency.dir=NULL,dependency.object=trees,plot.graph=FALSE)
  
  file.info.object <- Condense.file.info(trees)
  
  return(list("tree"=trees,"graph"=g.all,"all.files"=file.info.object))
  
}