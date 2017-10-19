#'  Returns the source files needed to repair synchrony
#' @param file.info Project file information
#' @param dag.to.sync Directed Acyclic graph in need of synchronization
#' @return data.frame with sources that need to be run, rows in run order
#' @export
#' @examples 
#'\dontrun{
#' projInfo <- getProjectInfo(pullSourceInfo("adaprHome")$dependency.dir)
#' trees <- readDependency(pullSourceInfo("adaprHome")$dependency.dir)
#' file.info <-condenseFileInfo(trees)
#' idSync(file.info,projInfo$graph) 
#'} 
idSync <- function(file.info,dag.to.sync){
  
  #
  # 
  # dagger is the dependency graph
  
  
  run.order <- igraph::topological.sort(dag.to.sync)
  
  tempdf <- data.frame(fullname.abbr=igraph::V(dag.to.sync)$name,file.class=igraph::V(dag.to.sync)$file.class,
                       file=igraph::V(dag.to.sync)$file,path=igraph::V(dag.to.sync)$path)[run.order,]
  
  
  repair.sync.file.info <- subset(tempdf,tempdf$file.class=="source")
  
  
  if(nrow(repair.sync.file.info)>0){
  
  	repair.sync.file.info$run.order <- seq(1,nrow(repair.sync.file.info))
  }else{
  	
  	repair.sync.file.info$run.order <- integer(0)
  }	
  return(repair.sync.file.info)
  
  
  
} #END: idSync
