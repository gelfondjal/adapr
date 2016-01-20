#'  Returns the source files needed to repair synchrony
#'  @param file.info Project file information
#' @param dag.to.sync Directed Acyclic graph in need of synchronization
#' @return data.frame with sources that need to be run, rows in run order
#' @export
#' 
ID.sync <- function(file.info,dag.to.sync){
  
  #
  # 
  # dagger is the dependency graph
  
  
  run.order <- topological.sort(dag.to.sync)
  
  repair.sync.file.info <- subset(data.frame(fullname.abbr=V(dag.to.sync)$name,file.class=V(dag.to.sync)$file.class,
                                             file=V(dag.to.sync)$file,path=V(dag.to.sync)$path)[run.order,],file.class=="source")
  
  
  repair.sync.file.info$run.order <- 1:nrow(repair.sync.file.info)
  
  return(repair.sync.file.info)
  
  
  
} #END: ID.sync
