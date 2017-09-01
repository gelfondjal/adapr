#' Lower level function that checks the synchrony of source files and their created objects
#' @param dagger a directed acyclic igraph representing dependencies
#' @param tree  dependency tree corresponding to dagger
#' @param plotl logical for plotting or not
#' @export
#' @return list with synchronizing information
#' @details Not for direct use. See syncProject() and syncTestProject().
#' @examples 
#'\dontrun{
#'  si <- pullSourceInfo("adaprHome")
#' projInfo <- getProjectInfo(si$dependency.dir)
#' syncTest(projInfo$graph,projInfo$tree)
#'} 
syncTest <- function(dagger,tree,plotl=FALSE){
  
  #equire(igraph)
  if(!igraph::is.dag(dagger)){stop("The computing dependencies have cycles.")}
  
  # track the run time of source files
  # track the modification time of target files
  igraph::V(dagger)$time <- ifelse(igraph::V(dagger)$file.class=="source",igraph::V(dagger)$run.time,igraph::V(dagger)$mod.time)
   
  file.info <- condenseFileInfo(tree)
 
  # find the out dated nodes
  file.check <- checkFileMtimeSource(dependency.object=tree)
  # vertices to update
  vertex.updates <- c()
  
  # get the abbreviated names from the tree to match to the vertex
  
  if(file.check$mtime.fail){
    
    all.fail <-file.check$stale.mtime
    
    failed.tree <- merge(all.fail,file.info,by=c("file","path"))
    
    print("Mod times file")
    print(failed.tree)
    
    failed.fullname.abbr <- failed.tree$fullname.abbr
    
    vertex.updates <- unique(c(vertex.updates,failed.fullname.abbr))
 
  }
  
  igraph::V(dagger)$color <- "blue"
  #	par(mfrow=c(1,2))
  #	plot(dagger,main="Dagger with time as color")
  dagger.updated <- dagger
  igraph::V(dagger.updated)$color <- ifelse(igraph::V(dagger.updated)$name %in% vertex.updates,"red","white")
  
  if(plotl){graphics::plot(dagger.updated,main="Dagger with out of sync detection")}
  
  # check for file hash inconsistencies
  
  file.check <- checkFileHashSource(dependency.object=tree)
  
  # get the abbreviated names from the tree to match to the vertex
  
  if(file.check$hash.fail){
    
    all.fail <- plyr::rbind.fill(file.check$stale.hash)
    
    failed.tree <- merge(all.fail,file.info,by=c("file","path"))
    
    print("Hash fails")
    print(failed.tree)
    
    failed.fullname.abbr <- failed.tree$fullname.abbr
    
    vertex.updates <- unique(c(vertex.updates,failed.fullname.abbr))
    
    
  }
  
  # combine file modification time inconsistencies with file hash inconsistencies
  
  
  
  
  # Propagate dependencies
  
  updated.vertex.dependency <- c()
  
  for(vertex.updates.iter in vertex.updates){
    
    print(vertex.updates.iter)
    
    if(!(vertex.updates.iter  %in% updated.vertex.dependency)){
      
      children <- igraph::V(dagger.updated)$name[stats::na.exclude(igraph::graph.bfs(dagger,vertex.updates.iter,unreachable=FALSE)$order)]
      
      updated.vertex.dependency <- union(children,updated.vertex.dependency)
      
    }
    
  }
  
  
  dagger.propagated <- dagger
  
  igraph::V(dagger.propagated)$color <- ifelse(igraph::V(dagger.propagated)$name %in% updated.vertex.dependency,"red","white")
  
  igraph::V(dagger.propagated)$synced <- ifelse(igraph::V(dagger.propagated)$name %in% updated.vertex.dependency,"No","Yes")
  
  
  
  #	plot(dagger.updated,main="dagger")
  #	plot(dagger.propagated,main="propagate")
  
  #	par(mfrow=c(1,1))
  
  stale.graph <- igraph::induced.subgraph(dagger.propagated,igraph::V(dagger.updated)[igraph::V(dagger.updated)$name %in% updated.vertex.dependency],impl="create_from_scratch")
  
  sources.to.sync <- idSync(file.info,stale.graph)
  
  return(list(synchronized=ifelse(nrow(sources.to.sync)>0,FALSE,TRUE),updated.graph = dagger.updated,propagated.graph=dagger.propagated,stale.graph=stale.graph,sources.to.sync=sources.to.sync))
  
  
  
} #END: Sync test
