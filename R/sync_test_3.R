#' check the synchrony of source files and their created objects
#' @param g.all dependency object or set of dependency objects making a directed acyclic graph
#' @param tree  dependency tree corresponding to dagger
#' @param plotl logical for plotting or not
#' @export

Sync.test <- function(dagger,tree,plotl=FALSE){
  
  if(!is.dag(dagger)){stop("The computing dependencies have cycles.")}
  
  # track the run time of source files
  # track the modification time of target files
  V(dagger)$time <- ifelse(V(dagger)$file.class=="source",V(dagger)$run.time,V(dagger)$mod.time)
   
  file.info <- Condense.file.info(tree)
 
  # find the out dated nodes
  file.check <- Check.file.mtime.source(dependency.object=tree)

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
  
  V(dagger)$color <- "blue"
  #	par(mfrow=c(1,2))
  #	plot(dagger,main="Dagger with time as color")
  dagger.updated <- dagger
  V(dagger.updated)$color <- ifelse(V(dagger.updated)$name %in% vertex.updates,"red","white")
  
  if(plotl){plot(dagger.updated,main="Dagger with out of sync detection")}
  
  # check for file hash inconsistencies
  
  file.check <- Check.file.hash.source(dependency.object=tree)
  
  # get the abbreviated names from the tree to match to the vertex
  
  if(file.check$hash.fail){
    
    all.fail <- rbind.fill(file.check$stale.hash)
    
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
      
      children <- V(dagger.updated)$name[na.exclude(graph.bfs(dagger,vertex.updates.iter,unreachable=FALSE)$order)]
      
      updated.vertex.dependency <- union(children,updated.vertex.dependency)
      
    }
    
  }
  
  
  
  dagger.propagated <- dagger
  
  V(dagger.propagated)$color <- ifelse(V(dagger.propagated)$name %in% updated.vertex.dependency,"red","white")
  
  V(dagger.propagated)$synced <- ifelse(V(dagger.propagated)$name %in% updated.vertex.dependency,"No","Yes")
  
  
  
  #	plot(dagger.updated,main="dagger")
  #	plot(dagger.propagated,main="propagate")
  
  #	par(mfrow=c(1,1))
  
  stale.graph <- induced.subgraph(dagger.propagated,V(dagger.updated)[V(dagger.updated)$name %in% updated.vertex.dependency],impl="create_from_scratch")
  
  sources.to.sync <- ID.sync(file.info,stale.graph)
  
  return(list(synchronized=ifelse(nrow(sources.to.sync)>0,FALSE,TRUE),updated.graph = dagger.updated,propagated.graph=dagger.propagated,stale.graph=stale.graph,sources.to.sync=sources.to.sync))
  
  
  
} #END: Sync test