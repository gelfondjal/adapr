#' check the synchrony of source files and their created objects
#' @param g.all dependency object or set of dependency objects making a directed acyclic graph
#' @param tree  dependency tree corresponding to dagger
#' @param plotl logical for plotting or not
#' @export

Sync.test <- function(dagger,tree,plotl=TRUE){
  
  
  if(!is.dag(dagger)){stop("The computing dependencies have cycles.")}
  
  
  # track the run time of source files
  # track the modification time of target files
  V(dagger)$time <- ifelse(V(dagger)$file.class=="source",V(dagger)$run.time,V(dagger)$mod.time)
  
  children.list <- get.adjlist(dagger,mode="out")
  
  updated.logical <- matrix(FALSE,length(children.list),1,dimnames=list(V(dagger)$name,"time"))
  
  # find the out dated nodes
  
  parent <- names(children.list)[1]
  
  for(parent in names(children.list)){
    
    if(length(children.list[[parent]])>0){
      
      parent.time <- V(dagger)$time[V(dagger)$name==parent]
      
      min.child.time <- min(V(dagger)$time[children.list[[parent]]])
      
      updated.logical[parent,] <- parent.time > min.child.time
      
      if(parent.time > min.child.time){
        
        print(paste("Parent younger than child:","Parent =",parent,parent.time))
        
        child.times <- subset(data.frame(child=V(dagger)$name[children.list[[parent]]], time=V(dagger)$time[children.list[[parent]]]),time<parent.time)
        print("Child times")
        print(child.times)
        
      }
      
    }
    
  }
  
  
  
  vertex.updates <- rownames(updated.logical)[updated.logical]
  
  V(dagger)$color <- redblue(length(V(dagger)))[rank(V(dagger)$time)]
  #	par(mfrow=c(1,2))
  #	plot(dagger,main="Dagger with time as color")
  
  
  dagger.updated <- dagger
  V(dagger.updated)$color <- ifelse(V(dagger.updated)$name %in% vertex.updates,"red","white")
  
  if(plotl){plot(dagger.updated,main="Dagger with out of sync detection")}
  
  # check for file hash inconsistencies
  
  file.check <- Check.file.hash(dependency.object=tree)
  
  file.info <- Condense.file.info(tree)
  
  # get the abbreviated names from the tree to match to the vertex
  
  if(file.check$hash.fail){
    
    all.fail <- rbind.fill(file.check$stale.hash,file.check$multiple.hash)
    
    failed.tree <- merge(all.fail,file.info,by=c("file","path"))
    
    print("Hash fails")
    print(failed.tree)
    
    failed.fullname.abbr <- failed.tree$fullname.abbr
    
    vertex.updates <- unique(c(vertex.updates,failed.fullname.abbr))
    
    
  }
  
  # combine file modification time inconsistencies with file hash inconsistencies
  
  
  
  
  # if target output is out of sync then update the
  # source file that creates it
  
  if(length(vertex.updates)>0){
    
    updated.vertex.info <- subset(file.info,fullname.abbr %in% vertex.updates)
    
    updated.vertex.info$target.path <- updated.vertex.info$path
    updated.vertex.info$target.file <- updated.vertex.info$file
    
    failed.tree.2 <- merge(updated.vertex.info,tree,by=c("target.file","target.path"))
    
    source.for.targets <- subset(file.info,file==unique(subset(failed.tree.2,dependency=="out")$source.file))$fullname.abbr
    
  }else{return(list(synchronized=TRUE))}
  
  vertex.updates <- unique(c(vertex.updates,source.for.targets))
  
  
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
  
  return(list(synchronized=FALSE,updated.graph = dagger.updated,propagated.graph=dagger.propagated,stale.graph=stale.graph,sources.to.sync=sources.to.sync))
  
  
  
} #END: Sync test