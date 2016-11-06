#' check the synchrony of source files and their created objects
#' @param dagger a directed acyclic igraph representing dependencies
#' @param tree  dependency tree corresponding to dagger
#' @param plotl logical for plotting or not
#' @return list with synchronizing information


Sync.test_2nd <- function(dagger,tree,plotl=TRUE){
  
  #equire(igraph)
  
  if(!igraph::is.dag(dagger)){stop("The computing dependencies have cycles.")}
  
  
  # track the run time of source files
  # track the modification time of target files
  igraph::V(dagger)$time <- ifelse(igraph::V(dagger)$file.class=="source",igraph::V(dagger)$run.time,igraph::V(dagger)$mod.time)
  
  children.list <- igraph::get.adjlist(dagger,mode="out")
  
  updated.logical <- matrix(FALSE,length(children.list),1,dimnames=list(igraph::V(dagger)$name,"time"))
  
  # find the out dated nodes
  
  parent <- names(children.list)[1]
  
  sources <- unique(tree$source.file)
  
  vertexnames <- igraph::V(dagger)$name
  
  vertexnames <- vertexnames[basename(vertexnames)%in% sources]
  
  
  for(parent in names(children.list)){
    
    if(length(children.list[[parent]])>0){
      
      parent.time <- igraph::V(dagger)$time[igraph::V(dagger)$name==parent]
      
      min.child.time <- min(igraph::V(dagger)$time[children.list[[parent]]])
      
      updated.logical[parent,] <- parent.time > min.child.time
      
      if(parent.time > min.child.time){
        
        print(paste("Parent younger than child:","Parent =",parent,parent.time))
        
        tempdf <- data.frame(child=igraph::V(dagger)$name[children.list[[parent]]], 
                   time=igraph::V(dagger)$time[children.list[[parent]]])  
        
        child.times <- subset(tempdf,tempdf$time<parent.time)
        print("Child times")
        print(child.times)
        
      }
      
    }
    
  }
  
  
  
  vertex.updates <- rownames(updated.logical)[updated.logical]
  
  igraph::V(dagger)$color <- "blue"
  #	par(mfrow=c(1,2))
  #	plot(dagger,main="Dagger with time as color")
  
  
  dagger.updated <- dagger
  igraph::V(dagger.updated)$color <- ifelse(igraph::V(dagger.updated)$name %in% vertex.updates,"red","white")
  
  if(plotl){graphics::plot(dagger.updated,main="Dagger with out of sync detection")}
  
  # check for file hash inconsistencies
  
  file.check <- Check.file.hash(dependency.object=tree)
  
  file.info <- Condense.file.info(tree)
  
  # get the abbreviated names from the tree to match to the vertex
  
  if(file.check$hash.fail){
    
    all.fail <- plyr::rbind.fill(file.check$stale.hash,file.check$multiple.hash)
    
    failed.tree <- merge(all.fail,file.info,by=c("file","path"))
    
    print("Hash fails")
    print(failed.tree)
    
    failed.fullname.abbr <- failed.tree$fullname.abbr
    
    vertex.updates <- unique(c(vertex.updates,failed.fullname.abbr))
    
    
  }
  
  # combine file modification time inconsistencies with file hash inconsistencies
  
  
  
  
  # if target output is out of sync then update the
  # source file that creates it

  # Don't need this anymore

#  if(length(vertex.updates)>0){
#    
#    updated.vertex.info <- subset(file.info,fullname.abbr %in% vertex.updates)
    
#    updated.vertex.info$target.path <- updated.vertex.info$path
#    updated.vertex.info$target.file <- updated.vertex.info$file
    
#    failed.tree.2 <- merge(updated.vertex.info,tree,by=c("target.file","target.path"))
    
#    source.for.targets <- subset(file.info,file==unique(subset(failed.tree.2,dependency=="out")$source.file))$fullname.abbr
    
#  }else{return(list(synchronized=TRUE))}
  
#  vertex.updates <- unique(c(vertex.updates,source.for.targets))
 



  
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
  
  sources.to.sync <- ID.sync(file.info,stale.graph)
  
  return(list(synchronized=ifelse(nrow(sources.to.sync)>0,FALSE,TRUE),updated.graph = dagger.updated,propagated.graph=dagger.propagated,stale.graph=stale.graph,sources.to.sync=sources.to.sync))
  
  
  
} #END: Sync test