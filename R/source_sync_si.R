#' Synchronize project by running necessary R scripts
#' @param source_info Project information within source_info list
#' @param run logical indicated whether to run or just identify asynchrony
#' @param plot.to.file logical for writing file in tree_controller.R directory
#' @return Data.frame with sources needed to synchronize with run times
#' @export
source.sync.si <- function(source_info,run=TRUE,plot.to.file=FALSE){
  
  # Run in order 
  # Compute run times
  
  
  project_info <- get.project.info.si(source_info)
  
  sync.out <- Sync.test.pi(project_info)
  
  if(sync.out$synchronized){
    
    print(paste("Project synchronized"))
    
    return(NULL)}
  
  
  ID.sync.out <- sync.out$sources.to.sync
  
  if(nrow(ID.sync.out)==0){
    warning("There is nothing to run")
  }
  
  tree.to.run <- subset(project_info$tree,source.file %in% ID.sync.out$file)	
  
  
  sync.out <- sync.test.si(source_info)
  
  propagated.names <- V(sync.out$propagated.graph)$name[V(sync.out$propagated.graph)$synced=="No"]
  
  
  syncPlotFile <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R","sync_updater.png")
  
  dir.create(dirname(syncPlotFile),showWarnings=FALSE)
  
  if(plot.to.file){png(syncPlotFile)}
  
  Plot.biggraph.horizontal(project_info$graph,title="Files to syncrhonise",black.vertex.list=propagated.names)			
  
  if(plot.to.file){graphics.off()}
  
  run.times <- ddply(tree.to.run,"source.file",function(x){
    
    last.run.time <- max(as.POSIXct(x$target.mod.time)-as.POSIXct(x$source.run.time),na.rm=TRUE)
    
    
    return(data.frame(last.run.time.sec=last.run.time))
    
    
  })
  
  run.times$source.file <- as.character(run.times$source.file)
  
  
  print("Starting synchronization")
  
  if(run){
    
    for(source.iter in 1:(nrow(ID.sync.out)+1)){
      
      sync.out <- sync.test.si(source_info)
      
      if(sync.out$synchronized){
        propagated.names <- ""
      }else{
        propagated.names <- V(sync.out$propagated.graph)$name[V(sync.out$propagated.graph)$synced=="No"]}
      
      remaining.time <- paste0(sum(subset(run.times,source.file %in% sync.out$sources.to.sync$file)$last.run.time.sec,na.rm=TRUE)," secs")
      
      title.of.graph <- paste(ifelse(sync.out$synchronized,"Sychronized Remaining","Files to synchronize"),"Run time = ",remaining.time,
                      "\n",ID.sync.out$file[source.iter])        
                              
                      
      
      if(plot.to.file){png(syncPlotFile)}
      
      
      Plot.biggraph.horizontal(project_info$graph,title=title.of.graph,black.vertex.list=propagated.names)			
      
      
      if(plot.to.file){graphics.off()}
      
      if(source.iter<=nrow(ID.sync.out)){clean_source(file.path(ID.sync.out$path[source.iter],ID.sync.out$file[source.iter]))}
      
      
    }
    
    
    
    
    
  }
  
  
  
  return(run.times)
  
  
}# source.sync.si