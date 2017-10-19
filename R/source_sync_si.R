#' Lower level function that synchronizes project by running necessary R scripts. Loads from source_info list.
#' @param source_info Project information within source_info list
#' @param run logical indicated whether to run or just identify asynchrony
#' @param plot.to.file logical for writing file in tree_controller.R directory
#' @return Data.frame with sources needed to synchronize with run times
#' @export
#' @details Not usually direct use. See syncProject() and syncTestProject().
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' sourceSyncSI(source_info)
#'} 
sourceSyncSI <- function(source_info,run=TRUE,plot.to.file=FALSE){
  
  # Run in order 
  # Compute run times
  
  
  project_info <- getProjectInfoSI(source_info)
  
  sync.out <- syncTestPI(project_info)
  
  if(sync.out$synchronized){
    
    print(paste("Project synchronized"))
    
    return(NULL)}
  
  
  idSync.out <- sync.out$sources.to.sync
  
  if(nrow(idSync.out)==0){
    warning("There is nothing to run")
  }
  
  tree.to.run <- subset(project_info$tree,project_info$tree$source.file %in% idSync.out$file)	
  
  
  sync.out <- syncTestSI(source_info)
  
  propagated.names <- igraph::V(sync.out$propagated.graph)$name[igraph::V(sync.out$propagated.graph)$synced=="No"]
  
  
  syncPlotFile <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R","sync_updater.png")
  
  dir.create(dirname(syncPlotFile),showWarnings=FALSE)
  
  if(plot.to.file){grDevices::png(syncPlotFile)}
  
  #Plot.biggraph.horizontal(project_info$graph,title="Files to syncrhonise",black.vertex.list=propagated.names)			
  
  createProgramGraph(source_info$project.id)
  
  if(plot.to.file){grDevices::graphics.off()}
  
  run.times <- plyr::ddply(tree.to.run,"source.file",function(x){
    
    last.run.time <- max(difftime(as.POSIXct(x$target.mod.time) ,
                                  as.POSIXct(x$source.run.time),units="secs"), na.rm = TRUE)
    
    return(data.frame(last.run.time.sec=last.run.time))
    
    
  })
  
  run.times$source.file <- as.character(run.times$source.file)
  
  
  print("Starting synchronization")
  
  

  
  if(run&(nrow(idSync.out)>0)){
    
    for(source.iter in 1:(nrow(idSync.out)+1)){
      
      sync.out <- syncTestSI(source_info)
      
      if(sync.out$synchronized){
        propagated.names <- ""
      }else{
        propagated.names <- igraph::V(sync.out$propagated.graph)$name[igraph::V(sync.out$propagated.graph)$synced=="No"]}
      
      remaining.time <- paste0(sum(subset(run.times,run.times$source.file %in% sync.out$sources.to.sync$file)$last.run.time.sec,na.rm=TRUE)," secs")
      
      title.of.graph <- paste(ifelse(sync.out$synchronized,"Sychronized Remaining","Files to synchronize"),"Run time = ",remaining.time,
                      "\n",idSync.out$file[source.iter])        
                              
                      
      
      if(plot.to.file){grDevices::png(syncPlotFile)}
      
      
      createProgramGraph(source_info$project.id)
      
      if(plot.to.file){grDevices::graphics.off()}
      
      if(source.iter<=nrow(idSync.out)){devtools::clean_source(file.path(idSync.out$path[source.iter],idSync.out$file[source.iter]))}
      
      
    }
    
    
    
    
    
  }
  
  
  
  return(run.times)
  
  
}# sourceSyncSI
