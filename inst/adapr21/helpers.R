
fixWindowsDashes <- function(dirname){
  
  return(gsub("\\\\","/",dirname))
  
}

runtimes.source.sync.si <- function(source_info) 
{
  project_info <- getProjectInfoSI(source_info)
  sync.out <- syncTestPI(project_info)
  if (sync.out$synchronized) {
    print(paste("Project synchronized"))
    return(NULL)
  }
  ID.sync.out <- sync.out$sources.to.sync
  if (nrow(ID.sync.out) == 0) {
    warning("There is nothing to run")
  }
  tree.to.run <- subset(project_info$tree, source.file %in% 
                          ID.sync.out$file)
  sync.out <- syncTestSI(source_info)
  propagated.names <- V(sync.out$propagated.graph)$name[V(sync.out$propagated.graph)$synced =="No"]
  
  run.times <- ddply(tree.to.run, "source.file", function(x) {
    last.run.time <- max(as.POSIXct(x$target.mod.time) - as.POSIXct(x$source.run.time), na.rm = TRUE)
    return(data.frame(last.run.time.sec = last.run.time))
  })
  
  return(run.times)
  
}