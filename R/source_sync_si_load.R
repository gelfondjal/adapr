#' Synchronize project by IDENTIFYING necessary R scripts
#' @param source_info Project information within source_info list
#' @return Data.frame with sources needed to synchronize with run times
#' @export
source_sync_si_load <- function (source_info) 
{ 
  project_info <- get.project.info.si(source_info) 
  sync.out <- Sync.test.pi(project_info)
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
  sync.out <- sync.test.si(source_info)
  propagated.names <- V(sync.out$propagated.graph)$name[V(sync.out$propagated.graph)$synced == "No"]
 
  run.times <- ddply(tree.to.run, "source.file", function(x) {
    last.run.time <- max(as.POSIXct(x$target.mod.time) - 
                           as.POSIXct(x$source.run.time), na.rm = TRUE)
    return(data.frame(last.run.time.sec = last.run.time))
  })
  run.times$source.file <- as.character(run.times$source.file)

  return(list(run.times = run.times,ID.sync.out=ID.sync.out,sync.out=sync.out))
}
