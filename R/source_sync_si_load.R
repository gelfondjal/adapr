#' Synchronize project by IDENTIFYING necessary R scripts
#' @param source_info Project information within source_info list
#' @return Data.frame with sources needed to synchronize with run times
#' @export
#' @details Not usually direct use. See syncProject() and syncTestProject().
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' sourceSyncSILoad(source_info)
#'} 
sourceSyncSILoad <- function (source_info) 
{ 
  project_info <- getProjectInfoSI(source_info) 
  sync.out <- syncTestPI(project_info)
  if (sync.out$synchronized) {
    print(paste("Project synchronized"))
    return(NULL)
  }
  idSync.out <- sync.out$sources.to.sync
  if (nrow(idSync.out) == 0) {
    warning("There is nothing to run")
  }
  tree.to.run <- subset(project_info$tree, project_info$tree$source.file %in% 
                          idSync.out$file)
  sync.out <- syncTestSI(source_info)
  propagated.names <- igraph::V(sync.out$propagated.graph)$name[igraph::V(sync.out$propagated.graph)$synced == "No"]
 
  run.times <- plyr::ddply(tree.to.run, "source.file", function(x) {
    last.run.time <- max(as.POSIXct(x$target.mod.time) - 
                           as.POSIXct(x$source.run.time), na.rm = TRUE)
    return(data.frame(last.run.time.sec = last.run.time))
  })
  run.times$source.file <- as.character(run.times$source.file)
  return(list(run.times = run.times,idSync.out=idSync.out,sync.out=sync.out))
}
