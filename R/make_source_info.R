#' Create source_info from project.id
#' @param project.id Project ID to use
#' @return source_info for generic source_info tree operations
#' @details Creates tree_controller.R directory. This directory is used for operation on the tree.
#' @export
pullSourceInfo <- function(project.id){
  
  source_info <- list()
  source_info$project.id <- project.id
  source_info$project.path <- getProjectPath(project.id)
  source_info$dependency.dir <- file.path(source_info$project.path,project.directory.tree$dependency.dir)
  source_info$results.dir <- file.path(source_info$project.path,project.directory.tree$results,"tree_controller.R")
  
  dir.create(source_info$results.dir,showWarnings=FALSE,recursive=TRUE)
  
  return(source_info)
  
}
