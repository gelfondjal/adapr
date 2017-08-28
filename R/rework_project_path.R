#' Lower level function that collects all trees in dependency.dir and changes the project path
#' @param dependency.dir location of dependency files to rework
#' @param new.path file path for the new project path
#' @details Not for direct use. Used with swapping branches
#' @export
#' 
reworkProjectPath <- function(dependency.dir=get("source_info")$depedency.dir,new.path=getProjectPath(get("source_info")$project.id)){
  
  
  
  dep.files <- list.files(dependency.dir,full.names=TRUE)
  
  list.deps <- lapply(dep.files,read.dependency)
  
  fixed.deps <- swapProjectPath(list.deps,new.path)
  
  names(fixed.deps) <- dep.files
  
  for(dep.file.iter in dep.files){
    
    utils::write.table(fixed.deps[[dep.file.iter]],dep.file.iter,sep="\t",row.names=FALSE)
  }
  
}# END: reworkProjectPath 
