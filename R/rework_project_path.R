#' Collects all trees in dependency.dir and changes the project path
#' @param dependency.dir location of dependency files to rework
#' @param new.path file path for the new project path
#' @details Used with swapping branches
#' @export
#' 
rework.project.path <- function(dependency.dir=source_info$depedency.dir,new.path=get.project.path(source_info$project.id)){
  
  
  
  dep.files <- list.files(dependency.dir,full.names=TRUE)
  
  list.deps <- lapply(dep.files,read.dependency)
  
  fixed.deps <- swap.project.paths(list.deps,new.path)
  
  names(fixed.deps) <- dep.files
  
  for(dep.file.iter in dep.files){
    
    write.table(fixed.deps[[dep.file.iter]],dep.file.iter,sep="\t",row.names=FALSE)
  }
  
}# END: rework.project.path 

