#' Take list of dependency file data and changes the project path
#' @param list.deps list of dependency file data
#' @param new.path file path for the new project path
#' @details Used with swapping branches by rework.project.path()
#' @return Updated list of dependency data
#' @export
#' 
swap.project.paths <-   function(list.deps,new.path=get.project.path(source_info$project.id)){
  
  # list.deps = list of dependency output files
  
  old.project.path <- list.deps[[1]]$project.path[1]
  
  
  shaved.variables <- c("path","source.file.path","target.path","project.path")
  
  
  for(i in 1:length(list.deps)){
    
    for(char.shave in shaved.variables){
      
      for(file.iter in 1:nrow(list.deps[[i]])){
        
        old.project.path <- list.deps[[i]]$project.path[file.iter] 	
        
        list.deps[[i]][[char.shave]][file.iter] <- gsub(old.project.path,"",list.deps[[i]][[char.shave]][file.iter],fixed=TRUE)
        
        list.deps[[i]][[char.shave]][file.iter] <- paste0(new.path,list.deps[[i]][[char.shave]][file.iter])
        
      }
      
    }
    
    
  }
  
  return(list.deps)
  
}# END: swap.project.paths	