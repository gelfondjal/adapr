#' Lower level function that takes list of dependency file data and changes the project path
#' @param list.deps list of dependency file data
#' @param new.path file path for the new project path
#' @details Not for direct use. Used with swapping branches by reworkProjectPath()
#' @return Updated list of dependency data
#' @export
#' 
swapProjectPath <-   function(list.deps,new.path=getProjectPath(get("source_info")$project.id)){
  
  # list.deps = list of dependency output files
  
  old.project.path <- list.deps[[1]]$project.path[1]
  
  
  shaved.variables <- c("path","source.file.path","target.path","project.path")
  
  
  if(length(list.deps)==0){stop("Error in swapProjectPath no dependencies")}
  
  for(i in 1:length(list.deps)){
    
    for(char.shave in shaved.variables){
      
      
      
      for(file.iter in seq_along(list.deps[[i]][,1])){
        
        old.project.path <- list.deps[[i]]$project.path[file.iter] 	
        
        list.deps[[i]][[char.shave]][file.iter] <- gsub(old.project.path,"",list.deps[[i]][[char.shave]][file.iter],fixed=TRUE)
        
        list.deps[[i]][[char.shave]][file.iter] <- paste0(new.path,list.deps[[i]][[char.shave]][file.iter])
        
      }
      
    }
    
    
  }
  
  return(list.deps)
  
}# END: swapProjectPath	
