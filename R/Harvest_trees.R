#' Collect trees from dependency directory
#' @param dependency.dir Directory with dependency files
#' @return data frame of stacked dependency files
#' @details adapts to relative or absolute directories
#' @export
#' @examples 
#'\dontrun{
#' Harvest.trees(pull_source_info("adaprHome")$dependency.dir) 
#'} 
#' 
#' 
Harvest.trees <- function(dependency.dir){
  
  #collects all trees in dependency.dir
  
  dep.files <- list.files(dependency.dir,full.names=TRUE)
  
  trees <- NULL
  
  if(length(dep.files)>0){
    
  list.deps <- lapply(dep.files,read.dependency)
  
  trees <- plyr::rbind.fill(list.deps)
    
  project.id <-  list.deps[[1]]$project.id[1] 
  
  new.path <- get.project.path(project.id)
  
  if(is.na(list.deps[[1]]$project.path[1])){
    
    shaved.variables <- c("path","source.file.path","target.path","project.path")
    
    for(char.shave in shaved.variables){
      
        trees[[char.shave]] <- file.path(new.path,trees[[char.shave]])  
      
    }
    
    return(trees)
    
  }
    
  
  } # if there exist dependency files
  
  
  return(trees)
}
