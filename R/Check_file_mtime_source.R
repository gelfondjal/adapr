#' Checks the consistency of the dependency directory with the files within the file system
#' Reports the source scripts that need to be updated!
#' @param dependency.dir Directory with dependency information files
#' @param dependency.object data frame with dependency information
#' @details Only needs one or the other argument. 
#' @return list of information about file hase mismatches
#' @export
#' @examples 
#'\dontrun{
#'  checkFileMtimeSource(pullSourceInfo("adaprHome")$dependency.dir)
#'} 
#' 
checkFileMtimeSource <- function(dependency.dir=NULL,dependency.object=NULL){
  
  #equire(plyr)
  
  if(is.null(dependency.object)){
    
    trees <- readDependency(dependency.dir)
    trees <- subset(trees,!is.na(trees$dependency))
  }else{trees <- dependency.object}
 
  source.df <- subset(trees,!duplicated(trees$source.hash))
  
  missingIsTrue <- function(x){return(ifelse(is.na(x),TRUE,x))}
  
  source.mtime.check <- plyr::ddply(source.df,c("source.file","source.file.path"),function(x){
    
    current.mtime <- ""
    try({
      current.mtime <- file.info(file=file.path(x$source.file.path[1],x$source.file[1]))$mtime
    })
    
    mtime.fail <- missingIsTrue(as.character(current.mtime)  != as.character(x$source.mod.time   ))
    
   # mtime.fail <- ifelse(is.na(mtime.fail),TRUE,mtime.fail)
    
    return(data.frame( mtime.fail))
  })
  
  failed.sources <- subset(source.mtime.check,source.mtime.check$mtime.fail)
  
  failed.sources$file <- failed.sources$source.file
  failed.sources$path <- failed.sources$source.file.path
  #Check target hashes are current
  
  target.mtime.check <- plyr::ddply(trees,c("source.file","source.file.path","target.path","target.file"),function(x){
    
    	currrent.mtime <- ""
    	try({
     	 current.mtime <- file.info(file=file.path(x$target.path[1],x$target.file[1]))$mtime
   		 })
    
    # Only gets 1 second level time resoluion, some OS may achieve more resolution	
    	
		x$mtime.fail <- missingIsTrue(as.character(current.mtime)  != as.character(x$target.mod.time))
		
		return(x)
    
     })
  
  failed.targets <- subset(target.mtime.check,target.mtime.check$mtime.fail)
  
  sources.of.failed.targets <- subset(failed.targets,select=c("source.file","source.file.path"))
  
  sources.of.failed.targets$file <- sources.of.failed.targets$source.file
  sources.of.failed.targets$path <- sources.of.failed.targets$source.file.path
  
  all.sources.torun <- subset(sources.of.failed.targets,select=c("file","path"))
  
  all.sources.torun <- unique(all.sources.torun)
  
  failed.targets$file <- failed.targets$target.file
  
  failed.targets$path <- failed.targets$target.path
  
  all.failures <- rbind(subset(sources.of.failed.targets,select=c("file","path")),subset(failed.sources,select=c("file","path")))
  
  out.list <- list(mtime.fail= as.logical(nrow(all.failures)),stale.mtime=unique(all.failures))
  
  return(out.list)
  
}
