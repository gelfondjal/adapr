#' Checks the consistency of the dependency directory with the files within the file system.
#' Reports the source scripts that need to be updated.
#' @param dependency.dir Directory with dependency information files
#' @param dependency.object data frame with dependency information
#' @details Only needs one or the other argument. 
#' @return list of information about file hash mismatches
#' @export
#' @examples 
#' \dontrun{
#' 
#' checkFileHashSource(pullSourceInfo("adaprHome")$dependency.dir)
#' } 
#' 
checkFileHashSource <- function(dependency.dir=NULL,dependency.object=NULL){
  
  #equire(plyr)
  
  if(is.null(dependency.object)){
    
    trees <- readDependency(dependency.dir)
    trees <- subset(trees,!is.na(dependency))
  }else{trees <- dependency.object}
  
  source.df <- subset(trees,!duplicated(trees$source.hash))
  
  #Check source hashes are current
  
  missingIsTrue <- function(x){return(ifelse(is.na(x),TRUE,x))}
  
  source.hash.check <- plyr::ddply(source.df,c("source.file","source.file.path"),function(x){
    
    	current.hash <- ""
    	try({
     	current.hash <- Digest(file=file.path(x$source.file.path[1],x$source.file[1]),serialize=FALSE)
   		 })
    
		hash.fail <- missingIsTrue(current.hash != x$source.hash)    
	    
    return(data.frame(hash.fail))
  })
  
  failed.sources <- subset(source.hash.check,source.hash.check$hash.fail)
  failed.sources$file <- failed.sources$source.file
  failed.sources$path <- failed.sources$source.file.path
  #Check target hashes are current
  
  target.hash.check <- plyr::ddply(trees,c("source.file","source.file.path","target.path","target.file"),function(x){
    
    	current.hash <- ""
    	try({
     	 current.hash <- Digest(file=file.path(x$target.path[1],x$target.file[1]),serialize=FALSE)
   		 })
    
		x$hash.fail <- missingIsTrue(current.hash != x$target.hash)    
		
		return(x)
    
     })
  
  failed.targets <- subset(target.hash.check,target.hash.check$hash.fail)
  
  sources.of.failed.targets <- subset(failed.targets,select=c("source.file","source.file.path"))
  
  sources.of.failed.targets$file <- sources.of.failed.targets$source.file
  sources.of.failed.targets$path <- sources.of.failed.targets$source.file.path
  
  all.sources.torun <- rbind(subset(sources.of.failed.targets,select=c("file","path")),subset(failed.sources,select=c("file","path")))
  
  all.sources.torun <- unique(all.sources.torun)
  
  failed.targets$file <- failed.targets$target.file
  
  failed.targets$path <- failed.targets$target.path
  
  all.failures <- rbind(subset(sources.of.failed.targets,select=c("file","path")),subset(failed.sources,select=c("file","path")))
  
  out.list <- list(hash.fail= as.logical(nrow(all.failures)),stale.hash=unique(all.failures))
  
  return(out.list)
  
}
