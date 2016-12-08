#' Checks the consistency of the dependency directory with the files within the file system
#' @param dependency.dir Directory with dependency information files
#' @param dependency.object data frame with dependency information
#' @details Only needs one or the other argument
#' @return list of information about file hase mismatches
#' @export
#' @examples 
#' \dontrun{
#' Check.file.hash(pull_source_info("adaprHome")$dependency.dir)
#' } 
#' 
#' 
Check.file.hash <- function(dependency.dir=NULL,dependency.object=NULL){
  
  #equire(plyr)
  
  if(is.null(dependency.object)){
    
    trees <- Harvest.trees(dependency.dir)
    trees <- subset(trees,!is.na(dependency))
  }else{trees <- dependency.object}
  
  source.df <- subset(trees,!duplicated(trees$source.hash))
  
  source.hash.count <- plyr::ddply(source.df,"source.file",function(x){
    
    all.hash <- unique(x$source.hash)
    
    n.unique.hash <- length(unique(all.hash))
    
    out.counts <- data.frame(path=x$source.file.path[1],file=x$source.file[1],n.unique.hash=n.unique.hash,file.hash=all.hash,stringsAsFactors=FALSE)
    
    return(out.counts)
  })
  
  target.hash.count <- plyr::ddply(trees,c("target.path","target.file"),function(x){
    
    all.hash <- unique(x$target.hash)
    
    n.unique.hash <- length(unique(all.hash))
    
    out.counts <- data.frame(path=x$target.path[1],file=x$target.file[1],n.unique.hash=n.unique.hash,file.hash=all.hash,stringsAsFactors=FALSE)
    
    return(out.counts)
  })
  
  all.hash.counts <- plyr::rbind.fill(source.hash.count,target.hash.count)
  
  multiple.hash <- subset(all.hash.counts,all.hash.counts$n.unique.hash!=1,select=c("path","file","file.hash","n.unique.hash"))
  
  
  hash.compute <- plyr::ddply(all.hash.counts,c("path","file","file.hash","n.unique.hash"),function(x){
    
    current.hash <- ""
    try({
      current.hash <- Digest(file=file.path(x$path[1],x$file[1]),serialize=FALSE)
    })
    
    out.counts <- data.frame(current.hash=current.hash,stringsAsFactors=FALSE)
    
    return(out.counts)
    
    
  })
  
  stale.hash <- subset(hash.compute,hash.compute$current.hash!=hash.compute$file.hash)
  
  hash.fail <- as.logical(nrow(multiple.hash)+nrow(stale.hash))
  
  out.list <- list(hash.fail=hash.fail,stale.hash=stale.hash,multiple.hash=multiple.hash)
  
  return(out.list)
  
}