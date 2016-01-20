#' Generate condensed information about files from dependency object
#' @param trees dependency object
#' @return data frame with information about each file in project
#' @export
#' 
Condense.file.info <- function(trees){
  
  source.files <- subset(trees,!duplicated(source.file))
  
  source.files$time <- as.POSIXct(source.files$source.mod.time)
  
  source.files$path <- source.files$source.file.path
  
  source.files$file <- source.files$source.file
  
  source.files$source.run.time <- as.POSIXct(source.files$source.run.time)
  
  source.files$file.hash <- source.files$source.hash
  
  source.files$description <- source.files$source.file.description
  
  source.files$file.class <- "source"
  
  target.files <- data.frame(file=trees$target.file,path=trees$target.path,time=as.POSIXct(trees$target.mod.time),file.class="target",source.run.time=trees$source.run.time,
                             file.hash=trees$target.hash,description=trees$target.description,stringsAsFactors=FALSE)
  
  all.files <- rbind(subset(source.files,select=names(target.files)),target.files)
  
  all.files$path.abbr <- abbreviate(gsub(trees$project.path[1],"",all.files$path),2)
  
  all.files$unique.file <- file.path(all.files$path.abbr,all.files$file)
  
  all.files$fullname <- file.path(all.files$path,all.files$file)
  
  all.files$fullname.abbr <- file.path(all.files$path.abbr,all.files$file)
  
  
  all.files <- subset(all.files,!duplicated(all.files$fullname))
  
  
  return(all.files)
  
}