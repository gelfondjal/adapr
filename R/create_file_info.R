#' Creates a list containing information about a file
#' @param path is the directory file resides
#' @param file is the filename
#' @param description is the description of the file
#' @param dbname is the database name 
#' @param si is the source_info list default is null
#' @return file info list with path filename fullname description and db.name
#' @export
#' @examples 
#'\dontrun{
#' file0 <- file.path(pull_source_info("adaprHome")$project.path,
#' project.directory.tree$analysis,"read_data.R")
#' Create.file.info(dirname(file0),basename(file0),"a program that reads")
#'} 
#' 
Create.file.info <- function(path,file,description="",dbname="",si=NULL){
  
  if(!is.null(si)){
    t <- c(path,file)
    description <- subset(si$all.files,(si$all.files$path==t[1])&(si$all.files$file==t[2]))$description[1]
    
  }
  
  file.info <- list(path,file,file.path(path,file),gsub("\t","",description),gsub("\\.","_",file))
  
  names(file.info) <- c("path","file","fullname","description","db.name")
  
  return(file.info)
  
}	