#' Loads a single R object from file
#' @param file contains R object
#' @return object for file that was read
#' @export
#' @examples 
#'\dontrun{
#' processed <- Load.branch("read_data.R/process_data.Rdata")
#'} 
Load.branch <- function(file){
  
  # Loads obj from source_info
  # updates dependency.file
  
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("Load.branch (adapr) error: source_info not found")
    
  }
  
  file.info <- getFileInfo(options()$adaprScriptInfo,data="",file0="",path.grep=file)
  
  obj <- load(file.info[["fullname"]],envir=parent.frame())
  
  
  #obj <- readRDS(file.info[["fullname"]])
  
  # print(file.info[["fullname"]])
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="in",stringsAsFactors=FALSE)
  
  options()$adaprScriptInfo$dependency$update(df.update)
  
  return(get(obj,envir=parent.frame()))
  
  #return(obj)
  
}
#' Loads a single R object from file, more flexible than Load.branch or base::load
#' @param file contains R object
#' @param read.fcn function to read the file, default readRDS
#' @param ... arguments passed to read.fcn
#' @return object for file that was read
#' @export
#' @examples 
#'\dontrun{
#' processed <- load.flex("read_data.R/process_data.RData")
#'} 
loadFlex <- function(file,read.fcn=readRDS,...){
  
  # Loads obj from source_info
  # updates dependency.file
  
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("load.flex (adapr) error: source_info not found")
    
  }
  
  file.info <- getFileInfo(options()$adaprScriptInfo,data="",file0="",path.grep=file)
  
  obj <- read.fcn(file.info[["fullname"]],...)
  
  # print(file.info[["fullname"]])
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="in",stringsAsFactors=FALSE)
  
  options()$adaprScriptInfo$dependency$update(df.update)
  
  #return(get(obj,envir=parent.frame()))
  
  return(obj)
  
}


#' Loads a single R object from file for a R Shiny app
#' @param project.id project name from which to load file
#' @param path directory that contains file to be loaded
#' @param file contains R object
#' @param read.fcn function to read the file, default readRDS
#' @param ... arguments passed to read.fcn
#' @return object for file that was read
#' @export
#' @examples 
#'\dontrun{
#' processed <- AppLoadFlex("adaprTest","Results/read_data.R","cardata.RData")
#'} 
AppLoadFlex <- function(project.id=getProject(),path,file,read.fcn=readRDS,...){
  
  # Loads obj from source_info
  # updates dependency.file
  
  fullpath <- file.path(getProjectPath(project.id),path,file)
  
  obj <- read.fcn(fullpath,...)
  
  return(obj)
  
}




