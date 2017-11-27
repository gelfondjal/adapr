#' Read data and capture the file information within dependency object
#' @param file.name name of file
#' @param description description of data file
#' @param read.fcn function for reading file
#' @param ... arguments to read function
#' @details Main fuction for reading file data in projects. 
#' Wrapper function for Read.cap, automatically generates file information. 
#' Assumes file is in project "Data" directory.
#' Use this in the body of the program.
#' Guesses which function to use to read the file, but user can specify any function that given a file name returns
#' an R object.
#' @return object read from file
#' @export
#'@examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' write.csv(cars,file.path(source_info$data.dir,"test.csv"))
#' cardata <- Read("test.csv","cars dataframe",as.is=TRUE)
#' file.remove(file.path(source_info$data.dir,"test.csv"))
#'}  
#'
Read <- function(file.name="data.csv",description="Data file",read.fcn=guess.read.fcn(file.name),...){
  
  # lightweight read.cap take small number of args
  # used file.name and description create file.information
  # reads from data directory
  
  
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("Read (adapr) error: source_info not found")
    
  }
  
  
  
  if(dirname(file.name)!="."){
    inpath <- file.path(source_info$data.dir,dirname(file.name))
    file.name <- basename(file.name)
  }else{inpath <- source_info$data.dir}
  
  if(!file.exists(file.path(inpath,file.name))){stop(paste("Read error: file does not exists:",file.path(inpath,file.name)))}
  
  file.info <- createFileInfo(inpath,basename(file.name),description)
  
  read.obj <- Read.cap(file.info,read.fcn,options()$adaprScriptInfo,...)
  
  return(read.obj)
  
}
#' Tracks files that read by functions not in adapr and captures the file information within dependency object
#' @param file.name name of file (vectorized)
#' @param description description of data file (vectorized)
#' @details Allows tracking of files read by other functions than Read. Assumes file is in project "Data" directory
#' @return Filepath of file to read
#' @export
#' @examples 
#'\dontrun{
#'  source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' write.csv(cars,file.path(source_info$data.dir,"test.csv"))
#' # Read with any function
#' temp <- utils::read.csv(file.path(source_info$data.dir,"test.csv"))
#' ReadTrack("test.csv","cars dataframe")
#' # Will track the file as though read with Read().
#' file.remove(file.path(source_info$data.dir,"test.csv"))
#'} 
ReadTrack <- function(file.name="data.csv",description="Data file"){
  
  # lightweight read.cap take small number of args
  # used file.name and description create file.information
  # reads from data directory
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("ReadTrack (adapr) error: source_info not found")
    
  }
  
  # Vectorize
  if(length(file.name)==0){stop("Error: ReadTrack (adapr) error: file length 0")}
  
  if(length(file.name)>1){
    if(length(description)==1){description <- rep(description,length(file.name))}
    if(length(description)!=length(file.name)){stop("ReadTrack (adapr) error: file length description mismatch")}
  }
  
  for(i in 1:length(file.name)){
  
  if(dirname(file.name[i])!="."){
    inpath <- file.path(source_info$data.dir,dirname(file.name[i]))
    file.name[i] <- basename(file.name[i])
  }else{inpath <- source_info$data.dir}
  
  if(!file.exists(file.path(inpath,file.name[i]))){stop(paste("Read error: file does not exists:",file.path(inpath,file.name)))}
  
  file.info <- createFileInfo(inpath,basename(file.name[i]),description[i])
  
  read.obj <- Read.cap(file.info,I,source_info)
  }
  return(read.obj)
  
}
