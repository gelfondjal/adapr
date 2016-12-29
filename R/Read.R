#' Read data and capture the file information within dependency object
#' @param file.name name of file
#' @param description description of data file
#' @param read.fcn function for reading file
#' @param ... arguments to read function
#' @details Simpler command than Read.cap, automatically generates file info. Assumes file is in project "Data" directory
#' @return object read from file
#' @export
#'@examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' write.csv(cars,file.path(source_info$data.dir,"test.csv"))
#' Read("test.csv","cars dataframe")
#' file.remove(file.path(source_info$data.dir,"test.csv"))
#'}  
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
  
  file.info <- Create.file.info(inpath,basename(file.name),description)
  
  read.obj <- Read.cap(file.info,read.fcn,source_info,...)
  
  return(read.obj)
  
}




#' Tracks files that read by functions not in adapr and captures the file information within dependency object
#' @param file.name name of file
#' @param description description of data file
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
  
  
  if(dirname(file.name)!="."){
    inpath <- file.path(source_info$data.dir,dirname(file.name))
    file.name <- basename(file.name)
  }else{inpath <- source_info$data.dir}
  
  if(!file.exists(file.path(inpath,file.name))){stop(paste("Read error: file does not exists:",file.path(inpath,file.name)))}
  
  file.info <- Create.file.info(inpath,basename(file.name),description)
  
  read.obj <- Read.cap(file.info,I,source_info)
  
  return(read.obj)
  
}
