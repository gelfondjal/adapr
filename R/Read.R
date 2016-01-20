#' Read data and capture the file information within dependency object
#' @param file.name name of file
#' @param description description of data file
#' @param read.fcn function for reading file
#' @param ... arguments to read function
#' @details Simpler command than Read.cap, automatically generates file info. Assumes file is in project "Data" directory
#' @return object read from file
#' @export
#' 
Read <- function(file.name="data.csv",description="Data file",read.fcn=guess.read.fcn(file.name),...){
  
  # lightweight read.cap take small number of args
  # used file.name and description create file.information
  # reads from data directory
  
  
  if(dirname(file.name)!="."){
    inpath <- file.path(source_info$data.dir,dirname(file.name))
    
  }else{inpath <- source_info$data.dir}
  
  
  
  file.info <- Create.file.info(inpath,basename(file.name),description)
  
  read.obj <- Read.cap(file.info,read.fcn,source_info,...)
  
  return(read.obj)
  
}
