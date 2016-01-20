#' Read data and capture the file information within dependency object
#' @param file.info file information list 
#' @param read.fcn function for reading the file
#' @param source_info source information list
#' @return object read from files
#' @export
#' 
Read.cap <- function(file.info,read.fcn,source_info,...){
  
  # read file using read.fcn
  # update dependency.file object in filesystem
  # ... options to write.fcn		
  # return object read from file
  
  read.obj <- read.fcn(file.info[["fullname"]],...)
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="in",stringsAsFactors=FALSE)
  
  source_info$dependency$update(df.update)
  
  return(read.obj)
  
  
} # Read.cap