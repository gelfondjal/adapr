#' Lower level function that reads data and capture the file information within dependency object
#' @param file.info file information list 
#' @param read.fcn function for reading the file
#' @param source_info source information list
#' @param ... arguments passed to read.fcn
#' @return object read from files
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' testfile <- file.path(source_info$data.dir,"test.csv")
#' write.csv(cars,testfile)
#' fileinfo <- createFileInfo(dirname(testfile),basename(testfile),"cars dataset")
#' Read.cap(fileinfo,read.csv,source_info)
#' file.remove(file.path(source_info$data.dir,"test.csv"))
#'
#'} 
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
