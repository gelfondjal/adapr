#' Write data and capture the file information within dependency object
#' @param obj object to write, if null then open graphics device
#' @param file.info file information list 
#' @param write.fcn function to write file
#' @param source_info source information list
#' @param ... arguments passed to write.fcn
#' @return file.info file information returned
#' @export
#' @details Not usually direct use. See Write() and Graph().
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' testfile <- file.path(source_info$results.dir,"test.csv")
#' fileinfo <- Create.file.info(dirname(testfile),basename(testfile),"cars dataset")
#' Write.cap(cars,fileinfo,write.csv,source_info,row.names=FALSE)
#'} 

Write.cap <- function(obj=NULL,file.info,write.fcn,source_info,...){
  
  # write obj to file using write.fcn
  # update dependency.file object in filesystem
  # ... options to write.fcn
  
  
  if(is.null(obj)){
    write.fcn(file.info[["fullname"]],...)
    #	print(c("wrote",file))
  }else{write.fcn(obj,file=file.info[["fullname"]],...)}
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="out",stringsAsFactors=FALSE)
  
  source_info$dependency$update(df.update)
  
  return(file.info)
  
} # Write.cap