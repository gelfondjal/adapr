#' Write data and capture the file information within dependency object
#' @param obj object to write, if null then open graphics device
#' @param file.info file information list 
#' @param source_info source information list
#' @return file.info file information returned
#' @export
#' 

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