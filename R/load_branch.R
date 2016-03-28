#' Loads a single R object from file
#' @param file contains R object
#' @return object for file that was read
#' @export
#' 
Load.branch <- function(file){
  
  # Loads obj from source_info
  # updates dependency.file
  
  
  file.info <- Get.file.info(source_info,data="",file0="",path.grep=file)
  
  obj <- load(file.info[["fullname"]],envir=parent.frame())
  
  # print(file.info[["fullname"]])
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="in",stringsAsFactors=FALSE)
  
  source_info$dependency$update(df.update)
  
  return(get(obj,envir=parent.frame()))
  
}