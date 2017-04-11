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
  
  file.info <- Get.file.info(source_info,data="",file0="",path.grep=file)
  
  #obj <- load(file.info[["fullname"]],envir=parent.frame())
  
  
  obj <- readRDS(file.info[["fullname"]])
  
  # print(file.info[["fullname"]])
  
  df.update <- data.frame(target.file=file.info[["file"]],target.path=file.info[["path"]],target.description=file.info[["description"]],dependency="in",stringsAsFactors=FALSE)
  
  source_info$dependency$update(df.update)
  
  #return(get(obj,envir=parent.frame()))
  
  return(obj)
  
}