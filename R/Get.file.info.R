#' Retrieve the file info for the file by name OR from the data subdirectory data
#' @param source_info Project information in list
#' @param data Data file name
#' @param file0 Filename not in data
#' @param path.grep Path to search for file in
#' @return File information list outcome of search
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' Get.file.info(source_info,file0="read_data.R")
#'} 
#' 
Get.file.info <- function(source_info,data="",file0="",path.grep=""){
  
  
  
  if(data!=""){
    file.row <- subset(source_info$all.files,(source_info$all.files$path==file.path(source_info$data.dir,data))&(source_info$all.files$file==file0))
  }else{
    file.row <- subset(source_info$all.files,(source_info$all.files$file==file0))  	  	
  }
  
  if(path.grep!=""){
    file.row <- subset(source_info$all.files,grepl(path.grep,source_info$all.files$fullname,fixed=TRUE))  	  	
  }
  
  if(nrow(file.row)<1){stop("Attempt to retreive non-extant file information")}
  
  if(nrow(file.row)>1){stop("Attempt to retreive non-unique file information")}
  
  file.info.out <- Create.file.info(file.row$path,file.row$file,file.row$description)
  
  return(file.info.out)
  
  
} #END: Get.file.info