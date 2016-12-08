#' Write object and capture file information
#' @param obj object to write
#' @param file.name file to write to the source "Result" directory
#' @param description describes object to write
#' @param write.fcn function for writing file of object type
#' @param date logical for adding date to filename
#' @param ... arguments passed to write.fcn
#' @return File information list 
#' @export
#' @examples 
#'\dontrun{
#' # Within an R script:
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' # Inside R script body:
#' write.csv(cars,file.path(source_info$results.dir,"test.csv"))
#' Write(cars,"cars.csv","cars dataframe")
#' # To examine effect:
#' showResults()  
#'} 
#' 


Write <- function(obj=NULL,file.name="data.csv",description="Result file",write.fcn=guess.write.fcn(file.name),date=FALSE,...){
  
  # lightweight Write.cap take small number of args
  # used file.name and description create file.information
  
  
  
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("Write (adapr) error: source_info not found")
    
  }
  
  
  if(date){
    
    prefix <- gsub("\\..*","",file.name)
    suffix <- gsub(".*\\.","\\.",file.name)
    
    datestring <- gsub("\\-","_",Sys.Date())
    
    
    file.name <- paste0(prefix,"_",datestring,suffix)
    
    file.name <- gsub(" ","_",file.name)
    
  }
  
  
  if(dirname(file.name)!="."){
    outpath <- file.path(source_info$results.dir,dirname(file.name))
    
  }else{outpath <- source_info$results.dir}
  
  
  outfile <- basename(file.name)
  
  file.info <- Create.file.info(outpath,outfile,description)
  
  write.obj <-Write.cap(obj,file.info,write.fcn,source_info,...)
  
  return(file.info)
  
}



#' Tracks files written by functions not in adapr and captures the file information within dependency object
#' @param file.name name of file
#' @param description description of data file
#' @details Allows tracking of files written by other functions than Write. Assumes file is in Results directory
#' @return Filepath of file that was written
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' write.csv(cars,file.path(source_info$results.dir,"test.csv"))
#' WriteTrack("cars.csv","cars dataframe")
#' showResults()  
#'} 
#'  

WriteTrack <- function(file.name="data.csv",description="Result file"){
  
  out <- Graph(file.name,description,I)
 
  return(out$fullname)
}



