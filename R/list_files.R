#' Lists the branches available for loading in the adapr project
#' @param si is source_info object
#' @return dataframe of objects and descriptions
#' @export
#' 
listBranches <- function(si=source_info){
  
  file_data<- si$all.files
  file_sub <- subset(file_data,grepl("Rdata$",file),select = c("file","path","description"))
  file_sub$path <- gsub(".*/","",file_sub$path)
  return(file_sub)
  
}


#' Lists the branches available for loading in the adapr project
#' @param si is source_info object
#' @return description of 
#' @export
#' 
listDatafiles <- function(si=source_info){
  
  # lightweight read.cap take small number of args
  # used file.name and description create file.information
  # reads from data directory
  
  file_data <- si$all.files
  file_sub <- subset(file_data,path==si$data.dir,select = c("file","path","description"))
  file_sub$path <- gsub(".*/","",file_sub$path)
  
  allfiles <- data.frame(file=list.files(si$data.dir,recursive=TRUE,full.names = 1),stringsAsFactors = FALSE)
  
  allfiles$path <- dirname(substring(as.character(allfiles$file),nchar(si$project.path)+2,nchar(as.character(allfiles$file))))
  
  allfiles$file <- basename(allfiles$file)
  
  allfiles <- merge(allfiles,file_sub,by=c("file","path"),all.x=TRUE)
  
  return(allfiles)
  
}



