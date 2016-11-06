#' Lists the branches available for loading in the adapr project
#' @param si is source_info object
#' @return dataframe of descriptions available branches
#' @export
#' 
listBranches <- function(si=get("source_info")){
  
  file_data <- si$all.files
  
  if(length(file_data$file)==0){
    print("No available branch files")
    return(NULL)
  }
  
  file_sub  <- subset(file_data,grepl("Rdata$",file_data$file),select = c("file","path","description"))
  
  if(length(file_sub$file)==0){
    print("No available branches")
    return(NULL)
  }
  
  file_sub$path <- gsub(".*/","",file_sub$path)
  return(file_sub)
  
}


#' Lists the data files available for reading in the adapr project
#' @param si is source_info object
#' @return description of data files
#' @export
#' 
listDatafiles <- function(si=get("source_info")){
  
 
  allfiles <- data.frame(file=list.files(si$data.dir,recursive=TRUE,full.names = 1),stringsAsFactors = FALSE)
  
  allfiles$path <- dirname(substring(as.character(allfiles$file),nchar(si$project.path)+2,nchar(as.character(allfiles$file))))
  
  allfiles$file <- basename(allfiles$file)

    
  file_data <- si$all.files
  file_sub <- data.frame()
  try({
  file_sub <- subset(file_data,file_data$path==si$data.dir,select = c("file","path","description"))
  })
  if(nrow(file_sub)==0){
    
    return(allfiles)
    
  }

  file_sub$path <- gsub(".*/","",file_sub$path)
  
  allfiles <- merge(allfiles,file_sub,by=c("file","path"),all.x=TRUE)
  
  return(allfiles)
  
}


#' Opens results directory
#' @param si is source_info object
#' @param project.id character string specifies project 
#' @param rscript character string specifies the R script result directory to open
#' @details Use BrowseURL to open results directory
#' @export
#' 
showResults <- function(si=get("source_info"),project.id="",rscript=""){
  if(project.id==""){
    utils::browseURL(si$results.dir)
  }else{
    resultdir <- file.path(get.project.path(project.id),project.directory.tree$results,rscript)
    utils::browseURL(resultdir)
  }
}

#' Opens project directory
#' @param si is source_info object
#' @param project.id character string specifies project to open
#' @details Use BrowseURL to open project directory
#' @export
#' 
showProject <- function(si=get("source_info"),project.id = ""){
  
  if(project.id==""){
    utils::browseURL(si$project.path)
  }else{
    utils::browseURL(get.project.path(project.id))
  }
}



