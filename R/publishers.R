#' Read result filepaths to publish
#' @param project.id Project to publish
#' @return dataframe of files to publish
#' @details File is in support directory/files_to_publish.csv
#' @export
#' @examples 
#' \dontrun{
#' getPubResults("adaprHome")
#'} 
#' 
getPubResults <- function(project.id=getProject()){
  
  # Retrieves or creates publication table from project.id
  
  source_info <- pullSourceInfo(project.id)
  publication.file <- file.path(source_info$project.path,project.directory.tree$support,"files_to_publish.csv")
  if(file.exists(publication.file)){
    publication.table <- utils::read.csv(publication.file,as.is=TRUE)
  }else{
    publication.table <- data.frame(Path="",Description="")[-1,]
    utils::write.csv(publication.table,publication.file,row.names=FALSE)
  }  
  
  return(publication.table)
  
}
#' Read in results to publish & Copies results to the project's publication directory
#' @param project.id Project to publish
#' @return dataframe of files to publish
#' @details File is in support directory/files_to_publish.csv
#' @export
#' @examples 
#' \dontrun{
#' publishResults("adaprHome")
#'} 
#' 
publishResults <- function(project.id=getProject()){
  publication.table <- getPubResults(project.id)
  
  pubpath <- getProjectPublishPath(project.id)
  
  if(nrow(publication.table)>0){
    
    publication.table <- publication.table[order(basename(publication.table$Path)),]  
    file.copy(file.path(getProjectPath(project.id),publication.table$Path),pubpath,overwrite=TRUE)
    pubout <- paste("Published",nrow(publication.table),project.id, "files",Sys.time(),"to",pubpath)
    
  }else{
    pubout <- "Nothing to publish at this time."
  }
  
  return(pubout)
  
}
#' Browses publication table for editing
#' @param project.id Project to publish
#' @return dataframe of files to publish
#' @details File is in support directory/files_to_publish.csv
#' @export
#' @examples 
#' \dontrun{
#' browsePubFiles("adaprHome")
#'} 
#' 
#' 
browsePubFiles <- function(project.id=getProject()){
  
  source_info <- pullSourceInfo(project.id)
  publication.file <- file.path(source_info$project.path,project.directory.tree$support,"files_to_publish.csv")
  if(file.exists(publication.file)){
  
    utils::browseURL(publication.file)
    
      }else{
    publication.table <- data.frame(Path="",Description="")[-1,]
    utils::write.csv(publication.table,publication.file,row.names=FALSE)
    utils::browseURL(publication.file)
    
  }  
  
  
}
