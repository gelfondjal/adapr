#' Read result filepaths to publish
#' @param project.id Project to publish
#' @return dataframe of files to publish
#' @details File is in support directory/files_to_publish.csv
#' @export
#' @examples 
#' \dontrun{
#' get.pubresults("adaprHome")
#'} 
#' 

get.pubresults <- function(project.id=get.project()){
  
  # Retrieves or creates publication table from project.id
  
  source_info <- pull_source_info(project.id)
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
#' send.pubresults("adaprHome")
#'} 
#' 
send.pubresults <- function(project.id=get.project()){

  publication.table <- get.pubresults(project.id)
  
  pubpath <- get.project.publish.path(project.id)
  
  if(nrow(publication.table)>0){
    
    publication.table <- publication.table[order(basename(publication.table$Path)),]  
    file.copy(file.path(get.project.path(project.id),publication.table$Path),pubpath,overwrite=TRUE)
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
#' show.pubresults("adaprHome")
#'} 
#' 
#' 
show.pubresults <- function(project.id=get.project()){
  
  source_info <- pull_source_info(project.id)
  publication.file <- file.path(source_info$project.path,project.directory.tree$support,"files_to_publish.csv")
  if(file.exists(publication.file)){
  
    utils::browseURL(publication.file)
    
      }else{
    publication.table <- data.frame(Path="",Description="")[-1,]
    utils::write.csv(publication.table,publication.file,row.names=FALSE)
    utils::browseURL(publication.file)
    
  }  
  
  
}


