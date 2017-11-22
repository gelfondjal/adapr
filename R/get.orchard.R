#' Returns the primary hub file with project location and id information
#' @return orchard
#' @export
#' @details Not for direct use. See listProjects for direct use.
#' @examples
#'\dontrun{
#' orchard <- get_orchard()
#' print(subset(orchard,project.id=="adaprHome"))
#'} 
#' 
#' 
get_orchard <- function(){
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")  
  
  if(!file.exists(orchard.site)){
    
    plantOrchard()
      
    return(NULL)
  }
  
  suppressWarnings(orchard <- utils::read.csv(orchard.site,as.is=TRUE)  )
  
  orchard <- orchard[order(orchard$project.id),]
  
  
  
  return(orchard)
}





#' Removes project from orchard, but doesn't delete project from file system
#' @param project.id0 which project to remove from orchard
#' @return Project listing data frame.
#' @export
#' @examples 
#'\dontrun{
#' removeProject("adaprHome")
#' relcateProject("adaprHome")
#'} 
#' 
#' 
removeProject <- function(project.id0){
  
  # 
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  orchard <- get_orchard()
  
  orchard <- subset(orchard,orchard$project.id!=project.id0)
  
  utils::write.csv(orchard,orchard.site,row.names=FALSE)
  
  return(orchard)
}
remove.project <- removeProject
#' Browses orchard in file system
#' @return orchard
#' @export
#' @examples 
#'\dontrun{
#' openProjectList()
#'} 
#' 
#' 
openProjectList <- function(){
  
  # 
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  out <- utils::browseURL(orchard.site)
  
  return()
}
#' List projects
#' @param allInfo logical whether to return all data
#' @param project.id0 character for specific project. Empty string default will list all projects.
#' @return data frame with project information
#' @export
#' @examples 
#'\dontrun{
#' listProjects(TRUE)
#'} 
#' 
#' 
listProjects <- function(project.id0="",allInfo=TRUE){
  
  out <- get_orchard()
  if(!allInfo){return(out$project.id)}
  if(project.id0!=""){out <- subset(out,out$project.id==project.id0)}
  return(out)
  
}
#' List project file information disk space, modification timespan, days inactive
#' @param project.id character vector of projects
#' @return dataframe with project information
#' @export
#' @examples 
#'\dontrun{
#' fileInfoProjects()
#'} 
#' 
fileInfoProjects <- function(project.id=listProjects()$project.id){
  
  size <- length(project.id)
  
  out <- data.frame(project.id=project.id,size=NA,startDate=Sys.time(),endDate=Sys.time())
  
  for(i in seq_along(project.id)){
    try({
  
          allfiles <- list.files(getProjectPath(project.id[i]),recursive = TRUE,full.names = TRUE)
      
          mtimes <- file.mtime(allfiles)
          
          out$startDate[i] <- min(mtimes)
          
          out$endDate[i] <- max(mtimes)
          
          out$ageDays[i] <- round(difftime(out$endDate[i],out$startDate[i],units = "days"),1)
          
          out$inactiveDays[i] <- round(difftime(Sys.time(),out$endDate[i],units = "days"),1)
          
          out$size[i] <- sum(file.size(allfiles),na.rm = TRUE)
      
    })
  
  
  
  }
  
  return(out)
  
}#END: filespace.project
