#' Returns the primary hub file with project location and id information
#' @return orchard
#' @export
#' @examples 
#'\dontrun{
#' orchard <- get_orchard()
#' print(subset(orchard,project.id=="adaprHome"))
#'} 
#' 
#' 

get_orchard <- function(){
  
  # 
  
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")  
  
  if(!file.exists(orchard.site)){
    
    plant.orchard()
      
    return(NULL)
  }
  
  suppressWarnings(orchard <- utils::read.csv(orchard.site,as.is=TRUE)  )
  
  orchard <- orchard[order(orchard$project.id),]
  
  return(orchard)
}


#' Removes project from orchard, but doesn't delete project from file system
#' @param project.id0 which project to remove from orchard
#' @return orchard
#' @export
#' @examples 
#'\dontrun{
#' uproot_tree("adaprHome")
#'} 
#' 
#' 

uproot_tree <- function(project.id0){
  
  # 
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  orchard <- get_orchard()
  
  orchard <- subset(orchard,orchard$project.id!=project.id0)
  
  utils::write.csv(orchard,orchard.site,row.names=FALSE)
  
  return(orchard)
}
