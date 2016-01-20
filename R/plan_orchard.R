#' Create project hub files in root directory
#' @return logical for succesful creation or not
#' @export
#' 
plant.orchard <- function(){
  
  # 
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv")	
  
  if(!file.exists(orchard.site)){
    
    dir.create(file.path(path.expand.2("~"),"ProjectPaths"))
    
    empty.orchard <- data.frame(project.id="",project.path="",swap.directory="")[-1,]	
    
    write.csv(empty.orchard,orchard.site,row.names=FALSE)
    
    
    return(TRUE)
  }
  
  return(FALSE)
}
