#' Returns the primary hub file with project location and id information
#' @return orchard
#' @export
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
