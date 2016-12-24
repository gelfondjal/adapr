#' Create project hub files in root directory
#' @return logical for succesful creation or not
#' @export
#' @examples 
#'\dontrun{
#'  plant.orchard()
#'} 
#'
plant.orchard <- function(){
  
  # 
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  if(!file.exists(orchard.site)){
    
    dir.create(file.path(path.expand.2("~"),"ProjectPaths"))
    
    empty.orchard <- data.frame(project.id="",project.path="",swap.directory="")[-1,]	
    
    utils::write.csv(empty.orchard,orchard.site,row.names=FALSE)
    
    
    return(TRUE)
  }
  
  return(FALSE)
}


#' Create first project
#' @param project.path Path where first project will go
#' @param publish.path Path to share project results
#' @return logical for succesful creation or not
#' @export
#' @examples 
#'\dontrun{
#'  opt <- get_adapr_options()
#'  first.project(opt$project.path,opt$publish.path)
#'} 
#' 
first.project <- function(project.path="",publish.path=""){
#project.path <- "/Users/Gelfond/Documents/Projects"
#publish.path <- "/Users/Gelfond/Documents/Projects/Swap"
  orchard.site <- get_orchard()	
  
  project.id <- "adaprHome"
  
  if(is.null(orchard.site)){
  	
  	# Create orchard
  	
  	plant.orchard()
 
  }
 
  
  testout <- plant.tree(project.id,project.path,publish.path)   
  
  return(testout)
}



