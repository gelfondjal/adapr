#' initialize project
#' @param project.id Project name, if missing then default
#' @param project.path Project home directory, if missing then default
#' @param publish.directory Project branch exchange directory
#' @param first.program Name of first program in project (read_data.R default)
#' @return logical for success or not
#' @details Sets project
#' @examples 
#'\dontrun{
#' init.project("adaprTest")
#'} 
#' @details Wrapper for plant.tree
#' @export
init.project <- function(project.id,project.path=NA,publish.directory=NA,first.program="read_data.R"){
  
  out <- plant.tree(project.id,project.path,publish.directory,first.program )
  
  set.project(project.id)
  
}

#' initialize project
#' @param project.id Project name, if missing then default
#' @param project.path Project home directory, if missing then default
#' @param swap.directory Project branch exchange directory
#' @param first.program Name of first program in project (read_data.R default)
#' @return logical for success or not
#' @details Not for direct use. See init.project().
#' @export
plant.tree <- function(project.id,project.path=NA,swap.directory=NA,first.program="read_data.R"){
  
  opts <- get_adapr_options()
  
  if(is.na(project.path)){
    project.path <- opts$project.path
    swap.directory <- opts$publish.path
    }
  
  if(!dir.exists(project.path)|!dir.exists(swap.directory)){
  	
    
  	print("Project Directories invalid")
  	
  	if(!dir.exists(project.path)){print(paste("Project Directory doesn't exist",project.path))}
  	if(!dir.exists(swap.directory)){print(paste("Project Publish Directory doesn't exist",swap.directory))}
  	
  	return(FALSE)
  	
  }
  
  project.path <- file.path(project.path,project.id)
  swap.directory <- file.path(swap.directory,project.id)
  
  empty.orchard <- data.frame(project.id=project.id,project.path=project.path,swap.directory=swap.directory)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  if(!file.exists(orchard.site)){plant.orchard()}
  
  all.orchards <- get_orchard()
  
  if(project.id %in% all.orchards$project.id){
    
    print("Project Exists")
    
    return(FALSE)
  }else{
    

    orchards.old <- utils::read.csv(orchard.site,as.is=TRUE)
    
    utils::write.csv(rbind(orchards.old,empty.orchard),orchard.site,row.names=FALSE)
    
    dir.create(swap.directory)
    
    dir.create(project.path)
    
    dir.create(file.path(project.path,project.directory.tree$analysis))
    
    dir.create(file.path(project.path,project.directory.tree$data))
    
       
    sprout.program(project.id,source.file.name=NA,description="",seed=2011,capture.load.command="library(adapr)",controller=TRUE)
    
    test <- sprout.program(project.id,source.file.name=first.program,description="reads data",seed=2011,capture.load.command="library(adapr)",controller=FALSE)
    try({
    devtools::clean_source(file.path(project.path,project.directory.tree$analysis,first.program),quiet=TRUE) 
    })
     if(!test){
     
     # revert to old orchard if failure
     	
     	utils::write.csv(orchards.old,orchard.site,row.names=FALSE)
     	
     	
     }
    
    return(test)
    
  }
  
  
  
}

#' Changes project directory/publish directory or identifies imported project
#' @param project.id0 Project name
#' @param project.path Project home directory
#' @param swap.directory Project publish directory
#' @return logical for success or not
#' @details Is wrapper for redirect.tree. Does not move the project only indicates new location.
#' @examples 
#'\dontrun{
#' relocate.project("adaprTest","mydirectory1","mydirectory2publish")
#'} 
#' @export
relocate.project <- function(project.id0,project.path=NA,swap.directory=NA){
  
  out <- redirect.tree(project.id0,project.path,swap.directory)
  
  return(out)
  
}



#' Changes project directory/publish directory or identifies imported project
#' @param project.id0 Project name
#' @param project.path Project home directory
#' @param swap.directory Project publish directory
#' @return logical for success or not
#' @export
#' @details Not for direct use. See relocate.project
redirect.tree <- function(project.id0,project.path=NA,swap.directory=NA){
  
  opts <- get_adapr_options()
  
  # Set missing to default
  
  if(is.na(project.path)){
    project.path <- opts$project.path
  }
  
  if(is.na(swap.directory)){
    swap.directory <- opts$publish.path
  }
  
  
  
  project.path <- file.path(project.path,project.id0)
  swap.directory <- file.path(swap.directory,project.id0)
  
  dir.create(project.path)
  dir.create(swap.directory)
  
  if(!dir.exists(project.path)|!dir.exists(swap.directory)){
  	
  	print("Project Directories invalid")
  	
  	if(!dir.exists(project.path)){print(paste("Project Directory doesn't exist",project.path))}
  	if(!dir.exists(swap.directory)){print(paste("Project Publish Directory doesn't exist",swap.directory))}
  	
  	return(FALSE)
  	
  }

  empty.orchard <- data.frame(project.id=project.id0,project.path=project.path,swap.directory=swap.directory)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  if(!file.exists(orchard.site)){plant.orchard()}
  
  all.orchards <- get_orchard()
  
  if(project.id0 %in% all.orchards$project.id){
    
    print("Project Exists: Redirected to new path")
    
    all.orchards <- subset(all.orchards,all.orchards$project.id!=project.id0)
    
    
  }else{
  	    print(paste(project.id0,"Project Identified"))

  	}

  utils::write.csv(rbind(all.orchards,empty.orchard),orchard.site,row.names=FALSE)

  return(TRUE)

}






