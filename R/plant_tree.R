#' initialize project
#' @param project.id Project name, if missing then default
#' @param project.path Project home directory, if missing then default
#' @param publish.directory Project branch exchange directory
#' @param first.program Name of first program in project (read_data.R default)
#' @param project.libraryTF character string "packrat","TRUE","FALSE" for using packrat package or Logical to use a local (not default) library
#' @param library.path  Path to local (not default) library
#' @return logical for success or not
#' @details Sets up project for first time. Defaults to main library. If using a local library, then leaving library path equal to "" puts the library within the project folder.
#' @examples 
#'\dontrun{
#' initProject("adaprTest")
#'} 
#' @details Wrapper for plantTree
#' @export
initProject <- function(project.id,project.path=NA,publish.directory=NA,first.program="read_data.R",project.libraryTF=FALSE,
                        library.path=""){
  
  project.libraryTF <-ifelse(!is.na(as.logical(project.libraryTF)),as.logical(project.libraryTF),ifelse(project.libraryTF=="packrat",
                                                                                                                                "packrat","FALSE"))
  project.libraryTF <- as.character(project.libraryTF)
  
  out <- plantTree(project.id,project.path,publish.directory,first.program,project.libraryTF,library.path)
  
  setProject(project.id)
  
}
#init.project <- initProject
#' initialize project
#' @param project.id Project name, if missing then default
#' @param project.path Project home directory, if missing then default
#' @param swap.directory Project branch exchange directory
#' @param first.program Name of first program in project (read_data.R default)
#' @param project.libraryTF Logical to use a local (not default) library
#' @param library.path = path to local (not default) library
#' @return logical for success or not
#' @details Not for direct use. See initProject().
#' @export
plantTree <- function(project.id,project.path=NA,swap.directory=NA,first.program="read_data.R",project.libraryTF=FALSE,
                      library.path=""){
  
  opts <- getAdaprOptions()
  
  if(is.na(project.path)){
    project.path <- opts$project.path
  }
  
  if(is.na(swap.directory)){
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
  
  empty.orchard <- data.frame(project.id=project.id,project.path=project.path,swap.directory=swap.directory,
                              project.libraryTF=project.libraryTF,library.path=library.path)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  if(!file.exists(orchard.site)){plantOrchard()}
  
  all.orchards <- get_orchard()
  
  if(project.id %in% all.orchards$project.id){
    
    print("Project Exists")
    
    return(FALSE)
  }else{
    
    orchards.old <- get_orchard()
    
    utils::write.csv(rbind(orchards.old,empty.orchard),orchard.site,row.names=FALSE)
    
    dir.create(swap.directory)
    
    dir.create(project.path)
    
    dir.create(file.path(project.path,project.directory.tree$analysis))
    
    dir.create(file.path(project.path,project.directory.tree$data))
    
    if(project.libraryTF){setProject(project.id)}
       
    sproutProgram(project.id,source.file.name=NA,description="",seed=2011,capture.load.command="library(\"adapr\")",controller=TRUE)
    
    test <- sproutProgram(project.id,source.file.name=first.program,description="reads data",seed=2011,capture.load.command="library(\"adapr\")",controller=FALSE)
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
#' changes project directory/publish directory/library locataion or identifies imported project
#' @param project.id0 Project name
#' @param project.path Project home directory
#' @param swap.directory Project publish directory
#' @param project.libraryTF Logical to use a local (not default) library
#' @param library.path = path to local (not default) library
#' @return logical for success or not
#' @details Is wrapper for redirectTree. Does not move the project only indicates new location.
#' @examples 
#'\dontrun{
#' relocateProject("adaprTest","mydirectory1","mydirectory2publish")
#'} 
#' @export
relocateProject <- function(project.id0,project.path=NA,swap.directory=NA,project.libraryTF=FALSE,
                            library.path=""){
  
  out <- redirectTree(project.id0,project.path,swap.directory,project.libraryTF,library.path)
  
  setProject(project.id0)
  
  return(out)
  
}


#' Lower level function that that changes project directory/publish directory or identifies imported project
#' @param project.id0 Project name
#' @param project.path Project Parent directory (Directory that contains project)
#' @param swap.directory Project publish directory
#' @param project.libraryTF Logical to use a local (not default) library
#' @param library.path = path to local (not default) library
#' @return logical for success or not
#' @details Not for direct use. See relocate.project
redirectTree <- function(project.id0,project.path=NA,swap.directory=NA,project.libraryTF=FALSE,
                         library.path=""){
  
  opts <- getAdaprOptions()
  
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
  empty.orchard <- data.frame(project.id=project.id0,project.path=project.path,swap.directory=swap.directory,
                              project.libraryTF=project.libraryTF,library.path=library.path)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")	
  
  if(!file.exists(orchard.site)){plantOrchard()}
  
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
