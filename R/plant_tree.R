#' initialize project
#' @param project.id Project name
#' @param project.path Project home directory
#' @param swap.directory Project branch exchange directory
#' @return logical for success or not
#' @export
plant.tree <- function(project.id,project.path,swap.directory){
  
  project.path <- file.path(project.path,project.id)
  swap.directory <- file.path(swap.directory,project.id)
  
  empty.orchard <- data.frame(project.id=project.id,project.path=project.path,swap.directory=swap.directory)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv")	
  
  if(!file.exists(orchard.site)){plant.orchard()}
  
  all.orchards <- read.csv(file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory.csv"),as.is=TRUE)
  
  if(project.id %in% all.orchards$project.id){
    
    print("Project Exists")
    
    return(FALSE)
  }else{
    
    
    orchards.old <- read.csv(orchard.site,as.is=TRUE)
    
   
    dir.create(swap.directory)
    
    dir.create(project.path)
    
    dir.create(file.path(project.path,project.directory.tree$analysis))
    
    dir.create(file.path(project.path,project.directory.tree$data))
    
    write.csv(rbind(orchards.old,empty.orchard),orchard.site,row.names=FALSE)
    
    
    sprout.program(project.id,source.file.name=NA,description="",seed=2011,capture.load.command="library(IT2)",controller=TRUE)
    
    return(TRUE)
    
  }
  
  
  
}

