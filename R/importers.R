#' Imports new project using adapr import file or by selecting any R script within the project
#' @param importer.file File created by adapr::makeImporter that is selected interactively OR any R script within program directory
#' @param Rscript logical indicating whether importer.file parameter is a "adapr_importer.R" file OR any R script within program directory.
#' @return Logical indicating success or not
#' @export
#' @details This is a console command. The function adapr::makeImproter will create the "adapr_importer.R" file needed. Function will create an interactive file choosing window.
#' @examples 
#'\dontrun{
#' importProject() # Will open window for file selection.
#'} 
importProject <- function(importer.file = NULL,Rscript=FALSE){
  
  
  if(is.null(importer.file)){
    
    yn <- readline("Import new project into adapr? y or n")
    if(tolower(yn)!="y"){stop("importProject fail. User did not enter y for yes")}
    
    if(!Rscript){
      print("Browse to project folder and select the 'adapr_importer.R' file within the Project directory:  [Project to Import]/adapr_importer.R")
    }else{
      print("Browse to project folder and any R Script '.R' file within the Project's Programs directory:  [Project to Import]/Programs/*.R")
    }
    print("An interactive file select window should pop up.")
    
    importer.file <- file.choose()
    
  }
  
  if(!Rscript){
    sourceOut <- source(importer.file)
    project.id <- basename(dirname(importer.file))
  }else{
    project.path0 <- dirname(dirname(dirname(importer.file)))
    project.id <- basename(dirname(dirname(importer.file)))			
    relocateProject(project.id0 = project.id, project.path= project.path0)
  }
  
  
  test <- FALSE
  try({
    setProject(project.id)
    plotter <- graphProject(testSync=FALSE)
    print(plotter)
    test <- TRUE	
    
  })
  message <- ifelse(test,paste(project.id,"Successfully imported"),paste("Import failed for project",project.id))
  return(message)
  
}


#' Creates an "adapr_importer.R" script that allows another user to import the project with 
#' the "importProject" function.
#' @param project.id name of project.
#' @return output form base::write about success of file creation.
#' @export
#' @details This is a console command. The function adapr::makeImproter will create the "adapr_importer.R" file needed by adapr::importProject fuction.
#' @examples 
#'\dontrun{
#' makeImporter() # Will create file called "adapr_importer.R" in top level project directory.
#'} 

makeImporter <- function(project.id = getProject()){
  #
  # Description:  imports adapr project
  #
  
  programPath <- file.path(getProjectPath(project.id),"adapr_importer.R")
  
  importCommands <- c("# This allows adapr to recognize a new project.","#Select this file with adapr::importProject() function or use base::source('adapr_importer.R').",
                      
                      "project.path0 <- dirname(dirname(parent.frame(2)$ofile))",
                      paste("relocateProject(project.id0=",paste0("\'",project.id,"\'"),
                            ", project.path= project.path0)")
  )
  
  out <- write(importCommands,programPath,sep="\n")					
  
  return(out)
  
}
