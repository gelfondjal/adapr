#' Create source file directories 
#' @param project.id0 project id name string
#' @param source.file0 filename of the source
#' @param source.description character description of what the source file does 
#' @return source_info list describing the project
#' @details Intializes git for the project, adds program git tracking, creates project library and initializes dependency tracking.
#' Creates directories for Project and Results. Initialize the file tracking object source_info.
#' Gathers file information on all project files.Initialize Git for project (if using Git).
#' Adds dependencies files to Git
#' Initialized archivist  repo for script 
#' Run all R scripts in support_function directory 
#' Run all R scripts in script specific support_function/myRscript.R directory 
#' Create R markdown file with same file prefix (if not already done).
#' Create publication file (if not already done). 
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#'} 
#' 
create_source_file_dir <- function(project.id0=get("project.id"),source.file0=get("source.file"),source.description=""){
  #equire(devtools)
  setProject(project.id0,TRUE)
  
  project.path <- getProjectPath(project.id0)
  project.tree <- project.directory.tree
  
  
  analysis.dir <- file.path(project.path,project.tree$analysis) # where the programs are
  data.dir <- file.path(project.path,project.tree$data)  # where the data are
  results.dir <- file.path(project.path,project.tree$results,source.file0) # Standard output
  archivist.dir <- file.path(results.dir,"archivist")
  tex.dir <- file.path(results.dir,project.directory.tree$tex.dir) # Publication quality output
  dependency.dir <- file.path(project.path,project.tree$dependency.dir) #where dependency files are stored
  support.dir <- file.path(project.path,project.tree$support) # where functions and libraries are stored
  library.dir <- file.path(support.dir,project.tree$library.bank) # where library is stored
  source.support.dir <- file.path(support.dir,gsub("\\.(R|r)$","_R",source.file0)) # where source specific libraries are stored
  apps.dir <- file.path(support.dir,"Apps") # where project apps are stored
  markdown.dir <- file.path(analysis.dir,"Markdown")  # where the markdown directory is
  
  project.tree <- project.directory.tree
  
  if(!grepl("\\.R$|\\.r$",source.file0)){stop(paste("Project",project.id0,"Source file",source.file0,"not an R file extension"))}
  
  if(!file.exists(file.path(analysis.dir,source.file0))){stop(paste("Project",project.id0,"Source file",source.file0,"does not exist in program directory"))}
  
  # Create necessary directories
  
  apply(matrix(c(analysis.dir,data.dir,results.dir,tex.dir,dependency.dir,support.dir,library.dir,apps.dir,source.support.dir,markdown.dir,archivist.dir   )),1,dir.create,showWarnings=FALSE,recursive=TRUE)
  
  print("Created Script directories")
  
  suppressWarnings(archivist::createLocalRepo(archivist.dir))
  
  source.file.info <- createFileInfo(analysis.dir,source.file0,description=source.description)	
  
  source_info <- list(analysis.dir=analysis.dir,data.dir=data.dir,tex.dir=tex.dir,results.dir=results.dir,support.dir = support.dir,library.dir=library.dir,
                      dependency.dir=dependency.dir,file=source.file.info,source.support.dir=source.support.dir,markdown.dir=markdown.dir,support.library.file="common_libs.csv")
  
  source_info$project.id <- project.id0
  source_info$project.path <- project.path	
  
  source_info$options <- getAdaprOptions(TRUE)
  
  try({
    treedf <- readDependency(dependency.dir)
    not.this.source <- subset(treedf,(treedf$source.file!=source_info$file[["file"]])&(!is.na(dependency)))
    if (nrow(not.this.source)){source_info$all.files<- condenseFileInfo(not.this.source)}
  },silent=TRUE)
  
  source_info$dependency.file <- paste(source.file.info[2],".txt",sep="")	
  
  source_info$git.path <- source_info$options$git.path
  
  source_info$git.log <- ifelse(is.null(source_info$options$git),TRUE,source_info$options$git=="TRUE")		
  
  # create depedency ref class instance
  
  source_info$dependency <- dependency(data= data.frame())
  
  source_info$options$git <- ifelse(is.null(source_info$options$git),TRUE,source_info$options$git=="TRUE")
  
  options(adaprScriptInfo = source_info) 
  
  initialize_dependency_info(source_info)
  
  #Start html r markdown tracking
  
    
  if(source_info$options$git){
  
  try({
    #git_binary_path <- git_path(NULL)
    #author  <- system2(git_binary_path, paste("config --global user.name"),stdout=TRUE)
    
    author <- git2r::config()$global$user.name
  })
  
  }# if git
  
  # Create markdown partner
  
  targetfile <- paste0(source_info$file$file,"md")
  targetdir <- source_info$markdown.dir
  source_info$rmdfile <- createMarkdown(target.file= targetfile,target.dir=targetdir,style="html_document",description=source_info$file$description,source_info)
  
  # Create publication file list
  
   publication.file <- file.path(source_info$support.dir,"files_to_publish.csv")
  
  if(!file.exists(publication.file)){
    print("No publication list file")
    publication.stub <- data.frame(Path="",Description="")[-1,]
    utils::write.csv(publication.stub,publication.file,row.names=FALSE)
  }
  
  
  
  #print(source_info)
  
  options(adaprScriptInfo = source_info) 
   
  return(source_info)
  
}	
