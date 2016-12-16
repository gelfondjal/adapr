#' Create source file directories 
#' @param project.id0 project id name string
#' @param source.file0 filename of the source
#' @param source.description character description of what the source file does 
#' @return source_info list describing the project
#' @details Intializes git for the project, adds program git tracking, and initializes dependency tracking
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#'} 
#' 
create_source_file_dir <- function(project.id0=get("project.id"),source.file0=get("source.file"),source.description=""){
  #equire(devtools)

  set.project(project.id0,TRUE)
  
    project.path <- get.project.path(project.id0)
  project.tree <- project.directory.tree
  
  
  analysis.dir <- file.path(project.path,project.tree$analysis) # where the programs are
  data.dir <- file.path(project.path,project.tree$data)  # where the data are
  results.dir <- file.path(project.path,project.tree$results,source.file0) # Standard output
  tex.dir <- file.path(results.dir,project.directory.tree$tex.dir) # Publication quality output
  dependency.dir <- file.path(project.path,project.tree$dependency.dir)
  support.dir <- file.path(project.path,project.tree$support)
  library.dir <- file.path(support.dir,project.tree$library.bank)
  source.support.dir <- file.path(support.dir,gsub("\\.(R|r)$","_R",source.file0))
  apps.dir <- file.path(support.dir,"Apps")
  markdown.dir <- file.path(analysis.dir,"Markdown")
  
  project.tree <- project.directory.tree
  
  if(!grepl("\\.R$|\\.r$",source.file0)){stop(paste("Project",project.id0,"Source file",source.file0,"not an R file extension"))}
  
  if(!file.exists(file.path(analysis.dir,source.file0))){stop(paste("Project",project.id0,"Source file",source.file0,"does not exist in program directory"))}
  
  # Create necessary directories
  
  apply(matrix(c(analysis.dir,data.dir,results.dir,tex.dir,dependency.dir,support.dir,library.dir,apps.dir,source.support.dir,markdown.dir   )),1,dir.create,showWarnings=FALSE,recursive=TRUE)
  
  source.file.info <- Create.file.info(analysis.dir,source.file0,description=source.description)	
  
  source_info <- list(analysis.dir=analysis.dir,data.dir=data.dir,tex.dir=tex.dir,results.dir=results.dir,support.dir = support.dir,library.dir=library.dir,
                      dependency.dir=dependency.dir,file=source.file.info,source.support.dir=source.support.dir,markdown.dir=markdown.dir,support.library.file="common_libs.csv")
  
  source_info$project.id <- project.id0
  source_info$project.path <- project.path	
  
  source_info$options <- get_adapr_options(TRUE)
  
  try({
    treedf <- Harvest.trees(dependency.dir)
    not.this.source <- subset(treedf,(treedf$source.file!=source_info$file[["file"]])&(!is.na(dependency)))
    if (nrow(not.this.source)){source_info$all.files<- Condense.file.info(not.this.source)}
  },silent=TRUE)
  
  source_info$dependency.file <- paste(source.file.info[2],".txt",sep="")	
  
  source_info$git.path <- source_info$options$git.path
  
  source_info$git.log <- ifelse(is.null(source_info$options$git),TRUE,source_info$options$git=="TRUE")		
  
  # create depedency ref class instance
  
  source_info$dependency <- dependency(data= data.frame())
  
  source_info$options$git <- ifelse(is.null(source_info$options$git),TRUE,source_info$options$git=="TRUE")
  
  initialize_dependency_info(source_info)
  
  #Start html markup tracking
  
    
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

  source_info$rmdfile <- create_markdown(target.file= targetfile,target.dir=targetdir,style="html_document",description=source_info$file$description,source_info)
  
  # Create publication file list
  
   publication.file <- file.path(source_info$support.dir,"files_to_publish.csv")
  
  if(!file.exists(publication.file)){
    print("No publication list file")
    publication.stub <- data.frame(Path="",Description="")[-1,]
    utils::write.csv(publication.stub,publication.file,row.names=FALSE)
  }
  
  
  
  #print(source_info)
  
  return(source_info)
  
}	
