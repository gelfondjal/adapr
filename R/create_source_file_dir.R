#' Create source file directories 
#' @param project.id is the project id name string
#' @param source.file is the filename of the source
#' @param description is the string description of what the source file does 
#' @param project.path is the home directory of the project
#' @param git.path path to git version control command
#' @return source_info list describing the project
#' @details Intializes git for the project, adds program git tracking, and initializes dependency tracking
#' @export
#' 
create_source_file_dir <- function(project.id="",source.file,source.description="",project.path=get.project.path(project.id),
                                   
                                   git.path=NULL,git.log = TRUE){
  require(devtools)
  require(plyr)
  require(igraph)
  require(digest)
  require(rCharts)
  require(pander)
  
  
  analysis.dir <- file.path(project.path,project.tree$analysis) # where the programs are
  data.dir <- file.path(project.path,project.tree$data)  # where the data are
  results.dir <- file.path(project.path,project.tree$results,source.file) # Standard output
  tex.dir <- file.path(results.dir,project.directory.tree$tex.dir) # Publication quality output
  dependency.dir <- file.path(project.path,project.tree$dependency.dir)
  support.dir <- file.path(project.path,project.tree$support)
  library.dir <- file.path(support.dir,project.tree$library.bank)
  source.support.dir <- file.path(support.dir,source.file)
  apps.dir <- file.path(support.dir,"Apps")
  
  project.tree <- project.directory.tree
  
  # Create necessary directories
  
  apply(matrix(c(analysis.dir,data.dir,results.dir,tex.dir,dependency.dir,support.dir,library.dir,apps.dir,source.library.dir   )),1,dir.create,showWarnings=FALSE,recursive=TRUE)
  
  source.file.info <- Create.file.info(analysis.dir,source.file,description=source.description)	
  
  source_info <- list(analysis.dir=analysis.dir,data.dir=data.dir,tex.dir=tex.dir,results.dir=results.dir,support.dir = support.dir,library.dir=library.dir,
                      dependency.dir=dependency.dir,file=source.file.info,source.support.dir=source.support.dir,support.library.file="common_libs.csv")
  
  source_info$project.id <- project.id
  source_info$project.path <- project.path	
  
  try({
    not.this.source <- subset(Harvest.trees(dependency.dir),(source.file!=source_info$file[["file"]])&(!is.na(dependency)))
    if (nrow(not.this.source)){source_info$all.files<- Condense.file.info(not.this.source)}
  },silent=TRUE)
  
  source_info$dependency.file <- paste(source.file.info[2],".txt",sep="")	
  
  source_info$git.path <- git.path
  
  source_info$git.log <- git.log		
  
  # create depedency ref class instance
  
  source_info$dependency <- dependency(data= data.frame())
  
  initialize_dependency_info(source_info)
  
  #Start html markup tracking
  
  if(source_info$option$pandoc){
 
  panderOptions("table.split.table",Inf)
  evalsOptions("cache.dir",source_info$tex.dir)
  setwd(source_info$tex.dir)
  source_info$report <- Pandoc$new()
  source_info$pandoc <- FALSE
  author <- ""
  
  if(source_info$option$git){
  
  try({
    git_binary_path <- git_path(NULL)
    author  <- system2(git_binary_path, paste("config --global user.name"),stdout=TRUE)
  })
  
  }# if git
  
  #try(pandocinstalled <- source_info$report$export("tester",open=FALSE))
  if(!source_info$option$pandoc){
    return("Warning: Pandoc is not installed on this computer")}else {
      source_info$pandoc <- TRUE
      source_info$report$title <- paste(source_info$project.id,source.file)
      source_info$report$author <-paste("ADAPR",author)
      source_info$report$add.paragraph(paste("Script description:",source.description))
      # Remove leftover plots      
      file.remove(list.files(file.path(source_info$tex.dir,"plots"),full.names=TRUE))
      
    }
    
    
    }#if pandoc
  return(source_info)
  
}	
