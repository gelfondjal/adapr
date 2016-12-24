#' Generates the shell of a code that is project specific
#' @param project.id Name of project
#' @param source.file.name Filename to create
#' @param description What program does
#' @param seed Set seed at program initialization
#' @param capture.load.command Command for loading inference tree library
#' @param controller logical to insert lines that operate on analysis tree
#' @return Logical indicating success or not
#' @details Will not overwrite existing program. Not for direct use. See make.program().
#' @export
#' 
sprout.program <- function(project.id=NA,source.file.name=NA,description="",seed=2011,capture.load.command="library(adapr)",controller=FALSE){
  
  
  if(controller){
    source.file.name <- "tree_controller.R"
    description <- "Operates on analysis tree"
  }
  
  start.lines.generic <- c("rm(list=ls())",paste("set.seed(",seed,")"),"",
                           capture.load.command)
  
  start.lines.specific <- c(paste0("source.file <-","\"",source.file.name,"\""),paste0("project.id <- \"",project.id,"\""))
  
  
  initialize.lines <- paste0("source_info <- create_source_file_dir(","source.description=",paste0("\"",description,"\")"))
  
  body.lines <- c(rep("\n",1),"# Program body here",rep("\n",2),"# End Program Body",rep("\n",1))
  
  final.line <- "dependency.out <- finalize_dependency()"
  

  
  controller.lines <- c( "#synctest.project()     #Tests project synchronization ",
                     "#sync.project()  # This runs all programs needed to synchronize",
                       "#report.project()              #This summarizes project in html")
  
  if(controller){final.line <- controller.lines}
  
  strings.to.write <- c(rep("\n",1),start.lines.generic,rep("\n",1),start.lines.specific,initialize.lines,body.lines,final.line)

  #print(strings.to.write)
  
  target.file <- file.path(get.project.path(project.id),project.directory.tree$analysis,source.file.name)
  
  if(!file.exists(target.file)){
    
    dir.create(file.path(get.project.path(project.id),project.directory.tree$analysis),showWarnings=FALSE)
    

    
    write(strings.to.write,target.file)
    return(TRUE)
  }
  
  
  
  return(FALSE)
  
}



#' Generates the shell of a code that is project specific
#' @param project.id Name of project
#' @param r is source file name or Filename to create
#' @param description What program does
#' @param seed Random start seed
#' @param run Execute r script?
#' @return Logical indicating failure or not
#' @details Will not overwrite existing program. Executes program stub. Mostly wrapper for sprout.program.
#' @export
#' @examples 
#'\dontrun{
#'  make.program("adaprHome","read_data.R")
#'} 
make.program <- function(project.id=get.project(),r="",description="",seed=2011,run=TRUE){
  
  if(!(toupper(gsub(".*\\.","",r))=="R")){
    
    print("Error make.program: Scripname doesn't end in .R")
    
    return(FALSE)
    
  }
  
  out <- sprout.program(project.id,source.file.name=r,description=,seed)
  
  if(run){run.program(project.id,r)}

  return(out)
}

