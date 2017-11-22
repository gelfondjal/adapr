#' Lower level function that generates the shell of a code that is project specific
#' @param project.id Name of project
#' @param source.file.name Filename to create
#' @param description What program does
#' @param seed Set seed at program initialization
#' @param capture.load.command Command for loading inference tree library
#' @param controller logical to insert lines that operate on analysis tree
#' @return Logical indicating success or not
#' @details Will not overwrite existing program. Not for direct use. See makeScript().
#' 
sproutProgram <- function(project.id=NA,source.file.name=NA,description="",seed=2011,capture.load.command="library(\"adapr\")",controller=FALSE){
  
  
  if(controller){
    source.file.name <- "tree_controller.R"
    description <- "Operates on analysis tree"
  }
  
  start.lines.generic <- c("rm(list=ls())",paste("set.seed(",seed,")"),"",
                           capture.load.command)
  
  start.lines.specific <- c(paste0("source.file <-","\"",source.file.name,"\""),paste0("project.id <- \"",project.id,"\""))
  
  
  initialize.lines <- paste0("source_info <- create_source_file_dir(","source.description=",paste0("\"",description,"\")"))
  
  body.lines <- c(rep("\n",1),"#Library statements here",rep("\n",2),"# Program body here",rep("\n",2),"# End Program Body",rep("\n",1))
  
  final.line <- "dependency.out <- finalize_dependency()"
  
  
  controller.lines <- c( "#syncTestProject()     #Tests project synchronization ",
                     "#syncProject()  # This runs all programs needed to synchronize",
                       "#reportProject()              #This summarizes project in html")
  
  if(controller){final.line <- controller.lines}
  
  strings.to.write <- c(rep("\n",1),start.lines.generic,rep("\n",1),start.lines.specific,initialize.lines,body.lines,final.line)
  #print(strings.to.write)
  
  target.file <- file.path(getProjectPath(project.id),project.directory.tree$analysis,source.file.name)
  
  if(!file.exists(target.file)){
    
    dir.create(file.path(getProjectPath(project.id),project.directory.tree$analysis),showWarnings=FALSE)
    
    
    write(strings.to.write,target.file)
    return(TRUE)
  }
  
  
  
  return(FALSE)
  
}



#' Generates the shell of a code that is project specific
#' @param r is source file name or Filename to create
#' @param description Character string describing what program does
#' @param project.id Character string for name of project
#' @param seed Random start seed
#' @param run Logical for execution of r script
#' @param openTF Logcial for opening R script
#' @return Logical indicating failure or not
#' @details Will not overwrite existing program. Executes program and opens stub program. Mostly wrapper for sproutProgram.
#' @export
#' @examples 
#'\dontrun{
#'  makeScript("read_data.R",description="reads data","adaprHome")
#'} 
makeScript <- function(r="",description="",project.id=getProject(),seed=2011,run=TRUE,openTF=TRUE){
  
  
  r <- gsub(" ","_",r)
  
  if(!(toupper(gsub(".*\\.","",r))=="R")){
    
    print("Error makeScript: Script name doesn't end in .R")
    
    return(FALSE)
    
  }
  
  out <- sproutProgram(project.id,source.file.name=r,description=description,seed)
  
  if(run){
    runScript(r,project.id)
    
  }
  
  if(openTF){
 
    openScript(r,project.id)
    
  }
  
  return(out)
}








