#' Generates the shell of a code that is project specific
#' @param project.id Name of project
#' @param source.file.name Filename to create
#' @param description What program does
#' @param seed Set seed at program initialization
#' @param capture.load.command Command for loading inference tree library
#' @return Logical indicating success or not
#' @details Will not overwrite existing program
#' @export
#' 
sprout.program <- function(project.id=NA,source.file.name=NA,description="",seed=2011,capture.load.command="library(IT2)",controller=FALSE){
  
  
  if(controller){
    source.file.name <- "tree_controller.R"
    description <- "Operates on analysis tree"
  }
  
  start.lines.generic <- c("rm(list=ls())",paste("set.seed(",seed,")"),"",
                           capture.load.command)
  
  start.lines.specific <- c(paste0("source.file <-","\"",source.file.name,"\""),paste0("project.id <- \"",project.id,"\""))
  
  
  initialize.lines <- paste0("source_info <- create.source.file.dir(project.id=project.id,source.file=source.file,get.project.path(project.id),","\n",
                             "source.description=",paste0("\"",description,"\")"))
  
  body.lines <- c(rep("\n",1),"# Program body here",rep("\n",2),"# End Program Body",rep("\n",1))
  
  final.line <- "dependency.out <- finalize.dependency.si(source_info,commit=\"\",effort.hours=0)"
  

  
  controller.lines <- c( "#sync.test.si(source_info)     #Tests project synchronization ",
                     "#source.sync.si(source_info,run=TRUE) # This runs all programs needed to synchronize",
                       "#project.report()               #This summarizes project in html",
                         "#sprout.program(source_info$project.id,source.file.name=\"myprog.R\",\"Describe myprog\") #makes new program",
                         "#send.branch(branch_cut,all=FALSE) #this packages a analytical branch ans sends to swap directory",
                       "#graft.branch(branch_name,run=TRUE,start.up=FALSE,project.id=NULL,overwriteTF=FALSE) #This loads and runs branch")
  
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
