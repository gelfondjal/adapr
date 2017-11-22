#' Generates the shell of a R function that is project specific in support_functions folder
#' @param functionName character string for R function.
#' @param description character string description of function
#' @param project.id Name of project
#' @return Logical indicating success or not
#' @details Function file will add ".R" extension. Will not overwrite existing program.See makeScript() for making an R script.
#' 
makeFunction <- function(functionName=NA,description="",project.id=getProject()){
  
  if(is.na(functionName)){stop("makeFunction error function name unspecified.")}
  
  functionNameR <- paste0(gsub("\\.","_",make.names(functionName)),".R")
  
  adapr_options <- getAdaprOptions()
  if(adapr_options$git=="TRUE"){
    author <- git2r::config()[["global"]]$user.name
  }else{author <- adapr_options$username}
  if(length(author)==0){author <- "Anonymous"}
  
  start.lines.generic <- c(paste0("# Project ID:  ",project.id),paste0("# Function:  ",functionName),paste0("# Author:  ",author))
  
  start.lines.specific <- c("#",paste0("# Description:  ",description),"#")
  
  initialize.lines <- paste0(make.names(functionName)," <- function(){")
  
  final.line <- paste("}# END function:",make.names(functionName))
  
  strings.to.write <- c(rep("\n",1),start.lines.generic,rep("\n",1),initialize.lines,start.lines.specific,rep("\n",3),final.line)
  
  #print(strings.to.write)
  
  target.file <- file.path(getProjectPath(project.id),project.directory.tree$support,functionNameR)
  
  if(!file.exists(target.file)){
    
    dir.create(file.path(getProjectPath(project.id),project.directory.tree$support),showWarnings=FALSE)

    write(strings.to.write,target.file)
    
    utils::browseURL(target.file)
    
    return(TRUE)
  }
  
  
  
  return(FALSE)
  
}