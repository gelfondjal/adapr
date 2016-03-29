#' Set up adapr 1st time
#' @details experimental
#' @export
#' 

default.adapr.setup <- function(){
  
  # Check git
  
  git_binary_path <- git_path(NULL)
  
  if(grepl("Git does not",git_binary_path)){ stop("Git is not installed Please download and configure (git-scm.com). Try GIT client GUI!!")}
  
  # Check git config  
  
  email <- ""
  
  try({
    email <- system2(git_binary_path, paste("config --global user.email"),stdout = TRUE)
  })
  
  if(!grepl("@",email)){
    
    print("Please enter Git User name")
    
    gituser <- readline("Please ender your preferred Git user name:    ")
    gitemail <- readline("Please ender your preferred Git email <somebody@somewhere.com>:   ")
    
    git.configure(gituser,gitemail)
    
    try({
      email <- system2(git_binary_path, paste("config --global user.email"),stdout = TRUE)
    })
    
    
    if (!grepl("@",email)) {return("Git is not configured. Run: git.configure(user.name, user.email)")}
    
  }else{print("Git configured")}
  
  orchards <- get_orchard()
  
  if(!("adaprHome" %in% orchards$project.id)){
    
    
    # Identify good spot for first project
    
    if(.Platform$OS.type == "unix"){
      topdirs <- list.files(path.expand.2("~"),full.names=TRUE)
      project.path <- file.path(grep("Documents$",topdirs,value=1)[1],"Projects")
    }
    
    if(.Platform$OS.type == "windows"){
      project.path <- file.path(path.expand.2("~"),"Projects")
    }
    
    
    publish.path <- file.path(project.path,"Publish")
    publish.path <- gsub("\\\\","/",publish.path)
    project.path <- gsub("\\\\","/",project.path)
    
    dir.create(project.path,recursive=TRUE)
    dir.create(publish.path,recursive=TRUE)
    
    project.path.start <- project.path#ifelse(dir.exists(project.path),project.path,"")
    publish.path.start <- publish.path#ifelse(dir.exists(publish.path),publish.path,"")
    
    
    first.project(project.path.start, publish.path.start)
    
    
  }else{
    
    print("ADAPRHOME project implies adpar already congifured")
    return("Already configured")
    
  }
  
  
  return("adapr setup: try 'shinyTree()'")
  
  
}# END default set.up

