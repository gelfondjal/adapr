#' Set up adapr 1st time
#' @details experimental
#' @export
#' 

default.adapr.setup <- function(){
  
  # check launch in Rstudio

   sysEnvironment <- Sys.getenv()

  rstudio <- ""
  try({
  	rstudio <- sysEnvironment[["RSTUDIO"]]
  })
  
  rstudio <- rstudio=="1"
  
  if(!rstudio){stop("Please start up first time in RStudio to identify pandoc resources.")}
  
  # check pandoc path
  
  PATHer <- sysEnvironment[["PATH"]]
  
  oldoptions <- get_adapr_options()
  
  oldpath <- oldoptions$PATH
  
  pandocpath <- sysEnvironment[["RSTUDIO_PANDOC"]]
  
  if(!grepl(pandocpath,oldpath,fixed=TRUE)){oldpath <- paste0(oldpath,.Platform$path.sep,pandocpath)}
  
  set_adapr_options("PATH",oldpath)
  
  # Check git
  
  git_binary_path <- git_path(NULL)
  
  if(grepl("Git does not",git_binary_path)){ 
  	
  	yesno <- readline("Do you want to use git y/n? (This is optional)")
  	
  	wantgit <- substring(yesno,1,1) %in% c("y","Y")
  	
  	
  	if(wantgit){
  	
  	stop("Git is not installed Please download and configure (git-scm.com). Try GIT client GUI!!")}
  	
  	}else{
  		
  		set_adapr_options("git","FALSE")
  		
  		}
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
  
  
  return("adapr setup: try 'adapr21()'")
  
  
}# END default set.up

