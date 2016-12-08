#' Set up adapr 1st time
#' @details Use on ADAPR start up. Requires RStudio to work. Will make project directories in computer Document directory and create adaprHome project.
#' @export
#' @examples 
#'\dontrun{
#' # Requires RStudio
#'default.adapr.setup()
#'
#'} 
#'

default.adapr.setup <- function(){
  
  # check launch in Rstudio
  
  total <- 6
  step <- 1
  #equire(devtools)

  
  print("Will make project directories in computer Document directory and create adaprHome project.")
  
  
  print(paste("Step",step,"of",total,"Identifying RSTUDIO step"))
  step <- step + 1

   sysEnvironment <- Sys.getenv()

  rstudio <- ""
  try({
  	rstudio <- sysEnvironment[["RSTUDIO"]]
  })
  
  rstudio <- rstudio=="1"
  
  if(!rstudio){stop("Please start up first time in RStudio to identify pandoc resources.")}
    
  # check pandoc path
  
  print(paste("Step",step,"of",total,"Check pandoc path"))
  step <- step + 1
  
  PATHer <- sysEnvironment[["PATH"]]
  
  oldoptions <- get_adapr_options()
  
  oldpath <- oldoptions$PATH
  
  pandocpath <- sysEnvironment[["RSTUDIO_PANDOC"]]
  
  if(length(oldpath)==0){oldpath <- PATHer}
    
  if(!grepl(pandocpath,oldpath,fixed=TRUE)){oldpath <- paste0(oldpath,.Platform$path.sep,pandocpath)}
  
  set_adapr_options("PATH",oldpath)
  
  # Check git
  
  print(paste("Step",step,"of",total,"Check git version control"))
  step <- step + 1

  git_binary_path <- "Git does not"
  
  try(git_binary_path <- git_path(NULL))
  
  yesno <- readline("Do you want to use git y/n? (This is optional)")
  
  wantgit <- substring(yesno,1,1) %in% c("y","Y")
  
  set_adapr_options("git",ifelse(wantgit,"TRUE","FALSE"))
  
  if(wantgit==FALSE){
    username <- readline("What is your username? (This is optional)")
    set_adapr_options("username",ifelse(username=="","Anonymous",username))
  }
  
  if(wantgit & grepl("Git does not",git_binary_path)){ 
  	

  	  stop("Git is not installed Please download and configure (git-scm.com). Try GIT client GUI!!")
  	
  	}
  # Check git config  
  
  if(wantgit & !grepl("Git does not",git_binary_path)){
  
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
  
  }# configure git
  
  orchards <- get_orchard()
  
  print(paste("Step",step,"of",total,"Creating 1st project adaprHome"))
  step <- step + 1
  
  
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
    
    print(paste("Step",step,"of",total,"Set default project paths"))
    step <- step + 1
    
    set_adapr_options("project.path",project.path.start)
    
    set_adapr_options("publish.path", publish.path.start)
    
    first.project(project.path.start, publish.path.start)
    
    
  }else{
    
    print("adaprHome project implies adpar already congifured")
    return("Already configured")
    
  }
  
  print(paste("Step",step,"of",total,"Completed default seup!"))
  step <- step + 1
  
  return("adapr setup: try 'adaprApp()'")
  
  
}# END default set.up

