#' Set up adapr 1st time
#' @details Use on ADAPR start up. Requires RStudio to work. Will make project directories in computer Document directory and create adaprHome project.
#' @export
#' @examples 
#'\dontrun{
#' # Requires pandoc location or RStudio
#' default.adapr.setup()
#'
#'} 
#'
defaultAdaprSetup <- function(){
  
  # check launch in Rstudio
  
  total <- 6
  step <- 1
  #equire(devtools)



  print("Will make project directories in computer Document directory and create adaprHome project.")
  
  print("Easier setup in RStudio.")
  
  print("For custom setup in non-default directories, use R Profile and adaprHomeDir R options: See adaprHomeDir().")
  
  yesno <- readline("Do want to setup in default directories? y/n")
  
  defaultSetUp <- substring(yesno,1,1) %in% c("y","Y")
  
  if(!defaultSetUp){
    print(paste("AdaprHomeDir is",adaprHomeDir()))
    yesno <- readline("Did you already set up adaprHomeDir option in R profile? y/n")
    defaultSetUp2 <- substring(yesno,1,1) %in% c("y","Y")
    if(!defaultSetUp2){stop("Set option adaprHomeDir in R Profile first.")}
    }
  
  print(paste("Step",step,"of",total,"Identifying RSTUDIO step"))
  step <- step + 1
   sysEnvironment <- Sys.getenv()
  rstudio <- ""
  try({
  	rstudio <- sysEnvironment[["RSTUDIO"]]
  })
  
  rstudio <- rstudio=="1"
  
  print(paste("Step",step,"of",total,"Check pandoc path"))
  step <- step + 1
  
  if(!rstudio){
    #print("Please start up first time in RStudio to identify pandoc resources.")
    print("RStudio not used to identify path to pandoc")
    }
    
  # check pandoc path
  
 
  PATHer <- sysEnvironment[["PATH"]]
  
  oldoptions <- getAdaprOptions()
  
  oldpath <- oldoptions$PATH
  if(!rstudio){
    
    yesno <- readline("Do you know the path to pandoc resource? y/n")
    
    foundPandoc <- substring(yesno,1,1) %in% c("y","Y")
    if(foundPandoc) {
      pandocpath <- readline("Please enter system path to pandoc resource.")
      }else{
        stop("Need pandoc resource path. Setup in RStudio identifies pandoc path.")
      }
  }else{
    pandocpath <- sysEnvironment[["RSTUDIO_PANDOC"]]
  }
  if(length(oldpath)==0){oldpath <- PATHer}
    
  if(!grepl(pandocpath,oldpath,fixed=TRUE)){oldpath <- paste0(oldpath,.Platform$path.sep,pandocpath)}
  
  setAdaprOptions("PATH",oldpath)
  

  
  orchards <- get_orchard()
  
  libdirectory <- readline("R library location? (leave blank if default)")
  
  libdirectory <- ifelse(libdirectory=="",.libPaths()[1],libdirectory)
  
  setAdaprOptions("library",libdirectory)

  
  
    
 
 
  yesno <- readline("Do you want to use adapr beta features (adapr installed from github) y/n?")
  
  adaprBeta <- substring(yesno,1,1) %in% c("y","Y")
  
  setAdaprOptions("adaprBeta",as.character(adaprBeta))
  
  if(!checkVersion("adapr",lib=libdirectory)){adaprInstall(libdirectory)}
  
  # Check git
  
  print(paste("Step",step,"of",total,"Check git version control"))
  step <- step + 1
  git_binary_path <- "Git does not"
  
  try(git_binary_path <- git_path(NULL))
  
  yesno <- readline("Do you want to use git y/n? (This is optional)")
  
  wantgit <- substring(yesno,1,1) %in% c("y","Y")
  
  setAdaprOptions("git",ifelse(wantgit,"TRUE","FALSE"))
  
  if(wantgit==FALSE){
    username <- readline("What is your username? (This is optional)")
    setAdaprOptions("username",ifelse(username=="","Anonymous",username))
  }
  
  if(wantgit & grepl("Git does not",git_binary_path)){ 
    
    warning("Git is not installed. Some version control features limited.
            Please download and configure (git-scm.com). Try GIT client GUI!!")
    
  }
  # Check git config  
  
  if(wantgit){
    
    email <- ""
    
    try({
      email <- git2r::config()[["global"]]$user.email
    })
    
    if(!grepl("@",email)){
      
      print("Please enter Git User name")
      
      gituser <- readline("Please ender your preferred Git user name:    ")
      gitemail <- readline("Please ender your preferred Git email <somebody@somewhere.com>:   ")
      
      gitConfigure(gituser,gitemail)
      
      try({
        email <-  git2r::config()[["global"]]$user.email
      })
      
      
      if (!grepl("@",email)) {return("Git is not configured. Run: gitConfigure(user.name, user.email)")}
      
    }else{print("Git configured")}
    
  }# configure git
  
  
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
    
    setAdaprOptions("project.path",project.path.start)
    
    setAdaprOptions("publish.path", publish.path.start)
    
    firstProject(project.path.start, publish.path.start)
    
    
  }else{
    
    print("adaprHome project implies adpar already congifured")
    return("Already configured")
    
  }
  
  print(paste("Step",step,"of",total,"Completed default seup!"))
  step <- step + 1
  
  return("adapr setup: try 'adaprApp()'")
  
  
}# END default set.up
#' Create adaprTest example project
#' @details To be run after default adapr set up.
#' @param localLibraryTF Logical for local library or not
#' @param overwrite Logical indicating whether to overwrite existing project
#' @export
#' @examples 
#'\dontrun{
#' # 
#'  loadAdaprTest()
#'
#'} 
#'
loadAdaprTest <- function(localLibraryTF=FALSE,overwrite=TRUE){
  
  # Loads an example project
  
  localLibraryTF <- ifelse(localLibraryTF=="packrat","packrat",as.logical(localLibraryTF))
  
  if(overwrite) {removeProject("adaprTest")}
  #setwd(file.path(getAdaprOptions()$project.path,"adaprTest"))
  #testFiles <- list.files(file.path(getAdaprOptions()$project.path,"adaprTest"),recursive=TRUE,all.files=FALSE)
  #zip("adaprTest.zip",files=testFiles)
  
  
  if("adaprTest" %in% get_orchard()$project.id){
    
    
    print("adaprTest already loaded try removeProject(\"adaprTest\")")
    
    return(1)
  }else{
    
    projectLocation <- system.file("adaprTest.zip", package = "adapr")
    
    newLocation <- getAdaprOptions()$project.path
    
    newDir <- file.path(newLocation,"adaprTest")

        dir.create(newDir)
    
    utils::unzip(zipfile = projectLocation, exdir = newDir, 
                 overwrite = TRUE)
    relocateProject("adaprTest",project.libraryTF = localLibraryTF)
    
    setProject("adaprTest")
    
  }
  return(0)   
}# END: loadAdaprTest
