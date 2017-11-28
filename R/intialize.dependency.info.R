#' Initializes dependency object source_info
#' @param source_info_arg is a source_info list with describing R script and project
#' @return Depedency file location
#' @details Not for direct use.
#' @export
#' 
initialize_dependency_info <- function(source_info_arg){
  
  # initialize dependency file R object, store filename
  # project.path is for git
  
  source.file.info <- source_info_arg$file
  project.path <- source_info_arg$project.path
  project.id <- source_info_arg$project.id
  dependency.path <- source_info_arg$dependency.dir
  
  dependency.file <- file.path(source_info_arg$dependency.dir,source_info_arg$dependency.file)  #file.path(dependency.path,paste(source.file.info[2],".txt",sep=""))
  
  dependency.out <- data.frame(source.file.path=source.file.info[["path"]],source.file=source.file.info[["file"]],source.file.description=source.file.info[["description"]],
                               source.run.time=as.POSIXct(Sys.time(),tz = Sys.timezone()),
                               path=dependency.path,project.path=project.path,project.id=project.id,
                               target.path=NA,
                               target.file=NA,
                               dependency=NA,
                               target.description=NA,
                               stringsAsFactors=FALSE)
  
  
  source_info_arg$dependency$update(dependency.out)
  
  #  write.dependency(dependency.out,dependency.file)	
  
  
  if(source_info_arg$options$git){
  
  try({
    
    #gitout <- gitInit(project.path)
    
    #setwd(source_info_arg$analysis.dir)
    
    #git_binary_path <- git_path(NULL)
    #if(source_info_arg$git.log){temp <- system2(git_binary_path,"log",stdout="")}else{
    #temp <- system2(git_binary_path,"log",stdout=NULL)}
    
    no.repository <- TRUE
    try({
      no.repository <- (0==length(git2r::commits(git2r::repository(project.path))))
    },silent = TRUE)
    
    if(no.repository){
      
      #gitAdd(project.path,file.path(source.file.info[["path"]],source.file.info[["file"]]))	
      #gitXcommit(project.path,"Intitialize git")
      
      git2r::init(project.path)
      repo <- git2r::repository(project.path)
      gitIgnoreLibrary(project.id)
      gitAdd(project.path,file.path(source.file.info[["path"]],source.file.info[["file"]]))
      git2r::commit(repo,message ="Initialize git")
      
      print("Initialized git repo")
      
    }
    gitIgnoreLibrary(project.id)
    gitAdd(project.path,file.path(source.file.info[["path"]],source.file.info[["file"]]))	
    
  })#try get
  }
  
  #  gitAdd(project.path,file.path(dependency.file))	
  
  # load the libraries and the dependent source functions

  loadInstallLibraryFile(file.path(source_info_arg$support.dir,source_info_arg$support.library.file)) # Vestigial from adapr 1.0
  
  runSourceDirectory(source_info_arg$support.dir)
  runSourceDirectory(source_info_arg$source.support.dir)
  
  outlibraries <- idPackages(file.path(source_info_arg$support.dir,source_info_arg$support.library.file))
  
  if (length(outlibraries)>0) {print(paste("Libraries not automatically loaded",outlibraries))}
  
  utils::write.csv(as.data.frame(devtools::session_info()$packages),file.path(source_info_arg$source.support.dir,"package_info.csv"))
  
  support.files <- unlist(lapply(c(source_info_arg$support.dir,source_info_arg$source.support.dir)
  										,list.files,recursive=FALSE,full.names=TRUE,include.dirs=FALSE))
    
  #print(support.files)  
  
  #print(dependency.file)
  
  #	return(NULL)	
  
  # add source files to dependency set was	
  
  for(file.name in support.files){
    print(file.name)
   
    if(grepl("(\\.r)|(\\.R)$",file.name)){
      if(source_info_arg$options$git){try({ gitAdd(project.path,file.name) })}
      
      Read.cap(createFileInfo(dirname(file.name),basename(file.name),"Support file"),I,options()$adaprScriptInfo)
    }
  }		 
 #No longer linking the dependency on the library file!  
    
#  file.name <- file.path(source_info_arg$support.dir,source_info_arg$support.library.file)
#  Read.cap(createFileInfo(dirname(file.name),basename(file.name),"Support file"),I,options()$adaprScriptInfo)
  
  
  
  return(dependency.file)
  
}	
