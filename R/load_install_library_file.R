#' This function is no longer supported. Loads libraries within the file library.list.file
#' @param library.data.file CSV File with a set of library names and repository locations
#' @param subgroup data frame with Package, repos, and specific columns
#' @param verbose Print which libraries are installed and loaded
#' @param install.all logicial indicated whether to install all project packages
#' @return Library information data
#' @details Installs and loads all packages.Not for direct use. See installProjectPackages().
#' @export
#' 
#' 
#' 
loadInstallLibraryFile <- function(library.data.file=NA,subgroup=NULL,verbose=FALSE,install.all=FALSE){
  
  if(is.na(library.data.file)){library.data.file <- file.path(getSourceInfo()$support.dir,getSourceInfo()$support.library.file)}
  
  if(!file.exists(library.data.file)){
    print("No library information file")
    library.stub <- data.frame(Package="devtools",repos="",specific=FALSE)
    utils::write.csv(library.stub,library.data.file,row.names=FALSE)
  }
  
  packages.info.all <- utils::read.csv(library.data.file,as.is=TRUE)
  
  if(!(install.all)){
  
  if(is.null(packages.info.all$specific)){packages.info.all$specific <- FALSE}
  
  #Only load nonspecific packages if subgroup is null
  
  if(is.null(subgroup)){
    packages.info <- subset(packages.info.all,!packages.info.all$specific)
    
  }else{
    
    packages.info <- subset(packages.info.all, (!packages.info.all$specific) | (packages.info.all$Package %in% subgroup$Package))    
    
    new.packs <- subset(subgroup,!(subgroup$Package %in% packages.info$Package))
    
    packages.info <- unique(rbind(subgroup,packages.info))
    
  }
  
  }#if not install.all, take subset of packages
  
  
  
  for(library.iter in 1:nrow(packages.info)){
    
    
    packages.info$install.check[library.iter]  <- 	require(packages.info$Package[library.iter],character.only=TRUE)
    
    if(packages.info$install.check[library.iter]){
      library(packages.info$Package[library.iter],character.only=TRUE)
      if(verbose){print(paste(packages.info$Package[library.iter],"Installed, loaded"))}
    }
  }
  
  # install biocounductor packages
  
  if(sum((packages.info$install.check==FALSE)&(packages.info$repos=="bioC"),na.rm=TRUE)){
    
    
    try({
      
      
      bioc.list <- subset(packages.info,(packages.info$repos=="bioC")&(packages.info$install.check==FALSE))$Package
      
      print(paste("Installing Bioconductor",bioc.list))
      source("http://bioconductor.org/biocLite.R")
      
      tempfcn <- get("biocLite")
      tempfcn(bioc.list,ask=TRUE)
      lapply(bioc.list, require, character.only=TRUE)
      
      print(paste("Loaded",bioc.list))
      
      packages.info$install.check[packages.info$repos=="bioC"] <- TRUE
      
    })	
    
  }
  
  # Use install command
  
  if(sum((packages.info$install.check==FALSE)&(packages.info$repos!="bioC"),na.rm=TRUE)){
    
    
    try({
      
      
      install.command.list <- subset(packages.info,(packages.info$repos!="bioC")&(packages.info$install.check==FALSE))$repos
      
      install.command.list <- grep("^install",install.command.list,value=TRUE)
      
      if(length(install.command.list)>0){
        sapply(install.command.list,function(x){source(textConnection(x))})
  
        lapply(subset(packages.info,packages.info$repos %in% install.command.list)$Package, require, character.only=TRUE)      
             
        packages.info$install.check[packages.info$repos %in% install.command.list] <- TRUE
      }
      
    })  
    
  }
  
  
  for(library.iter in 1:nrow(packages.info)){
    
    
    try({
      
      if(!packages.info$install.check[library.iter] ){
        
        print(paste("Installing",packages.info$Package[library.iter]))
        
        repository <- packages.info$repos[library.iter]
        
        repository <- ifelse(is.na(repository)|(repository==""),getOption("repos"),repository)
        
        utils::install.packages(packages.info$Package[library.iter],lib=.libPaths()[1],repos=repository)
        print(.Library)
        print(.libPaths())
        print(Sys.getenv("R_LIBS_USER"))
        library(packages.info$Package[library.iter],character.only=TRUE)		
        
        if(verbose){ print(paste("Loaded",packages.info$Package[library.iter]))}
        
        
        packages.info$install.check[library.iter] <- TRUE
      }
    })
    
  }
  
  
  if(!is.null(subgroup)){
  
      tempckgdf <- rbind(subgroup,packages.info.all)
      packages.info.out <- subset(tempckgdf,!duplicated(tempckgdf$Package))
            
      not.installed <- subset(packages.info,!packages.info$install.check)$Package
      
      packages.info.out <- subset(packages.info.out,!(packages.info.out$Package %in% not.installed))
      
      utils::write.csv(packages.info.out[order(packages.info.out$Package),],library.data.file,row.names=FALSE)
    
}
  
  
  print(packages.info$install.check)
  
  return(packages.info)	
  
  
}
#' Install package of specific version
#' @param package package name to install
#' @param version package version
#' @param installVersion logical, TRUE for install a specific version, if FALSE then latest
#' @param lib path to local library
#' @param repos character of repository
#' @param show.available logical to display whether the package is available
#' @param packageSource character describing where the papckage is from.
#' @param ... Argument to install.packages/install.version functions
#' @return Library information data
#' @details Installs from CRAN and bioconductor packages. Local libraries will not be installed.
#' @export
#'
install <- function(package,version=NULL,installVersion=FALSE,lib=.libPaths()[1],repos=getOption("repos"),show.available=FALSE,packageSource="",...){
  
  if(repos=='cran'){
    # Show available versions of the package
    if(show.available){
      print(versions::available.versions(package)[[package]])
    }
    # Install package no version specified (default is to use the most current version)
    if(!installVersion){
      utils::install.packages(package,lib=lib,...)
      cat(paste0("Cran Package ",package," successfully installed!\n"))
    }
    # Install package with version specified
    if(installVersion){
      versions::install.versions(package, version, lib=lib,...)
      cat(paste0("Cran Package ",package,version," successfully installed!\n"))
    }
  }# Cran package
  # Install bioconductor package
  if(repos=="bioc"){
    if(!exists("biocLite")){
      source("http://bioconductor.org/biocLite.R")
    }
    tempfcn <- get("biocLite")
    tempfcn(package,ask=TRUE)
    cat(paste0("Bioconductor Package ",package," successfully installed!\n"))
  }# Bioc package
    
    
  if(repos=="gith"){
   	
   	gitname <- gsub(".*\\(|\\).*","",packageSource)
   	
   	if(!installVersion){gitname <- gsub("@.*","",gitname)}
   	
   	devtools::install_github(gitname)

  }# github package 
    
    
}
#' Check install of package of specific version
#' @param package0 name of package
#' @param version0 package version
#' @param versionCheck logical to install specific version
#' @param lib path to local library
#' @return logical on installed status
#' @export
checkVersion <- function(package0,version0="",versionCheck=FALSE,lib=.libPaths()[1]){
  
  installed <- utils::installed.packages(lib)
  packageTRUE <- package0 %in% rownames(installed)
  if(versionCheck&packageTRUE){
    packageTRUE <- gsub("\\-","\\.",as.character(utils::packageVersion(package0,lib)))==gsub("\\-","\\.",version0)
  }
  return(packageTRUE)
  
}
#' Install package of specific version
#' @param input data.frame with 3 columns package version repos to install
#' @param lib path to local library
#' @param versionCheck logical to install specific version
#' @return Library information data
#' @details Calls adapr::install and installs from CRAN and bioconductor packages. Local packages will not be installed.
#' @export
#' @examples 
#'\dontrun{
#' setProject("adaprHome")
#' installLibrary()
#' } 
#'
installLibrary <- function(input=getLibrary(),lib=getProjectLibrary(),versionCheck=FALSE){
    input$success <- FALSE
    for(p in seq_along(input[,1])){
    
      #cat(paste0("Installing package ",input$package[p],"...\n"))
      
      if(checkVersion(input$package[p],input$version[p],versionCheck,lib)){
        
        #cat(paste0("Version ",as.character(packageVersion(input$package[p],lib))," already installed!\n"))  
        
      }
      else{
        
        dependencies <- !versionCheck
        
        if(dependencies){dependencies <- c("Depends","Imports")}
        
        
        adapr::install(input$package[p],version=input$version[p],installVersion=versionCheck,lib=lib,repos=input$repos[p],
                show.available=FALSE,dependencies=!versionCheck,packageSource=input$source[p])
      
      }
     
      
      input$success[p] <- checkVersion(input$package[p],input$version[p],versionCheck,lib)
      input$installVersion[p] <- ""
      
      try({
        input$installVersion[p] <- as.character(utils::packageVersion(input$package[p], 
                                                                      .libPaths()))
      })
      
    }#loop over p package
  
  return(input)
}



#' Get library for a project
#' @param project.id character vector of project
#' @return dataframe of libraries
#' @export
#' @examples 
#'\dontrun{
#' getLibrary("adaprHome")
#'} 
#' 
getLibrary <- function(project.id = getProject()){
  
  programs <- listScripts(project.id)
  
  path <- getProjectPath(project.id)
  
  packageInfo <- plyr::ddply(programs,"source.file",function(program){
    
    results.dir <- file.path(path,adapr::project.directory.tree$results,program$source.file[1])
    
    packages <- file.path(results.dir,paste0("Session_info_",gsub("\\.","_",program$source.file[1]),".RObj"))
    #print(packages)
    redd <- FALSE
    try({
     
      out <- readRDS(file=packages)$packages
      
    },silent=TRUE)
    
    if(!exists("out")){
      load(file=packages)
      if(!exists("obj")){obj <- NULL}
      out <- obj$packages
    }
    
   return(out)
    
  })
  
  packageInfo$repos <- tolower(substring(packageInfo$source,1,4))
  
  packageInfo <- subset(packageInfo,!duplicated(packageInfo$package))
  
  return(packageInfo)
  
}
