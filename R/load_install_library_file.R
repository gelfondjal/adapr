#' Loads librarys within the file library.list.file
#' @param library.data.file CSV File with a set of library names and repository locations
#' @param subgroup data frame with Package, repos, and specific columns
#' @param verbose Print which libraries are installed and loaded
#' @return Library information data
#' @details Installs and loads all packages
#' @export
#' 
#' 
load.install.library.file <- function(library.data.file=NA,subgroup=NULL,verbose=FALSE){
 
  if(is.na(library.data.file)){library.data.file <- file.path(source_info$support.dir,source_info$support.library.file)}
  
  if(!file.exists(library.data.file)){
    print("No library information file")
    library.stub <- data.frame(Package="devtools",repos="",specific=FALSE)
    write.csv(library.stub,library.data.file,row.names=FALSE)
  }
  
  packages.info.all <- read.csv(library.data.file,as.is=TRUE)
  
  if(is.null(packages.info.all$specific)){packages.info.all$specific <- FALSE}
  
  #Only load nonspecific packages if subgroup is null
  
  if(is.null(subgroup)){
    packages.info <- subset(packages.info.all,!specific)
    
  }else{
    
    packages.info <- subset(packages.info.all, (!specific) | (Package %in% subgroup$Package))    
    
    new.packs <- subset(subgroup,!(Package %in% packages.info$Package))
    
    packages.info <- unique(rbind(subgroup,packages.info))
    
  }
  
  
  
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
      
      
      bioc.list <- subset(packages.info,(repos=="bioC")&(install.check==FALSE))$Package
      
      print(paste("Installing",bioc.list))
      
      
      source("http://bioconductor.org/biocLite.R")
      
      biocLite(bioc.list,ask=TRUE)
      lapply(bioc.list, require, character.only=TRUE)
      
      print(paste("Loaded",bioc.list))
      
      packages.info$install.check[packages.info$repos=="bioC"] <- TRUE
      
    })	
    
  }
  
  # Use install command
  
  if(sum((packages.info$install.check==FALSE)&(packages.info$repos!="bioC"),na.rm=TRUE)){
    
    
    try({
      
      
      install.command.list <- subset(packages.info,(repos!="bioC")&(install.check==FALSE))$repos
      
      install.command.list <- grep("^install",install.command.list,value=TRUE)
      
      if(length(install.command.list)>0){
        sapply(install.command.list,function(x){source(textConnection(x))})
  
        lapply(subset(packages.info,repos %in% install.command.list)$Package, require, character.only=TRUE)      
             
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
        
        install.packages(packages.info$Package[library.iter],lib=.libPaths()[1],repos=repository)
        print(.Library)
        print(.libPaths())
        print(Sys.getenv("R_LIBS_USER"))
        library(packages.info$Package[library.iter],character=TRUE)		
        
        if(verbose){ print(paste("Loaded",packages.info$Package[library.iter]))}
        
        
        packages.info$install.check[library.iter] <- TRUE
      }
    })
    
  }
  
  
  if(!is.null(subgroup)){
  

      packages.info.out <- subset(rbind(subgroup,packages.info.all),!duplicated(Package))
            
      not.installed <- subset(packages.info,!install.check)$Package
      
      packages.info.out <- subset(packages.info.out,!(Package %in% not.installed))
      
      write.csv(packages.info.out[order(packages.info.out$Package),],library.data.file,row.names=FALSE)
    
}
  
  
  print(packages.info$install.check)
  
  return(packages.info)	
  
  
}
