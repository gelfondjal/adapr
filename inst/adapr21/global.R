
#if(!require(shinyIncubator)){devtools::install_github("shiny-incubator", "rstudio")}
#require(shinyIncubator)
library(devtools)
library(adapr)

all.orchards <-get_orchard()


adapr_options <- get_adapr_options(TRUE)


# Create start-up directories
# Martin

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

#print(publish.path)
#print(project.path)

#dir.create(project.path,recursive=TRUE)
#dir.create(publish.path,recursive=TRUE)

project.path.start <- project.path#ifelse(dir.exists(project.path),project.path,"")
publish.path.start <- publish.path#ifelse(dir.exists(publish.path),publish.path,"")

if(is.null(adapr_options$project.path)){
	
	set_adapr_options("project.path" ,project.path.start)
	
}

if(is.null(adapr_options$publish.path)){
	
	set_adapr_options("publish.path",publish.path.start)
	
}

adapr_options <- get_adapr_options(TRUE)


# Retrieve git login info

gitAuthor <- "Name"
gitEmail <- "Email"
if(adapr_options$git=="TRUE"){
try({
gitAuthor <- system2(git_path(NULL), paste("config --global user.name"),stdout=TRUE)
gitEmail <- system2(git_path(NULL), paste("config --global user.email"),stdout=TRUE)
})
}
if(class(gitAuthor)!="character"){
  
  gitAuthor <- "Name"
  gitEmail <- "Email"
  
  
  
}




get_filelist <- function(project.id){
  
  # Get files and descriptions for project
  
  si <- pull_source_info(project.id)
  
  files <- get.project.info.si(si)$all.files
  
  files$Path <- gsub(paste0(si$project.path,"/"),"",fixed=TRUE,files$fullname)
 
  files$Description <- files$description
  
  results <- subset(files,select=c("Path","Description"))
   
  return(results)
  
}


smart.install.packages <- function(package.name,repository=""){
  
  print("smart.install.packages")
  print(require(package.name))
  
  repository <- ifelse(is.na(repository),"",repository)
  
  if(repository==""){
    
    install.packages(package.name)
    
    return(TRUE)
  }
  
  
  if(repository=="bioC"){
  
    source("http://bioconductor.org/biocLite.R")
    
    biocLite(package.name,ask=TRUE)
    
    return(TRUE)
  }  
  
  install.command.list <- grepl("^install",repository)
  
  if(install.command.list){
   source(textConnection(repository))
   return(TRUE)
  }
    
  
  
  
  return(FALSE)
  
}
