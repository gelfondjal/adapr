
if(!require(shinyIncubator)){devtools::install_github("shiny-incubator", "rstudio")}
require(shinyIncubator)
library(devtools)
library(adapr)

all.orchards <-get_orchard()

# Create start-up directories

topdirs <- list.files(path.expand.2("~"),full.names=TRUE)
project.path <- file.path(grep("Documents$",topdirs,value=1)[1],"Projects")


publish.path <- file.path(project.path,"Publish")
publish.path <- gsub("\\\\","/",publish.path)
project.path <- gsub("\\\\","/",project.path)

#print(publish.path)
#print(project.path)

dir.create(project.path,recursive=TRUE)
dir.create(publish.path,recursive=TRUE)

project.path.start <- ifelse(dir.exists(project.path),project.path,"")
publish.path.start <- ifelse(dir.exists(publish.path),publish.path,"")


# Retrieve git login info

gitAuthor <- "Name"
gitEmail <- "Email"

try({
gitAuthor <- system2(git_path(NULL), paste("config --global user.name"),stdout=TRUE)
gitEmail <- system2(git_path(NULL), paste("config --global user.email"),stdout=TRUE)
})

if(class(gitAuthor)!="character"){
  
  gitAuthor <- "Name"
  gitEmail <- "Email"
  
  
  
}




get_filelist <- function(project.id){
  
  si <- pull_source_info(project.id)
  
  files <- get.project.info.si(si)$all.files
  
  paths <- gsub(file.path(si$project.path,""),"",fixed=TRUE,files$fullname)
  descriptions <- files$description
  
  results <- subset(data.frame(path=paths,description=descriptions,stringsAsFactors=FALSE),grepl("^Results",path))
  
  results$path <-  gsub(file.path("Results",""),"",fixed=TRUE,results$path)
  
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
