
if(!require(shinyIncubator)){devtools::install_github("shiny-incubator", "rstudio")}
require(shinyIncubator)
library(devtools)
library(adapr)

all.orchards <-get_orchard()



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
