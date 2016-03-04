
if(!require(shinyIncubator)){devtools::install_github("shiny-incubator", "rstudio")}
require(shinyIncubator)
library(devtools)
library(pander)
library(IT2)
panderOptions("table.split.table",Inf)
all.orchards <-get_orchard()


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
