
#' Read library file
#' @param project.id project.id to read library file 
#' @return Library information data
#' @details uses read.csv
#' @export
#' @examples 
#' \dontrun{
#' read_library("adaprHome")
#'} 
#' 
read_library <- function(project.id=get.project()){
  source_info <- pull_source_info(project.id)    
  library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")

  libout <- utils::read.csv(library.file,as.is=TRUE)
  
  libout <- libout[order(libout$Package),]
  
  return(libout)
  
}



#' Add R package to a project
#' @param project.id project.id to add R package to
#' @param library.name R package name to add
#' @param library.install Command to install library: NA for CRAN, bioC for bioconductor
#' @param library.specific logical indicate whether package is for specific R script
#' @return Library information data
#' @details Installs and loads all packages
#' @export
#' @examples 
#' \dontrun{
#' add_package("adaprHome","ggplot2")
#'} 
#' 
add_package <- function(project.id=get.project(),library.name,library.install=NA,library.specific=FALSE){

  source_info <- pull_source_info(project.id)    
  library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  subgroup <- data.frame(Package=library.name,repos=library.install,
                       specific=as.logical(library.specific))  
  library.info <- load.install.library.file(library.file,subgroup)
  library.info <- utils::read.csv(library.file,as.is=TRUE)
  return(library.info)
}



#' Remove R package to a project
#' @param project.id project.id to add R package to
#' @param library.name R package name to add
#' @return Library information data
#' @details Remove line from file
#' @export
#' @examples 
#' \dontrun{
#' remove_package("adaprHome","ggplot2")
#'} 
#' 
remove_package <- function(project.id=get.project(),library.name){
  libout <- read_library(project.id)
  
  libout <- subset(libout,libout$Package!=library.name)
  source_info <- pull_source_info(project.id)    
  library.data.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  
  libout <- libout[order(libout$Package),]
  
  utils::write.csv(libout,library.data.file,row.names=FALSE)
  
  return(libout)
}

#' Installs all packages
#' @param project.id project.id to install packages for R package to
#' @return Library information data
#' @details Installs packages 
#' @export
#' @examples 
#' \dontrun{
#' install_project_packages("adaprHome")
#'} 
install_project_packages <- function(project.id=get.project()){
  source_info <- pull_source_info(project.id)    
  library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  load.install.library.file(library.data.file= library.file,verbose=TRUE,install.all=TRUE)
  
  return(read_library(project.id))
}
  
  
  
  