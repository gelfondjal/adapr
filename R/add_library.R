#' Read library file
#' @param project.id project.id to read library file 
#' @return Autoloaded library information data
#' @details Not for direct use. Uses read.csv to read autoloaded libraries.
#' @export
#' @examples 
#' \dontrun{
#' readLibrary("adaprHome")
#'} 
#' 
readLibrary <- function(project.id=getProject()){
  source_info <- pullSourceInfo(project.id)    
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
#' @details Not for direct use. Installs and loads all packages
#' @export
#' @examples 
#' \dontrun{
#' addPackage("adaprHome","ggplot2")
#'} 
#' 
addPackage <- function(project.id=getProject(),library.name,library.install=NA,library.specific=FALSE){
  source_info <- pullSourceInfo(project.id)    
  library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  subgroup <- data.frame(Package=library.name,repos=library.install,
                       specific=as.logical(library.specific))  
  library.info <- loadInstallLibraryFile(library.file,subgroup)
  library.info <- utils::read.csv(library.file,as.is=TRUE)
  return(library.info)
}
#' Remove R package to a project
#' @param project.id project.id to add R package to
#' @param library.name R package name to add
#' @return Library information data
#' @details Not for direct use. Remove line from autoloading packages file.
#' @export
#' @examples 
#' \dontrun{
#' removePackage("adaprHome","ggplot2")
#'} 
#' 
removePackage <- function(project.id=getProject(),library.name){
  libout <- readLibrary(project.id)
  
  libout <- subset(libout,libout$Package!=library.name)
  source_info <- pullSourceInfo(project.id)    
  library.data.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  
  libout <- libout[order(libout$Package),]
  
  utils::write.csv(libout,library.data.file,row.names=FALSE)
  
  return(libout)
}
#' Installs all packages
#' @param project.id project.id to install packages for R package to
#' @return Library information data
#' @details Not for direct use. Installs autoloaded packages.
#' @export
#' @examples 
#' \dontrun{
#' installProjectPackages("adaprHome")
#'} 
installProjectPackages <- function(project.id=getProject()){
  source_info <- pullSourceInfo(project.id)    
  library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
  loadInstallLibraryFile(library.data.file= library.file,verbose=TRUE,install.all=TRUE)
  
  return(readLibrary(project.id))
}
  
  
  
  
