#' Installs and loads library specific to a project
#' @param package character for package to load/install
#' @param repository character for location of repository. "cran" for CRAN and "bioc" for bioconductor.
#' @param github character for devtools::install_github("xxx")
#' @param project.id project.id project id install within
#' @details Use within program Body 
#' @export
#' @examples
#'\dontrun{
#' Library("adapr","cran",project.id="adaprHome")
#'} 
#' 
Library <- function(package,repository="cran",github="",project.id=getProject()){
  
  project_path <- getProjectPath(project.id)
  
  subDir <- getProjectLibrary(project.id)
  
  is.installed <- function(mypkg){
    is.element(mypkg, utils::installed.packages(lib.loc=subDir)[,1])
  }
  
  if(subDir!=""){.libPaths(subDir)}
  
  # Sets the library to that directory
  if(file.exists(subDir)){
    .libPaths(subDir)
  }else{
    dir.create(subDir)
    .libPaths(subDir)
  }
  
  if(package!="adapr"){
    
    loadTest <- is.installed("adapr")
    
    if(!loadTest){
      
      adaprInstall(library.location=subDir ,betaTF=FALSE)
      
    }
    
  }
  
  #detach(package,unload=TRUE,character.only=TRUE)
  
  loadTest <- is.installed(package)
  
  if(loadTest){
    require(package, lib.loc=.libPaths()[1], character.only=TRUE)
    return(loadTest)
    }
  
  # Check if package is available and install the package in directory Packages
  
  where <- ifelse(github=="",repository,"github")
  
  # CRAN 
  if(where %in% c("cran","bioc")){
    adapr::install(package,version=NULL,lib=subDir,repos=where,
            show.available=FALSE,dependencies=c("Imports","Depends"))
    }
  if(where=="github"){   
      # Trying github
      cat("***\nTrying to get package from github...\n\n")
      
      #install.packages("devtools", lib=.libPaths()[1])
      #withr::with_libpaths(new = .libPaths()[1], devtools::install_github(github))
      devtools::install_github(github,lib=.libPaths()[1])
    }   
  # Github
 
  loadTest <- require(package, lib.loc=.libPaths()[1], character.only=TRUE)
 
  return(loadTest)
  
}  



### EOF ###
#' Returns character string of adapr R package import dependencies
#' @return character vector of package names
#' @export
#' @examples
#'\dontrun{
#' adaprDependencies()
#'} 
#' 
adaprDependencies <- function(){
deps <- c("gdata",
  "plotly",
  "ggplot2",
  "shiny",
  "shinydashboard",
  "knitr",
  "rmarkdown",
  "igraph",
  "digest",
  "devtools",
  "plyr",
  "git2r",
  "methods",
  "parallel",
  "versions",
  "packrat",
  "doParallel")

return(deps)

}




