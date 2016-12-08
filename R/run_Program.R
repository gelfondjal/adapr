#' Run an R script within a project using devtools::clean_source
#' @param project.id project id
#' @param source.file R script within that project
#' @return value from clean_source from devtools package
#' @export
#'@examples 
#'\dontrun{
#' run.program("adaprHome","read_data.R")
#'} 
#' 
run.program <- function(project.id=get("source_info")$project.id,source.file=get("source_info")$file$file){
  
  # get project object
  
  out <- devtools::clean_source(file.path(get.project.path(project.id),project.directory.tree$analysis,source.file))
  
  return(out)
}




#' Remove an R script from a project. Removes program, dependency, and results.
#' @param project.id project id
#' @param source.file R script within that project
#' @param ask is a logical whether to ask user
#' @return value from file.remove
#' @details Cannot be undone through adapr! Will not remove markdown or other program side-effects.
#' @export
#' 
#'@examples 
#'\dontrun{
#' prune.program("adaprHome","read_data.R")
#'} 
#' 
prune.program <- function(project.id=get("source_info")$project.id,source.file=get("source_info")$file$file,ask=TRUE){
  # get project object
  
  if(ask){
  
  test <- readline("Are you sure you want to remove program & results? y/n")
  
  if(test!="y"){
    
    return(FALSE)
    
  }
  }
  
  
  program <- file.path(get.project.path(project.id),project.directory.tree$analysis,source.file)
  dependencyDir <- file.path(get.project.path(project.id),project.directory.tree$dependency.dir,
                paste0(source.file,".txt"))
  results <- file.path(get.project.path(project.id),project.directory.tree$results,source.file)
  
  inside.results <- list.files(results,full.names=TRUE,recursive = TRUE)
  
  while(length(inside.results)>0){
  
  inside.out <- file.remove(inside.results)
  
  inside.results <- list.files(results,full.names=TRUE,recursive = TRUE,include.dirs = TRUE)
  
  }
  
  results.out <- file.remove(results,recursive=TRUE,include.dirs = TRUE)
  
  results <- file.remove(c(program,dependencyDir))
    
  return(c(results,inside.out,results))
}





