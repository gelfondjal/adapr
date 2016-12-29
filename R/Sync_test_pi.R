#' Tests the synchrony of files in dependency tree
#' @param project_info Project information from get_project_info function
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' @details Not for direct use. See sync.project() and synctest.project().
#' @examples 
#'\dontrun{
#' si <- pull_source_info("adaprHome")
#' projInfo  <- get.project.info(si$dependency.dir)
#' Sync.test.pi(projInfo)
#'} 
#'
#'
Sync.test.pi <- function(project_info,plotl0=FALSE){return(Sync.test(project_info$graph,project_info$tree,plotl=plotl0))}

