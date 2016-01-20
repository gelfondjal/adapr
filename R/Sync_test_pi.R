#' Tests the synchrony of files in dependency tree
#' @param project_info Project information from get_project_info function
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' 
Sync.test.pi <- function(project_info,plotl0=FALSE){return(Sync.test(project_info$graph,project_info$tree,plotl=plotl0))}

