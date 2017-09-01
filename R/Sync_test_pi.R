#' Lower level function that tests the synchrony of files in dependency tree given project information.
#' @param project_info Project information from get_project_info function
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' @details Not for direct use. See syncProject() and syncTestProject().
#' @examples 
#'\dontrun{
#' si <- pullSourceInfo("adaprHome")
#' projInfo  <- getProjectInfo(si$dependency.dir)
#' syncTestPI(projInfo)
#'} 
#'
#'
syncTestPI <- function(project_info,plotl0=FALSE){return(syncTest(project_info$graph,project_info$tree,plotl=plotl0))}
