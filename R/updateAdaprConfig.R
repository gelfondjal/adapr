#' Updates the project list file to include project specific libraries.
#' @return orchard
#' @export
#' @details Adds 2 columns to project listing. (project.library and library.path). This enables project specific libraries.
#' @details Projects are by default set to use the default library. If packrat is used, this is probably not the case.
#' @examples
#'\dontrun{
#' orchard <- updateOrchardLibraries()
#' print(subset(orchard,project.id=="adaprHome"))
#'} 
#' 
updateAdaprConfig <- function(){
  
  projectListing <- get_orchard()
  
  if("project.libraryTF" %in% names(projectListing)) {return(projectListing)}
  
  widerOrchard <- data.frame(get_orchard(),project.libraryTF=FALSE,library.path="",stringsAsFactors = FALSE)
  
  orchard.site <- file.path(path.expand.2("~"),"ProjectPaths","projectid_2_directory_adapr.csv")
  
  utils::write.csv(widerOrchard,orchard.site,row.names=FALSE)
  
  libdirectory <- readline("R library location? (leave blank if default)")
  
  setAdaprOptions("library",ifelse(libdirectory=="",.libPaths()[1],libdirectory))
  
  return(get_orchard())
  
}