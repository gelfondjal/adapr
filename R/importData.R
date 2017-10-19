#' Import selected file into the project data directory
#' @param datafile filename can be unspecified
#' @param project.id string indicating which project data directory to copy into
#' @param overwriteTF logical indicating whether to overwrite exist file.
#' @details If datafile is "" then a file choose dialogue is created
#' @export
#' @examples 
#'\dontrun{
#' # Will open file browser to copy into adaprHome data directory
#' importData(project.id="adaprHome")
#'
#'} 
importData <- function(datafile="",project.id = getProject(),overwriteTF=TRUE){
  
  print("Importing to project")
  print(project.id)
  datafile <- file.choose()
  
  file.copy(datafile,dataDir(project.id),overwrite=overwriteTF)
  
}