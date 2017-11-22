#' Opens script from a project with default R program. Can open markdown files as well.
#' @param rscript R script or Mardown filename to open
#' @param project.id string for project id to search within.
#' @details If rscript argument is blank, then lists available scripts for convenience. 
#' @export
#' @examples 
#'\dontrun{
#' # Opens read_data.R within the adaprHome project
#' openScript("read_data.R","adaprHome")
#'
#'} 
#'

openScript <- function(rscript="",project.id = getProject()){
  
  markdownTF <- length(grep("\\.Rmd$",rscript))>0
  
  if(rscript==""){
    
    files <- list.files(file.path(getProjectPath(getProject()),"Programs"))
    
    files <- grep("\\.R",files,value=TRUE)
    
    df <- listScripts()
    df <- df[order(df$source.file),]
    print(df)
    
    n <- as.integer(readline("Which script (specify row number)?"))
    
    if(!(n %in% 1:length(files))){n <- 1}
    
    rscript <- df$source.file[n]
    
  }
  
  programDir <- "Programs"
  if(markdownTF){ programDir <- file.path("Programs","Markdown")}
  
  return(utils::browseURL(file.path(getProjectPath(project.id ),programDir,rscript)))
}

