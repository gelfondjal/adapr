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
    
    files <- list.files(file.path(getProjectPath(project.id),"Programs"))
    
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





#' Opens R markdown from a project with default R program. 
#' @param markdownFile Mardown filename to open, then current R script
#' @param project.id string for project id to search within.
#' @details If markdownFile argument is blank or does not exist, then lists available scripts for convenience. 
#' @export
#' @examples 
#'\dontrun{
#' # Opens read_data.R within the adaprHome project
#' openMarkdown("read_data.Rmd","adaprHome")
#'
#'} 
#'

openMarkdown <- function(markdownFile=getSourceInfo()$rmdfile$file,project.id = getProject()){
  
    files <- list.files(file.path(getProjectPath(project.id),"Programs","Markdown"))
    
    files <- grep("\\.Rmd",files,value=TRUE)
    
    if(!(markdownFile %in% basename(files))){
    
      df <- data.frame(Markdown=sort(basename(files)))
      print(df)
    
      n <- as.integer(readline("Which markdown (specify row number)?"))
    
      if(!(n %in% 1:length(files))){n <- 1}
    
      markdownFile <- df$Markdown[n]

    }
    
    
 programDir <- file.path("Programs","Markdown")
  
  return(utils::browseURL(file.path(getProjectPath(project.id ),programDir,markdownFile)))
}



