#' Searches R scripts and R markdown files within a project.
#' @param matcher string or regular expression to identify within R or R markdown files
#' @param project.id string for project id to search within.
#' @param ... arguments to grep
#' @details Uses grep. Counts lines with matches, but repeats within a line are not counted.
#' @return Data frame with file names and counts of lines with matches. 
#' @export
#' @examples 
#'\dontrun{
#' # Opens read_data.R within the adaprHome project
#' searchScripts("read_data.R","adaprHome")
#'
#'} 
#'

searchScripts <- function(matcher,project.id = getProject(),...){
  
  path <- getProjectPath(project.id)
  
  
  files <- list.files(file.path(path,'Programs'),recursive = TRUE)
  
  filesr <- grep('(R$)|(\\.Rmd$)',files, value=TRUE)
  
  scripts <- file.path(path,'Programs',filesr)
  
  top <- lapply(scripts,scan,what=character(),sep="\n")
  
  names(top) <- basename(scripts)
  
  dirnamer <- basename(dirname(scripts))
  
  dirs <- data.frame(file=basename(scripts),directory=dirnamer)
  
  count.dat <-  plyr::ldply(lapply(top,function(y){return(sum(grepl(matcher,y)))}))
  
  count.dat <- subset(count.dat,count.dat$V1>0)
  
  names(count.dat) <- c("file","count")
  
  count.dat <- merge(dirs,count.dat)
  
  count.dat <- count.dat[order(-count.dat$count),]
  
  return(count.dat)
  
}

