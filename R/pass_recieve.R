#' Simple way to send objects from one script to another.
#' @param x R object to send
#' @param description String describing R object stored in tree
#' @return Value of Write.
#' @details Adds '.rda' to object name and writes to Result file. Works with 'Receive'.
#' @export
#' @examples 
#'\dontrun{
#' # Requires internet connection to access GitHub.
#' Pass(myObject,"great result")
#'} 

Pass <- function(x,description="results"){
  #
  # Description: Quick drop of function  
  #
  
  fileName <- paste0(as.character(substitute(x)),".rda")
  
  out <- Write(x,fileName,description)
  
  return(out)
  
}# END function: Pass

#' Simple way to read R object into script produced from another.
#' @param varname String of R object name from other R script.
#' @param Rscript R script that created the R object to read in.
#' @param output Default FALSE logical indicating whether to return object object or create object of name varname.
#' @return NULL or value of varname
#' @details Creates object in environment called varname. Don't need to specify Rscript. 
#' @details Adds '.rda' to object name and reads from Result file. Works with 'Pass'.
#' @export
#' @examples 
#'\dontrun{
#' # Requires internet connection to access GitHub.
#' Recieve("myObject") # Creates myObject in workspace
#' newName <- Recieve("myObject",output=TRUE) # Returns value of 'myObject'
#' Receive("myObject","myRscript.R")
#'} 

Receive <- function(varname,Rscript="",output=FALSE){
  
  fileName <- paste0(varname,".rda")
  
  fileName <- ifelse(Rscript=="",fileName,file.path(Rscript,fileName))
  
  x <- loadFlex(fileName)
  
  if(output){
    return(x)
  }else{  
    eval(parse(text=(paste(varname,"<<-x"))))
    return(paste("Overwrote",varname))
  }
}