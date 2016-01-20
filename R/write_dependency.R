#' Write the dependcy object to file
#' @param dependency.object dependency object to output
#' @param dependency.pointer filename to write
#' @return TRUE
#' @export
#' 
write.dependency <- function(dependency.object,dependency.pointer){
  
  # A generic function that writes the dependency object to the dependency.pointer location
  
  write.table(dependency.object,dependency.pointer,sep="\t",row.names=FALSE)
  
  return(TRUE)
  
}