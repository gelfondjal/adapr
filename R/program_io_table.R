#' Create program io table
#' @param dependency.out Tree of dependencies
#' @details groups inputs and outputs
#' @return Matrix summarizing inputs and outputs
#' @export
#' 
program.io.table <- function(dependency.out){
  
  meta.summary <-  dependency.out[order(dependency.out$dependency),]
  
  
  
  if(nrow(meta.summary)>0){
    
    short.name <- meta.summary$target.file
    
    IO.status <- as.character(meta.summary$dependency)
    
    description <- meta.summary$target.description
    
    file.fullname <- file.path(meta.summary$target.path,meta.summary$target.file)
    
    hyperlink <- file.fullname
    
    #	short.name <- ifelse(IO.status=="out",hyperlink,short.name)
    
    table.matrix <- data.frame(cbind(IO.status,short.name,file.fullname,description),stringsAsFactors=FALSE)
    
    
  }	
  
  names(table.matrix) <- c("IO","File","Fullname","Description")
  
  return(table.matrix)
  
  
}

