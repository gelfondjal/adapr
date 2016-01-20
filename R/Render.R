#' Renders and Rmarkdown file
#' @param Rmd.file Filename of .Rmd file within the R scripts' results directory (source_info$results.dir)
#' @param ... extra arguments for rmarkdown::render
#' @return Rendered output file information
#' @details Uses rmarkdown library to access objects in the R script. Will track dependencies for objects used within Rmd file.
#' @export
#' 
#' 


Render_Rmd <- function(Rmd.file,description="Rmarkdown",...){
  
  require(rmarkdown)
  
  mdfile <- Create.file.info(source_info$results.dir,Rmd.file,description)
  
  outputfile <- rmarkdown::render(mdfile$fullname,...)
  
  
  Read.cap(mdfile,I,source_info)#,envir=parent.frame())
  
  outfile <- Create.file.info(source_info$results.dir,basename(outputfile),paste("rendered Rmarkdown of",description))
    
  Write.cap(NULL,outfile,I,source_info)#,envir=parent.frame())
  
  return(outfile)
  
}
