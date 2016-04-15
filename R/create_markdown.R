#' Renders and Rmarkdown file
#' @param target.file Markdown file to create
#' @param target.dir Directory to send target file
#' @param tyle Markdown target style
#' @param description Markdown description
#' @param si source_info object for tracking
#' @param overwrite overwrite existing R markdown?
#' @return File information
#' @details Uses rmarkdown library to access objects in the R script. Will track dependencies for objects used within Rmd file.
#' @export
#' 
#' 

create_markdown <- function(target.file=paste0(source_info$file$file,"md"),target.dir=source_info$markdown.dir,style="html_document",description="Markdown",si,overwrite=FALSE){
	
	file.information <- Create.file.info(target.dir,target.file,description=description)
		
	target.file <- file.path(target.dir,target.file)

#	Read.cap(file.information,read.fcn=I,source_info=si)
  
	
	if((!overwrite)&file.exists(target.file)){return(file.information)}
	
  if(source_info$options$git){
	git_binary_path <- git_path(NULL)
	author <- system2(git_binary_path, paste("config --global user.name"),stdout = TRUE)
  }else{author <- source_info$options$username}
	
	start.lines.generic <- c("---",
							paste("title:",paste0("\"",si$project.id," ",basename(target.file),": ",description,"\"")),
							paste("author:",paste0("\"",author,"\"")),
							paste("output:",style),
							"---",
							"```{r,echo=FALSE}\n
							paste(\"Created on\",(Sys.time() ))\n
							```",
							"```{r}\n```")
								
	start.lines.generic <- paste(start.lines.generic,collapse="\n")
	
	

	
	write(start.lines.generic,target.file)
	
	
	
	
	return(file.information)
	
}