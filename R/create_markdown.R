#' Creates an Rmarkdown file in specified directory
#' @param target.file Markdown file to create
#' @param target.dir Directory to send target file
#' @param style Markdown target style
#' @param description Markdown description
#' @param si source_info object for tracking
#' @param overwrite overwrite existing R markdown?
#' @return File information
#' @details Uses rmarkdown library to access objects in the R script. Will track dependencies for objects used within Rmd file.
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' #Create markdown file in markdown directory for tree_controller.R
#' createMarkdown()
#'} 
#' 
#' 
createMarkdown <- function(target.file=paste0(getSourceInfo()$file$file,"md"),target.dir=getSourceInfo()$markdown.dir,style="html_document",description="Markdown",si,overwrite=FALSE){
	
	file.information <- createFileInfo(target.dir,target.file,description=description)
		
	target.file <- file.path(target.dir,target.file)
#	Read.cap(file.information,read.fcn=I,source_info=si)
  
	
	docID <- printDocId(bornTF = TRUE,gitTF=TRUE)
	
	if((!overwrite)&file.exists(target.file)){return(file.information)}
	
  adapr_options <- getAdaprOptions()
  if(adapr_options$git=="TRUE"){
	author <- git2r::config()[["global"]]$user.name
  }else{author <- adapr_options$username}
	
	start.lines.generic <- c("---",
							paste("title:",paste0("\"",si$project.id," ",basename(target.file),": ",description,"\"")),
							paste("author:",paste0("\"",author,"\"")),
							paste("output:",style),
							"---",
							"```{r,echo=FALSE,message=FALSE,warning=FALSE,include=FALSE}\n require(\"adapr\") \n require(\"methods\") \n",
							"require(\"knitr\") \n",
#              "paste0(\"Created on \",(Sys.time() ))\n",
        
							"```\n\n\n",
              paste0(docID[1],"\n",docID[2],"\n",docID[3]),
							paste0("```{r,echo=FALSE,message=FALSE,warning=FALSE,include=FALSE}\n #If TRUE then will \"knit\" in RStudio \n if(FALSE){scriptLoader(",
							paste0("\"",si$project.id,"\""),
							                              ",", 
              paste0("\"",si$file$file  ,"\""),")}","\n",
							"\n require(\"adapr\") \n require(\"methods\") \n",
							"require(\"knitr\") \n",
							"```\n\n\n",
						"```{r,echo=FALSE,message=FALSE,warning=FALSE,include=TRUE}\n",

							"printDocId(bornTF = FALSE,gitTF=TRUE,parentFile=getSourceInfo()$rmdfile$fullname)\n",  
						  "```\n\n\n",

							      "\n\n\n\n"),	"<!-- Begin R Markdown Body Here --> ",
							
							"\n```{r,echo=FALSE} \n paste0(\"Finished on \",(Sys.time() ))\n if(checkRmdMode()){dependency.out <- finalize_dependency() } \n```")
								
	start.lines.generic <- paste(start.lines.generic,collapse="\n")
	
	write(start.lines.generic,target.file)
	
	return(file.information)
	
}
