#' Writes dependency data to file in "Dependency" directory
#' @param RMD Logical denoting whether the finalizing occurs in markdown vs R script
#' @param write Logical indicated to write the dependency object
#' @details Operates git tracking of program and dependency file. 
#' @details Strips project directory out of dependency file
#' @return dependency.object 
#' @export
#' @examples 
#'\dontrun{
#' #Executed only at the end of adapr R script
#' #finalize_dependency() 
#'} 
#' 

finalize_dependency <- function(RMD=TRUE,write=TRUE){
  
  # read in dependency object from dependency.file in source_info
  # return dependency object
  
  if(!exists("source_info")){
    
    source_info <- list()
    
    stop("finalize_dependency (adapr) error: source_info not found")
    
  }
  
 
  current.dir <- getwd()
  # Copy and render Rmd file
  file.copy(source_info$rmdfile$fullname,file.path(source_info$results.dir,source_info$rmdfile$file),overwrite=TRUE)
  if(!checkRmdMode()){
  outputfile <- rmarkdown::render(file.path(source_info$results.dir,source_info$rmdfile$file))
  outfile <- Create.file.info(source_info$results.dir, basename(outputfile), paste("rendered Rmarkdown of", source_info$file$file))
  Write.cap(NULL, outfile, I, source_info)
  }else{
    outputfile <- gsub("Rmd","html",file.path(source_info$results.dir,source_info$rmdfile$file))
    outfile <- Create.file.info(source_info$results.dir, basename(outputfile), paste("rendered Rmarkdown of", source_info$file$file))
    if(!file.exists(outfile$fullname)){write(0,outfile$fullname)} # Write a stub for tracking
    Write.cap(NULL, outfile, I, source_info)
  }
  Read.cap(source_info$rmdfile, I, source_info)


  
  Write(utils::sessionInfo(),paste0("Session_info_",source_info$file$db.name,".RObj"),paste0("sessionInfo for", source_info$file[["file"]]),save)
  
  # Render the markdown
    
  dependency.file <- file.path(source_info$dependency.dir,source_info$dependency.file)
  
  
  dependency.out <- source_info$dependency$data 
  
  dependency.out <- subset(dependency.out,!is.na(dependency))
  
  n.output.files <-	nrow(subset(dependency.out,dependency=="out"))
  print(c("# of output files",n.output.files))
  
  project.path <- dependency.out$project.path[1]
  
  dependency.out$source.git <- NA
    if(source_info$options$git){
  try({
   # dependency.out$source.git <- paste(git.info(as.character(dependency.out$path[1]),file.path(dependency.out$source.file.path[1],dependency.out$source.file)[1])[1:5],collapse=" ")
    dependency.out$source.git <- commit2char(git2r::commits(git2r::repository(source_info$project.path))[[1]])
    
  })	
  }
  dependency.out$source.mod.time <- as.character(file.info(file.path(dependency.out$source.file.path[1],dependency.out$source.file[1]))$mtime)
  
  dependency.out$source.hash <-   Digest(file=file.path(dependency.out$source.file.path[1],dependency.out$source.file[1]),serialize=FALSE) 
  
  
  for(dep.row.iter in 1:nrow(dependency.out)){
    
    target.file <- file.path(dependency.out$target.path[dep.row.iter],dependency.out$target.file[dep.row.iter])
    
    #		print(target.file)
    
    dependency.out$target.hash[dep.row.iter] <- Digest(file=as.character(target.file),serialize=FALSE)
    
    dependency.out$target.mod.time[dep.row.iter] <- as.character(file.info(as.character(target.file))$mtime)
    
  }


  dependency.out <- subset(dependency.out,""!=dependency.out$target.hash)
  
  dependency.out <- subset(dependency.out,!duplicated(file.path(as.character(dependency.out$target.path),as.character(dependency.out$target.file))))

  setwd(current.dir)
    
  if(write){
    
    # strip out project path in dependency file
          
    trees <- dependency.out
    new.path <- ""
    shaved.variables <- c("path","source.file.path","target.path","project.path")
     
    n.shavechars <- nchar(trees$project.path[1])+1
      
      for(char.shave in shaved.variables){
          trees[[char.shave]]<- substr(trees[[char.shave]],n.shavechars,nchar(trees[[char.shave]]))
          trees[[char.shave]] <- gsub(paste0("^",.Platform$file.sep,"+"),"",trees[[char.shave]])
      }
    
    write.dependency(trees,dependency.file)
    
    if(source_info$options$git){
    	try({	git.add(project.path,file.path(dependency.file))	})
    }
       
    
  }
  
  # Write library versions to specific source directory
  
  print(paste("Completed",source_info$file[["file"]]))
  
  setwd(current.dir)
  
  return(dependency.out)	
  
  
}
