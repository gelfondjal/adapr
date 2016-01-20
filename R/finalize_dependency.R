#' Writes dependency data to file in "Dependency" directory
#' @param source_info The source information list that contains dependency object
#' @param write Logical indicated to write the dependency object
#' @details Operates git tracking of program and dependency file. 
#' @details Strips project directory out of dependency file
#' @return dependency.object 
#' @export

finalize_dependency <- function(source_info,write=TRUE){
  
  # read in dependency object from dependency.file in source_info
  # return dependency object
  
  current.dir <- getwd()
  
  Write(sessionInfo(),paste0("Session_info_",source_info$file$db.name,".RObj"),paste0("sessionInfo for", source_info$file[["file"]]),save)
  
  if(source_info$pandoc){
    panderOptions("table.split.table",Inf)
    evalsOptions("cache.dir",source_info$tex.dir)
    setwd(source_info$tex.dir)
    source_info$report$format<-'html'
    source_info$report$export(source_info$file$db.name,open=FALSE)
    pandocInfo <- Create.file.info(source_info$tex.dir,paste0(source_info$file$db.name,".html"),"html markdown")
    Write.cap(NULL,pandocInfo,I,source_info)
    setwd(current.dir)
  }
  
  dependency.file <- file.path(source_info$dependency.dir,source_info$dependency.file)
  
  
  dependency.out <- source_info$dependency$data 
  
  dependency.out <- subset(dependency.out,!is.na(dependency))
  
  n.output.files <-	nrow(subset(dependency.out,dependency=="out"))
  print(c("# of output files",n.output.files))
  
  project.path <- dependency.out$project.path[1]
  
  dependency.out$source.git <- NA
  try({
    dependency.out$source.git <- paste(git.info(as.character(dependency.out$path[1]),file.path(dependency.out$source.file.path[1],dependency.out$source.file)[1])[1:5],collapse=" ")
    
  })	
  dependency.out$source.mod.time <- as.character(file.info(file.path(dependency.out$source.file.path[1],dependency.out$source.file[1]))$mtime)
  
  dependency.out$source.hash <-   Digest(file=file.path(dependency.out$source.file.path[1],dependency.out$source.file[1]),serialize=FALSE) 
  
  
  for(dep.row.iter in 1:nrow(dependency.out)){
    
    target.file <- file.path(dependency.out$target.path[dep.row.iter],dependency.out$target.file[dep.row.iter])
    
    #		print(target.file)
    
    dependency.out$target.hash[dep.row.iter] <- Digest(file=as.character(target.file),serialize=FALSE)
    
    dependency.out$target.mod.time[dep.row.iter] <- as.character(file.info(as.character(target.file))$mtime)
    
  }


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
    
    if(source_info$option$git){
    	try({	git.add(project.path,file.path(dependency.file))	})
    }
       
    
  }
  
  
  print(paste("Completed",source_info$file[["file"]]))
  
  return(dependency.out)	
  
  
}
