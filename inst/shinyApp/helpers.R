git.configure.test <-
  function ()
  {
    
    
    
    config.out <- 99
    try({
      
      git_binary_path <- git_path(NULL)
      config.out  <- system2(git_binary_path, paste("config --global user.name"))
      
    })
    
    return(config.out)
  }


git.configure.test()



temp <- git.configure.test()



get_filelist <- function(project.id){
	
	si <- pull_source_info(project.id)
	
	files <- get.project.info.si(si)$all.files
	
    paths <- gsub(file.path(si$project.path,""),"",fixed=TRUE,files$fullname)
    descriptions <- files$description
	
	results <- subset(data.frame(path=paths,description=descriptions,stringsAsFactors=FALSE),grepl("^Results",path))
	
  results$path <-  gsub(file.path("Results",""),"",fixed=TRUE,results$path)
  
	return(results)
	
}


runtimes.source.sync.si <- function(source_info) 
{
  project_info <- get.project.info.si(source_info)
  sync.out <- Sync.test.pi(project_info)
  if (sync.out$synchronized) {
    print(paste("Project synchronized"))
    return(NULL)
  }
  ID.sync.out <- sync.out$sources.to.sync
  if (nrow(ID.sync.out) == 0) {
    warning("There is nothing to run")
  }
  tree.to.run <- subset(project_info$tree, source.file %in% 
                          ID.sync.out$file)
  sync.out <- sync.test.si(source_info)
  propagated.names <- V(sync.out$propagated.graph)$name[V(sync.out$propagated.graph)$synced =="No"]
  
  run.times <- ddply(tree.to.run, "source.file", function(x) {
    last.run.time <- max(as.POSIXct(x$target.mod.time) - as.POSIXct(x$source.run.time), na.rm = TRUE)
    return(data.frame(last.run.time.sec = last.run.time))
  })
  
  return(run.times)
  
}