#' Tests the synchrony of files in dependency tree
#' @param source_info source_info containing project information 
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' @details Not usually direct use. See syncProject() and syncTestProject().
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' syncTestSI(source_info)
#'} 
syncTestSI <- function(source_info,plotl0=FALSE){
  
  project_info <- getProjectInfoSI(source_info)
  
  return(syncTest(project_info$graph,project_info$tree,plotl=plotl0))}
#' Tests the synchrony of files in dependency tree
#' @param project.id is project to test the synchrony of
#' @return list with logical indicated whether project is synchronized or not and details about synchrony
#' @export
#' @examples 
#'\dontrun{
#' syncTestProject("adaprHome")
#'} 
#' 
#' 
syncTestProject <- function(project.id=getProject()){
  
  source_info <- pullSourceInfo(project.id)
  
  project_info <- getProjectInfoSI(source_info)
  
  return(syncTest(project_info$graph,project_info$tree,plotl=FALSE))}
#' Checks the synchronization project and runs scripts needed for synchronization
#' @param project.id is project to synchronize
#' @param ask logical whether to report estimated run time prior to execution
#' @return Character string with message about success for synchronization
#' @export
#' @examples 
#'\dontrun{
#' syncProject("adaprHome")
#'} 
#' 
#' 
#' 
syncProject <- function (project.id = getProject(), ask = FALSE) 
{
  source_info <- pullSourceInfo(project.id)
  test.sync0 <- syncTestSI(source_info)
  if (test.sync0$synchronize) {
    text <- paste(project.id, "Already synchonized")
    last.prog <- " "
  }  else {
    syncer <- sourceSyncSILoad(source_info)
    run.times <- syncer$run.times
    idSync.out <- syncer$idSync.out
    sync.out <- syncer$sync.out
    wait0 <- ceiling(as.numeric(sum(run.times$last.run.time.sec)) * 
                       1.5)
    if (ask) {
      print(idSync.out)
      execute <- readline(paste("Synchronizing project", 
                                project.id, "will take about", ceiling(wait0/60), 
                                "minutes. Proceed? y/n"))
      if (substring(tolower(execute), 1, 1) != "y") {
        print(paste("Not executed:", project.id, "will take about", 
                    ceiling(wait0/60), "minutes. Proceed? y/n"))
        out <- merge(subset(idSync.out, select = c("file", 
                                                   "run.order")), run.times, by.x = "file", by.y = "source.file")
        out <- out[order(out$run.order), ]
        return(out)
      }
    }
    
    
    ggproject <- createProgramGraph(project.id,testSync=FALSE)
    ggproject$vertex$synccolor <- factor(ggproject$vertex$synccolor,levels=c("Synchronized","Not Synchronized","Running"))
    
    
    ggproject$vertex$synccolor[basename(ggproject$vertex$fullname) %in% idSync.out$file] <- "Not Synchronized"
    
    n.scripts.to.sync <- nrow(idSync.out)
    
    startmessage <- paste("Start sync approximate Time:", 
                          wait0, "seconds", n.scripts.to.sync, "scripts")
    full.time <- wait0
    last.prog <- "Go"
    source.iter <- 0
    
    print(rapidPlot(ggproject,startmessage))   
    
    
    while ((last.prog != "") & source.iter < nrow(idSync.out)) {
      
      source.iter <- source.iter + 1
      
      namer <- basename(ggproject$vertex$fullname)
      
      ggproject$vertex$synccolor[namer==idSync.out$file[source.iter]] <- "Running"
      
      
      messageOut <- paste(idSync.out$file[source.iter], paste0(source.iter, 
                                                               "/", n.scripts.to.sync), wait0, "seconds remaining")
      
      
      print(rapidPlot(ggproject,message=messageOut))    
      
      last.prog <- ""
      try({
        devtools::clean_source(file.path(idSync.out$path[source.iter], 
                                         idSync.out$file[source.iter]))
        last.prog <- idSync.out$file[source.iter]
        
        ggproject$vertex$synccolor[namer==idSync.out$file[source.iter]] <- "Synchronized"
        
      })
      #print(run.times$last.run.time.sec[source.iter])
      
      
      
      wait0 <- wait0 - run.times$last.run.time.sec[source.iter]
    }
    
    
    print(rapidPlot(ggproject,message="Stopped"))    
    
    
    failure.script <- ifelse(source.iter <= nrow(idSync.out), 
                             as.character(idSync.out$file[source.iter]), "")
  }
  text <- paste("Sync successful for", project.id)
  if (last.prog == "") {
    text <- paste(last.prog, "failed sync for", project.id, 
                  "Script that failed:", failure.script)
  }
  else {
    if (getAdaprOptions()$git == "TRUE") {
      try({
        commitProject(paste("adapr Sync at", Sys.time()), 
                      project.id)
      })
    }
  }
  return(text)
}


#' Partial project synchronization of dependencies to an Rscript. Runs only scripts needed for synchronization.
#' @param rscript script to synchronize the output of
#' @param project.id is project to synchronize
#' @param ask logical whether to report estimated run time prior to execution
#' @return Character string with message about success for synchronization
#' @export
#' @examples 
#'\dontrun{
#' syncTrunk("read_data.R","adaprHome")
#'} 
#' 
#' 
#' 
syncTrunk <- function(rscript,project.id=getProject(),ask=FALSE){
  
  source_info <- pullSourceInfo(project.id)
 # test.sync0 <- syncTestSI(source_info)
  syncer <- sourceSyncSILoad(source_info)
  
  if(syncer$sync.out$synchronized){
    text <- paste(project.id,"Already synchonized")
    last.prog <- " "
  }else{
    
#    syncer <- sourceSyncSILoad(source_info)
    subber <- getDepSubgraph(rscript,project.id=getProject(),FALSE)
    
    run.times <- subset(syncer$run.times,syncer$run.times$source.file %in% basename(subber))
    
    idSync.out <- subset(syncer$idSync.out,syncer$idSync.out$file %in% basename(subber))
    
    sync.out <- syncer$sync.out
    
    wait0<-ceiling(as.numeric(sum( run.times$last.run.time.sec))*1.5)
    
    if(ask){
      
      print(idSync.out)
      
      execute <- readline(paste("Partially synchronizing project",project.id,"will take about",ceiling(wait0/60),"minutes. Proceed? y/n"))
      
      if(substring(tolower(execute),1,1)!="y"){
        print(paste("Not executed:",project.id,"will take about",ceiling(wait0/60),"minutes. Proceed? y/n"))
        out <- merge(subset(idSync.out,select=c("file","run.order")),run.times,by.x="file",by.y="source.file")
        out <- out[order(out$run.order),]
        return(out)
        
      }
      
    }
    #progress <- shiny::Progress$new()
    #on.exit(progress$close())
    
    n.scripts.to.sync <- nrow(idSync.out)
    
    startmessage <- paste("Start sync approximate Time:", wait0, "seconds",n.scripts.to.sync,"scripts")
    
    #shiny::withProgress(message=startmessage, expr={
    
    #        progress$set(message=paste("Start sync",startmessage),value=0)
    
    
    
    full.time <- wait0
    last.prog <- "Go"
    source.iter <- 0
    while((last.prog != "") & source.iter < nrow(idSync.out)){
      source.iter <- source.iter + 1
      
      warning(paste(idSync.out$file[source.iter],paste0(source.iter,"/",n.scripts.to.sync),wait0,"seconds remaining"))
      
      
      last.prog <- ""
      
      
      try({
        devtools::clean_source(file.path(idSync.out$path[source.iter],idSync.out$file[source.iter]))
        
        #run.program(input$project.id,idSync.out$file[source.iter],TRUE)  
        
        #Sys.sleep(3)
        
        last.prog <- idSync.out$file[source.iter]
        
        
      })
      
      
      print(run.times$last.run.time.sec[source.iter] )
      
      wait0 <- wait0 - run.times$last.run.time.sec[source.iter] 
      
    }
    failure.script <- ifelse(source.iter <= nrow(idSync.out),as.character(idSync.out$file[source.iter]),"")
    
  }#END progress bar
  
  
  text<-paste("Sync successful for",project.id)
  if(last.prog==""){    
    text<-paste(last.prog,"failed sync for",project.id,"Script that failed:",failure.script)
  }else{
    
    if(getAdaprOptions()$git=="TRUE"){commitProject(paste("adapr Sync at",Sys.time()),project.id)}
  }
  return(text)
  
}
