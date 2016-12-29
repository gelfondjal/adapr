#' Tests the synchrony of files in dependency tree
#' @param source_info source_info containing project information 
#' @param plotl0 Logical indicated whether to plot the updated files
#' @return list or logical indicated whether project is synchronized or not
#' @export
#' @details Not usually direct use. See sync.project() and synctest.project().
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' sync.test.si(source_info)
#'} 
sync.test.si <- function(source_info,plotl0=FALSE){
  
  project_info <- get.project.info.si(source_info)
  
  return(Sync.test(project_info$graph,project_info$tree,plotl=plotl0))}

#' Tests the synchrony of files in dependency tree
#' @param project.id is project to test the synchrony of
#' @return list with logical indicated whether project is synchronized or not and details about synchrony
#' @export
#' @examples 
#'\dontrun{
#' synctest.project("adaprHome")
#'} 
#' 
#' 
synctest.project <- function(project.id=get.project()){
  
  source_info <- pull_source_info(project.id)
  
  project_info <- get.project.info.si(source_info)
  
  return(Sync.test(project_info$graph,project_info$tree,plotl=FALSE))}


#' Checks the synchronization project and runs scripts needed for synchronization
#' @param project.id is project to synchronize
#' @param ask logical whether to report estimated run time prior to execution
#' @return Character string with message about success for synchronization
#' @export
#' @examples 
#'\dontrun{
#' sync.project("adaprHome")
#'} 
#' 
#' 
#' 

sync.project <- function(project.id=get.project(),ask=FALSE){
source_info <- pull_source_info(project.id)
test.sync0 <- sync.test.si(source_info)

if(test.sync0$synchronize){
  text <- paste(project.id,"Already synchonized")
  last.prog <- " "
 }else{
  
  syncer <- source_sync_si_load(source_info)
  
  run.times <- syncer$run.times
  
  ID.sync.out <- syncer$ID.sync.out
  
  sync.out <- syncer$sync.out
  
  wait0<-ceiling(as.numeric(sum( run.times$last.run.time.sec))*1.5)
  
  if(ask){
  
    print(ID.sync.out)
    
    execute <- readline(paste("Synchronizing project",project.id,"will take about",ceiling(wait0/60),"minutes. Proceed? y/n"))
    
    if(substring(tolower(execute),1,1)!="y"){
      print(paste("Not executed:",project.id,"will take about",ceiling(wait0/60),"minutes. Proceed? y/n"))
      out <- merge(subset(ID.sync.out,select=c("file","run.order")),run.times,by.x="file",by.y="source.file")
      out <- out[order(out$run.order),]
      return(out)
      
    }
  
  }
  #progress <- shiny::Progress$new()
  #on.exit(progress$close())
  
  n.scripts.to.sync <- nrow(ID.sync.out)
  
  startmessage <- paste("Start sync approximate Time:", wait0, "seconds",n.scripts.to.sync,"scripts")
  
  #shiny::withProgress(message=startmessage, expr={
    
    #        progress$set(message=paste("Start sync",startmessage),value=0)
    
    
    
    full.time <- wait0
    last.prog <- "Go"
    source.iter <- 0
    while((last.prog != "") & source.iter < nrow(ID.sync.out)){
      source.iter <- source.iter + 1
      #for (source.iter in 1:nrow(ID.sync.out)) {
      
      warning(paste(ID.sync.out$file[source.iter],paste0(source.iter,"/",n.scripts.to.sync),wait0,"seconds remaining"))
      
     
      last.prog <- ""
      
      
      try({
        devtools::clean_source(file.path(ID.sync.out$path[source.iter],ID.sync.out$file[source.iter]))
        
        #run.program(input$project.id,ID.sync.out$file[source.iter],TRUE)  
        
        #Sys.sleep(3)
        
        last.prog <- ID.sync.out$file[source.iter]
        
        
      })
      
     
      print(run.times$last.run.time.sec[source.iter] )
      
      wait0 <- wait0 - run.times$last.run.time.sec[source.iter] 
      

    }
    failure.script <- ifelse(source.iter <= nrow(ID.sync.out),as.character(ID.sync.out$file[source.iter]),"")
    
  }#END progress bar
  
 
  #withProgress(session,min=1,max=3,expr={
  #    setProgress(message = 'Synchronizing',detail=paste("Approximate Time:", wait, "seconds"),value=2)
  #  test.sync <- source.sync.si(source_info,run=TRUE,TRUE)
  #    setProgress(value=3)
  
  text<-paste("Sync successful for",project.id)
  if(last.prog==""){    
    text<-paste(last.prog,"failed sync for",project.id,"Check",failure.script)
  }

return(text)
  
}
  