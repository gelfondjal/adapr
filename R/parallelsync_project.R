#' Uses DOSNOW package for parallel syncrhonization of project.
#' @param project.id Project to synchronize.
#' @param n.cores Number of cores to use. Should be >1, but less than number of logical CPUs.
#' @return data.frame with success/failure status.
#' @export
#' @details Experimental. See also sync.project() and synctest.project(). Uses Results/tree_controller.R directory to pass work/completion data between nodes.
#' @examples 
#'\dontrun{
#' parallelsync.project("adaprHome")
#'} 
#'
#'

parallelSync.project <- function(project.id = get.project(),n.cores=2){

graphdat <- graph.project(project.id)

project.plot <- graphdat$ggplot

# Identify files with no dependencies for running

findNodependency <- function(graphdat,completed=""){
  
  isg <- igraph::graph_from_data_frame(graphdat$edges) #  data.frame()graphdat$edges
  
  
  sfiles <- setdiff(as.character(graphdat$vertex$v),completed)
  
  # sfiles minus completed!
  
  adjlist <- lapply(igraph::as_adj_list(isg,mode="in"),names)
  
  for(i in names(adjlist)){if(!(i %in% sfiles)){adjlist[[i]]<-NULL}}                  
  
  adjlist <-  lapply(adjlist,function(x){intersect(x,sfiles)})
  
  adjlist0 <- adjlist
  
  for(i in names(adjlist)){
    
    if(length(adjlist0[[i]])>0){adjlist0[[i]]<-NULL}
    
  }
  
  hotlist <- setdiff(names(adjlist0),completed)
  
  return(hotlist)
  
}

nodeps <- findNodependency(graphdat)
# Initialize broadcast files

checkdir <- pull_source_info(project.id)$results.dir

completedfile <- file.path(checkdir,"completed.csv")
workfile <- file.path(checkdir,"working.csv")
todofile <- file.path(checkdir,"todo.csv")
graphfile <- file.path(checkdir,"graph.Rdata")


workdat <- data.frame(rscript="",compute.node=NA)[-1,]
todo <- data.frame(rscript=subset(graphdat$v,graphdat$v$synccolor!="Synchronized")$v)

if(nrow(subset(graphdat$v,graphdat$v$synccolor=="Synchronized"))){
  completed <-data.frame(rscript=subset(graphdat$v,graphdat$v$synccolor=="Synchronized")$v,compute.node=0)
}else{
  
  completed <-data.frame(rscript="",compute.node=0)[-1,]
  
}

utils::write.csv(completed,completedfile,row.names=FALSE)
utils::write.csv(workdat,workfile,row.names=FALSE)
utils::write.csv(todo,todofile,row.names=FALSE)
save(graphdat,findNodependency,file=graphfile)



load(graphfile)

nodes <- data.frame(compute.node=1:n.cores,completedfile,workfile,todofile,
                    graphfile,
                    stringsAsFactors = FALSE)


clust <- snow::makeCluster(n.cores,type="SOCK")
            
doSNOW::registerDoSNOW(clust)


presult <- plyr::ddply(nodes,"compute.node",function(x){
  
  
  scriptRun <- NULL
  
  load(x$graphfile[1]) # load graph info
  
  todo <- utils::read.csv(x$todofile[1],as.is=TRUE)
  test <- nrow(todo)>0
  
  while(test){
    
    complist <- utils::read.csv(x$completedfile[1],as.is=TRUE)$rscript
    worklist <- utils::read.csv(x$workfile[1],as.is=TRUE)
    
    todoNow <- findNodependency(graphdat,completed=complist)
    
    todoNow <- intersect(todoNow,todo$rscript)
    
    success <- TRUE
    
    if(length(todoNow)>0){
      
      todoNow <- todoNow[1]
      
      utils::write.csv(subset(todo,todo$rscript!=todoNow),
                       x$todofile[1],row.names=FALSE)
      
      workorder <- data.frame(rscript=todoNow,compute.node=x$compute.node[1])
      
      worklist <- rbind(subset(worklist,select=c("rscript","compute.node")),workorder)
      
      utils::write.csv(worklist,x$workfile[1],row.names=FALSE)
      
      Sys.sleep(1)
      
      workdf0 <- utils::read.csv(x$workfile[1],as.is=TRUE)
      workdf <- subset(workdf0,workdf0$rscript==todoNow)
      workdf <- workdf[which.min(workdf$compute.node),]
      
      workdf <- subset(workdf,workdf$compute.node==x$compute.node[1])
      
      
      
      if(nrow(workdf)>0){
        
        
        success <- FALSE
        
        fullname <- subset(graphdat$vertex,graphdat$vertex$v==todoNow)$fullname
        
        try({
          
          devtools::clean_source(fullname,quiet=TRUE)
          
          
          success <- TRUE
          
        })
        
        complist <- utils::read.csv(x$completedfile[1],as.is=TRUE)
        complist <- rbind(complist,data.frame(rscript=todoNow,
                                              compute.node=x$compute.node[1]))
        utils::write.csv(complist,x$completedfile[1],row.names=FALSE) 
        
        workdf0 <- utils::read.csv(x$workfile[1],as.is=TRUE)
        
        utils::write.csv(subset(workdf0,workdf0$rscript != todoNow),
                         x$workfile[1],row.names=FALSE)
        
        scriptRun <- rbind(scriptRun,
                           data.frame(rscript=todoNow,
                                      success=success))
        
        
      }#execute
      
      
      
    }# available file
    
    Sys.sleep(x$compute.node[1]*0.25)
    
    if(!success){
      
      utils::write.csv(todo[1,][-1],x$todofile[1],row.names=FALSE)
      
    }
    
    todo <- utils::read.csv(x$todofile[1],as.is=TRUE)
    test <- nrow(todo)>0
    
    
  }               
  
  return(scriptRun)
  
  
},.parallel = TRUE)

	
snow::stopCluster(clust)

return(presult)


}
