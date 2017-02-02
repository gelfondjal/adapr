#' Uses DOSNOW package for parallel syncrhonization of project. Takes advantage of directed acyclic graph structure to run R script processes in parallel.
#' @param project.id Project to synchronize.
#' @param n.cores Number of cores to use. Should be >1, but less than number of logical CPUs.
#' @return data.frame with success/failure status.
#' @export
#' @details Experimental. See also monitorParallelSync.project(), sync.project() and synctest.project(). Uses Results/tree_controller.R directory to pass work/completion data between nodes.
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
  
  isg <- graphdat$rgrapher #  data.frame()graphdat$edges
  
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


#clust <- snow::makeCluster(n.cores,type="SOCK")
#parallel::registerDoSNOW(clust)

clust <- parallel::makeCluster(n.cores,type="SOCK")
doParallel::registerDoParallel(clust)

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

	
parallel::stopCluster(clust)

return(presult)


}






#' Track parallelSync.project while in progress
#' @param project.id Project to synchronize.
#' @param check.interval how many seconds to delay until last check
#' @return ggplot of project graph
#' @export
#' @details Must use separate R process from parallelSync.project(). Refreshes project plot with compute node labels are working or completed
#' @examples 
#'\dontrun{
#' monitorParallelSync.project("adaprHome")
#'} 
#'


monitorParallelSync.project <- function(project.id = get.project(),check.interval=5){
  
  graphdat <- graph.project(project.id)
  
  project.plot <- graphdat$ggplot
 
  # Read broadcast files
  
  checkdir <- pull_source_info(project.id)$results.dir
  
  completedfile <- file.path(checkdir,"completed.csv")
  workfile <- file.path(checkdir,"working.csv")
  todofile <- file.path(checkdir,"todo.csv")
  graphfile <- file.path(checkdir,"graph.Rdata")
  
  
  synccolors <- c("aquamarine3","darkorange2")
  names(synccolors) <- c("Synchronized", "Not Synchronized")
  
  vertexnames <- graphdat$vertex$v
  
  workdat <- data.frame(rscript="",compute.node=NA)[-1,]
  todo <- data.frame(rscript=subset(graphdat$v,graphdat$v$synccolor!="Synchronized")$v)
  
  if(nrow(subset(graphdat$v,graphdat$v$synccolor=="Synchronized"))){
    completed <-data.frame(rscript=subset(graphdat$v,graphdat$v$synccolor=="Synchronized")$v,compute.node=0)
  }else{
    
    completed <-data.frame(rscript="",compute.node=0)[-1,]
    
  }
  

  
  load(graphfile)
  
  
  dfo <- graphdat$vertex
  if(sum(!(c("x","y","v")  %in% names(dfo)))){
    
    x <- NULL
    y <- NULL
    v <- NULL
    
    stop("create_program_graph (adapr) error: cannot find vertex")
    
  }
  
  
  testWorking <- TRUE
  
   while(testWorking){
  
    todo <- utils::read.csv(todofile,as.is=TRUE)
    complist <- utils::read.csv(completedfile,as.is=TRUE)
    worklist <- utils::read.csv(workfile,as.is=TRUE)
    
    froms <- graphdat$edges 
      
    unsync.vertex <- unique(setdiff(subset(graphdat$vertex,graphdat$vertex$synccolor!="Synchronized")$v,complist$rscript))

    # check dfo namespace
    
    ranger <- range(dfo$x)
    
    span <- 0.25*abs(diff(ranger))
    
    horizontal.range <- c(ranger[1]-span,ranger[2]+span)
    
    rangery <- range(dfo$y)
    
    
    graph.height <- length(unique(dfo$y))
    
    graph.width <- length(unique(dfo$x))
    
      dotsize0 <- 10
      if(graph.height>5){dot.size <-1+10/graph.height}else{dot.size0 <- 10}              
      
      text.nudge0 <- 0.05*abs(diff(rangery))
      
      
      text.nudge0 <- dotsize0/20
      
      #if(graph.height>5){text.nudge0 <- 0.05 +text.nudge0/graph.height}              
      
      text.size0 <- 4
      
      if(graph.width>5){text.size0 <-2 + 2*text.size0/graph.width}              
      
      dfo$synccolor <- as.character(ifelse(dfo$v %in% unsync.vertex,"Not Synchronized","Synchronized"))
      
      dfo$synccolor <- factor(dfo$synccolor,levels=c("Synchronized","Not Synchronized"))
      
      proj.gg <- ggplot2::ggplot(dfo,ggplot2::aes(x=x,y=y,label=basename(as.character(v))))+
      ggplot2::geom_point(ggplot2::aes(colour=dfo$synccolor),size=dotsize0,alpha=0.7)+
      ggplot2::geom_point(shape = 1,size = dotsize0,colour = "grey70", stroke=2)+
      ggplot2::geom_text(vjust=-0.5,size=text.size0,color="black")
      
      proj.gg <- proj.gg + ggplot2::annotate(geom="label",x=dfo$x,y=dfo$y-ifelse(length(vertexnames)>1,0.125,0.1),label=dfo$description,size=text.size0)
      
      if(nrow(worklist)>0){
        
        dfw <- merge(worklist,dfo,by.x="rscript",by.y="v")
        
        proj.gg <- proj.gg + ggplot2::annotate(geom="label",x=dfw$x,y=dfw$y+ifelse(length(vertexnames)>1,0.125,0.1),label=dfw$compute.node,size=text.size0,color=synccolors[2])
        
        
      }
      
      
      if(nrow(complist)>0){
        
        dfc <- merge(complist,dfo,by.x="rscript",by.y="v")
        
        proj.gg <- proj.gg + ggplot2::annotate(geom="label",x=dfc$x,y=dfc$y+ifelse(length(vertexnames)>1,0.125,0.1),label=dfc$compute.node,size=text.size0,color=synccolors[1])
        
        
      }
      

      if(nrow(dfo)==1){
        
        proj.gg <- proj.gg + ggplot2::scale_y_continuous(limits=rangery)
        
      }
      
      
      testEdges <- !is.null(graphdat$edges)
      if(testEdges){testEdges <- nrow(graphdat$edges)>0 }
      
      if(testEdges){
        
        proj.gg <- proj.gg+ggplot2::annotate(geom="segment",x=froms$x,y=froms$y,xend=froms$x2,yend=froms$y2,
                                             arrow=ggplot2::arrow(length=ggplot2::unit(0.2,"cm"),type="closed"),alpha=0.5/ifelse(graph.width>5,5,1))
        
      }  
      proj.gg <- proj.gg +ggplot2::scale_x_continuous(limits=horizontal.range)+ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(),
                                                                                              axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),
                                                                                              axis.title.x=ggplot2::element_blank(),
                                                                                              axis.title.y=ggplot2::element_blank(),legend.position="bottom",
                                                                                              panel.background=ggplot2::element_blank(),panel.border=ggplot2::element_blank(),panel.grid.major=ggplot2::element_blank(),
                                                                                              panel.grid.minor=ggplot2::element_blank(),plot.background=ggplot2::element_blank())+ggplot2::ggtitle(paste(project.id,"- R Script Graph"))+ggplot2::theme(text=ggplot2::element_text(size=20))
      
      proj.gg <- proj.gg+ ggplot2::scale_color_manual(name = ggplot2::element_blank(), # or name = element_blank()
                                                      #labels = c("Synchronized", "Not Synchronized"),
                                                      values =synccolors)
      
      
      print(proj.gg)
      
      testWorking <- nrow(worklist)>0
      
      Sys.sleep(check.interval)
      
   }  # while working still nonempty
  
    return(proj.gg)
  
}




