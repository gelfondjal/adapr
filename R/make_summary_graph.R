#' Make.summary graph of projects based on files in dependency directory
#' @param dependency.dir Dependency directory
#' @param dependency.object Dependency.data
#' @param plot.graph Logical to plot graph or not
#' @details Only take dependency directory XOR dependency.data
#' @export
Make.summary.graph <- function(dependency.dir=NULL,dependency.object=NULL,plot.graph=FALSE){
  
  #equire(igraph)
  
  if(is.null(dependency.object)){
    
    trees <- Harvest.trees(dependency.dir)
    
    trees <- subset(trees,!is.na(dependency))
  }else{trees <- dependency.object}
  
  g.all <- Make.dependency.graph.obj(trees)
  
  suffixes <- gsub(".*\\.","",igraph::V(g.all)$name)  	
  
  outputs <- c("tex","Robj","pdf")
  data.types <- c("csv","txt")
  sources <- c("R")
  
  file.class <- rbind(cbind("output",outputs),cbind("data",data.types),cbind("source",sources))
  file.type.df <- data.frame(file.type=file.class[,1],suffix=file.class[,2])
  
  shape.vector <- c("circle","square","square")
  names(shape.vector) <- c("data","source","output")
  
  
  file.type.df$shape <- shape.vector[as.character(file.type.df$file.type)]
  
  
  all.file.info <- Condense.file.info(trees)
  
  all.file.info$fullname.abbr <- file.path(all.file.info$path.abbr,all.file.info$file)
  
  all.file.info$time <- as.POSIXct(all.file.info$time)
  
  all.file.info$rank <- rank(all.file.info$tim)
  
  #igraph::V(g.all)$color <- file.type.df$color[match(suffixes,file.type.df$suffix)]
  
  igraph::V(g.all)$shape <- file.type.df$shape[match(suffixes,as.character(file.type.df$suffix))]
  
  igraph::V(g.all)$color <- "blue"
  
  igraph::V(g.all)$mod.time <-  all.file.info$time[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  igraph::V(g.all)$run.time <-  as.POSIXct(all.file.info$source.run.time[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)])
  
  igraph::V(g.all)$file.class <-  all.file.info$file.class[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  igraph::V(g.all)$file.hash <-  all.file.info$file.hash[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  igraph::V(g.all)$file <-  all.file.info$file[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  igraph::V(g.all)$path <-  all.file.info$path[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  
  igraph::V(g.all)$description <-  all.file.info$description[match(igraph::V(g.all)$name,all.file.info$fullname.abbr)]
  
  igraph::V(g.all)$shape <- ifelse(is.na(igraph::V(g.all)$shape),"circle",igraph::V(g.all)$shape)
  
  #plot(g.all,main="Dependencies",vertex.label=get.vertex.attribute(g.all,"name"))
  
  tree.layout <- igraph::layout.sugiyama(g.all,attributes="all")
  
  
  
  g2 <- tree.layout$extd_graph
  suffixes <-  gsub(".*\\.","",igraph::V(g2)$name)
  file.type.df <- rbind(file.type.df,data.frame(file.type="dummy",suffix="",shape="circle"))
  
  igraph::V(g2)$shape <- file.type.df$shape[match(suffixes,as.character(file.type.df$suffix))]
  
  igraph::V(g2)$shape <- ifelse(is.na(igraph::V(g2)$shape),"circle",igraph::V(g2)$shape)
  
  igraph::V(g2)$color <-  "blue"
  
  igraph::V(g2)$color <- ifelse(is.na(igraph::V(g2)$color),"black",igraph::V(g2)$color)
  
  g2$layout[,2] <- g2$layout[,2]+ifelse(!is.na(igraph::V(g2)$name),stats::runif(length(g2$layout[,2]),min=-0.25,max=0.25),0)
  
  if(plot.graph){
    
    graphics::plot(g2,vertex.size=ifelse(!is.na(igraph::V(g2)$name),10,1),main="Analytical Dependencies",vertex.label.dist=0.25,vertex.label.color="black")
    
    graphics::par(mar=c(14,5,5,10))
    
    graphics::plot(all.file.info$rank,all.file.info$time,col="blue",pch=19,main="File Modification Time",ylab="Modification Time",xlab="Rank")
    
    graphics::points(all.file.info$rank,all.file.info$time,pch=1)
    
    #image.plot( zlim=c(1,max(all.file.info$rank)) ,add=TRUE,col=rainbow(256),legend.only=TRUE,axes=FALSE,ylab="")
    
  }
  
  #legend("topright",legend=c("Older Files","New files"),pch=15,col=c("blue","red"))
  
  
  
  return(g.all)
  
} # END: make.summary.graph