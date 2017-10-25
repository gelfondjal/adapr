#' Produces script only subgraph
#' @param project.id Project id
#' @param plotTF logical to plot subgraph
#' @details For use with getDepSubgraph 
#' @return list with subgraph in igraph format, layout for plottingss
#' @export
#' @examples 
#'\dontrun{
#'  subGraph <- scriptSubgraph(project.id=getProject())
#'  plot(subGraph[[1]],vertex.label=basename(V(subGraph[[1]])$name),layout=subGraph[[2]])
#'} 
#' 
scriptSubgraph <- function(project.id=getProject(),plotTF=FALSE){
  
  si <- pullSourceInfo(project.id)
  projinfo <- getProjectInfoSI(si)
  projgraph <- projinfo$graph
  
  sources <- unique(projinfo$tree$source.file)
  
  vertexnames <- subset(projinfo$all.files,file %in%sources)$fullname.abbr
  
 # if(length(vertexnames)==1){stop("scriptSubgraph: Only 1 R script")}
  
  # Get all script output files
  
  inedges <- igraph::adjacent_vertices(projgraph, vertexnames,"out")
  inedges <- lapply(inedges,function(x){return(attr(x,"name"))})
  inedges <- plyr::ldply(inedges,as.data.frame)
  names(inedges) <- c("to","from")
  
  dfgraph <- igraph::as_data_frame(projgraph,what="edges")
  dfgraph$to <- plyr::mapvalues(dfgraph$to,as.character(inedges$from),as.character(inedges$to),warn_missing = FALSE)
  dfgraph$from <- plyr::mapvalues(dfgraph$from,as.character(inedges$from),as.character(inedges$to),warn_missing = FALSE)
  dfoo <- igraph::graph_from_data_frame(dfgraph)
  
  # Remove output files from graph
  
  graph2 <- igraph::simplify(igraph::delete_vertices(dfoo,setdiff(igraph::V(dfoo)$name,vertexnames)))
  lo <- igraph::layout.sugiyama(projgraph)
  
  tp <- function(x){
    
    x <- x[,2:1]
    
    x[,1] <- max(x[,1])- x[,1]
    
    return(x)
  }
  
  longgraph <- NULL
  isg <- graph2
  isgdf <- igraph::as_data_frame(isg)
  noedges <- 0
  
  # Sometimes No Edges can mess up graph layouts
  
  if(nrow(isgdf)==0){
    noedges <- 1
    isgdf <- igraph::as_data_frame(igraph::graph.data.frame(data.frame(from=vertexnames[1],to=vertexnames[1])))
  }
  # Sometimes 1 Vertex can mess up graph layouts
    
  if(length(vertexnames)==1){
    
    dfo <- data.frame(v=vertexnames[1],x=0,y=0)	
    dfo <- merge(dfo,subset(projinfo$all.files,select=c("fullname.abbr","fullname","description")),by.x="v",by.y="fullname.abbr")
    dfo <- matrix(1,1,2)
    
  }else{
    dfo <- tp(igraph::layout.sugiyama(isg)$layout)
  
  }
  if(plotTF){graphics::plot(isg,vertex.label=basename(igraph::V(isg)$name),layout=dfo)}
  
  return(list(subgraph=isg,layout=dfo))
  
}   # End subgraph
#' Produces subgraph of dependencies of R script
#' @param rscript R script name
#' @param project.id Project graph
#' @param plotTF logical to plot or not
#' @details Only needs one or the other argument. 
#' @return list with subgraph in igraph format, data frame format, and layout for plottingss
#' @export
#' @examples 
#'\dontrun{
#'  subGraph <- scriptSubgraph(project.id=getProject())
#'  plot(subGraph[[1]],vertex.label=basename(igraph::V(subGraph[[1]])$name),layout=subGraph[[2]])
#'} 
#' 
getDepSubgraph <- function(rscript,project.id=getProject(),plotTF=FALSE){
  
  subGraph <- scriptSubgraph(project.id,plotTF=FALSE)
  
  endVertex <- igraph::V(subGraph$subgraph)$name[basename(igraph::V(subGraph$subgraph)$name)==rscript]
  
  upStream <- unique(getUpstream(endVertex,subGraph$subgraph))
  
  igraph::V(subGraph$subgraph)$color <- ifelse(igraph::V(subGraph$subgraph)$name %in% upStream,"red","azure3")
  
  igraph::V(subGraph$subgraph)$label.color <- ifelse(igraph::V(subGraph$subgraph)$name %in% upStream,"black","blue")
  
  igraph::E(subGraph$subgraph)$color <- "azure2"
  
  igraph::E(subGraph$subgraph)$arrow.size <- 0.25
  
  if(length(igraph::E(subGraph$subgraph)$arrow.size)){
  
  try({
  
  fromTo <- igraph::ends(subGraph$subgraph,1:length(igraph::E(subGraph$subgraph)))
  
  hotEdges <- which((fromTo[,1] %in% upStream)&(fromTo[,2] %in% upStream))
  
  igraph::E(subGraph$subgraph)$color[hotEdges] <- "red"
  
  })
  }# if nonzero edges
  
  if(plotTF){
    graphics::plot(subGraph$subgraph,vertex.label=basename(igraph::V(subGraph$subgraph)$name),
                   layout=subGraph$layout,main=paste("Partial Synch for",rscript,"in",project.id,"project"))
  }
  return(upStream)
  
  
}
#' Identifies dependencies in a DAG
#' @param endVertex vertex
#' @param isg Project graph
#' @details Lower level function. Uses recursion, may contain non-unique vertices. 
#' @return list with subgraph in igraph format, data frame format, and layout for plottingss
#' @export
getUpstream <- function(endVertex,isg){
  if(!igraph::is.dag(isg)){stop("Error in getUpstream: not a DAG")}
  
  verts <- names(igraph::adjacent_vertices(isg, endVertex,"in")[[1]])
  if(length(verts)==0){return(c(endVertex))}
  
  for(i in seq_along(verts)){
    verts <- unique(c(verts,getUpstream(verts[i],isg)))
  }
  
  verts <- unique(c(verts,endVertex))
  
  return(verts)
  
}
