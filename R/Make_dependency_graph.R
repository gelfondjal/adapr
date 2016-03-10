#' Creates an graph object from a dependency object
#' @param dependency.out Dependency object(s) to make graph out of
#' @return graph object of project/program dependencies
#' @export
#' 
Make.dependency.graph.obj <- function(dependency.out){
  
  # make dependency graph using dependency.out object
  # return dependency graph
  
  
  file.info <- Condense.file.info(dependency.out)
  
  
  dependencies <- dependency.out
  
  dependencies$target.fullname <- file.path(dependencies$target.path,dependencies$target.file)
  dependencies$source.fullname <- file.path(dependencies$source.file.path,dependencies$source.file)
  
  
  dependencies <- merge(dependencies,subset(file.info,file.class="source",select=c("fullname","unique.file")),by.x="source.fullname",by.y="fullname",all.x=TRUE)
  
  dependencies <- plyr::rename(dependencies,replace=c("unique.file"="source.fullname.abbr"))
  
  dependencies <- merge(dependencies,subset(file.info,file.class="target",select=c("fullname","unique.file")),by.x="target.fullname",by.y="fullname",all.x=TRUE)
  
  dependencies <- plyr::rename(dependencies,replace=c("unique.file"="target.fullname.abbr"))
  
  
  
  # identify unique filenames as levels
  
  levels <- file.info$fullname.abbr
  
  edge.mat <- cbind(as.numeric(factor(dependencies$"source.fullname.abbr",levels=levels)),as.numeric(factor(dependencies$"target.fullname.abbr",levels=levels)))
  
  # properly order dependcies in 2 column matrix based on in/out value
  
  edge.mat2 <- cbind(ifelse(dependencies$dependency=="in",edge.mat[,2],edge.mat[,1]),ifelse(dependencies$dependency=="in",edge.mat[,1],edge.mat[,2]))
  
  graph.out <- graph(t(edge.mat2),n=max(edge.mat2))
  
  df <- data.frame(from=ifelse(dependencies$dependency=="in",dependencies$"target.fullname.abbr",dependencies$"source.fullname.abbr"),to=ifelse(dependencies$dependency=="in",dependencies$"source.fullname.abbr",dependencies$"target.fullname.abbr"))
  
  g <- graph.data.frame(df,directed=TRUE)
  
  return(g)
  
}
