#' Make plot of network within html documents.
#' Summarize all programs.
#' @param source_info Source information list
#' @param graph.width Sankey Plot dimensions
#' @param graph.height Sankey Plot dimensions
#' @details Assumes source_info in workspace
#' @export

project_reporter <- function(source_info,graph.width=960,graph.height=500){
  
  require(R2HTML)
  
  project.info <- get.project.info.si(source_info)
  
  project.graph <- project.info$graph
  
  
  E(project.graph)$weight = 0.1
  edgelist <- get.data.frame(project.graph)
  
  
  
  
  colnames(edgelist) <- c("source","target","value")
  
  #make character rather than numeric for proper functioning
  edgelist$source <- as.character(edgelist$source)
  edgelist$target <- as.character(edgelist$target)
  sankeyPlot <- rCharts$new()
  sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')
  sankeyPlot$set(
    data = edgelist,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = graph.width,
    height = graph.height
    
  )
  
  
  project.graph.file <- file.path(source_info$results.dir,'full_networks.html')
  
  sankeyPlot$save(project.graph.file,cdn=TRUE)
  
  
  support.names <- subset(project.info$all.files,description=="Support file")$fullname.abbr
  
  edgelist <- subset(edgelist,!(source%in%support.names)&!grepl("Session_info",edgelist$source,fixed=TRUE)&!grepl("Session_info",edgelist$target,fixed=TRUE))
  
  
  sankeyPlot <- rCharts$new()
  sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')
  sankeyPlot$set(
    data = edgelist,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = graph.width,
    height = graph.height
    
  )
  
  
  reduced.project.graph.file <- file.path(source_info$results.dir,'reduced_networks.html')
  
  sankeyPlot$save(reduced.project.graph.file,cdn=TRUE)
  
  
  
  
  
  
  
  
  programs <- subset(project.info$tree,!duplicated(source.file),select=c("source.file","source.file.path","source.file.description"))
  
  programs$source.file.fullname <- file.path(programs$source.file.path,programs$source.file)
  
  run.times <- ddply(project.info$tree,"source.file",function(x){
    
    last.run.time <- max(as.POSIXct(x$target.mod.time)-as.POSIXct(x$source.run.time),na.rm=TRUE)
    
    
    return(data.frame(last.run.time.sec=last.run.time))
    
    
  })
  
  tab.out <- merge(programs,run.times,by="source.file")
  
  tab.out$source.link <- make.hyperlink(tab.out$source.file.fullname,tab.out$source.file)
  
  sorted.names <- V(project.info$graph)$file[topological.sort(project.info$graph)]
  sorted.names <- sorted.names[sorted.names %in% tab.out$source.file]
  
  tab.out <- tab.out[match(sorted.names,tab.out$source.file),]
  
  program.split <- split(project.info$tree,project.info$tree$source.file)
  
  summaries.out <- lapply(program.split,program.io.table)
  
  
  
  outputs <- list()
  
  for(source.iter in names(summaries.out)){
    
    temp <- summaries.out[[source.iter]]
    
    temp$File <- make.hyperlink(temp$Fullname,temp$File)
    
    outputs[[source.iter]] <- subset(temp,select=c("IO","File","Description"))
    
    
  }
  
  
  
  
  HTMLStart(outdir = source_info$results.dir, filename = "project_summary", extension = "html", echo = FALSE, autobrowse = FALSE, HTMLframe = TRUE, withprompt = "HTML> ", CSSFile = "R2HTML.css", BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output")
  
  R2HTML::HTML(subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec")),caption=paste(source_info$project.id,"Project Summary"),captionalign="top")
  
  R2HTML::HTML(data.frame(Big_Graph=make.hyperlink(project.graph.file,"Project Graph")),align="center")
  R2HTML::HTML(data.frame(No_support_file_graph=make.hyperlink(reduced.project.graph.file,"Project Graph")),align="center")
  
  supports <- subset(rbind.fill(outputs),Description=="Support file")
  
  for(namer in names(outputs)){
    
    R2HTML::HTML(subset(outputs[[namer]],Description!="Support file"),caption=namer,captionalign="top")
    
  }
  
  R2HTML::HTML(subset(supports,!duplicated(File)),caption="Support files",captionalign="top")
  
  
  R2HTML::HTMLStop()
  
}
