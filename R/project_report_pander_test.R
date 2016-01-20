#' Make plot of network within html documents.
#' Summarize all programs.

project_reporter_pander_0<-

function (source_info, graph.width = 960, graph.height = 500) 
{
  library(pander)
  library(devtools)
  if(!require("rCharts")){install_github('rCharts','ramnathv')}
  library(rCharts)
  library(plyr)
  
  #See if Pander is installed
  setwd(source_info$results.dir)
  panderOptions("table.split.table",Inf)
  evalsOptions("cache.dir",source_info$results.dir)

  #rm("myReport",env=parent.frame())
  myReport <<- Pandoc$new()
  author <- ""
#  try({
#  git_binary_path <- git_path(NULL)
#  author  <- system2(git_binary_path, paste("config --global user.name"),stdout=TRUE)
#  })
#  try(pandocinstalled<-myReport$export("test",open=FALSE))
#  if(!exists("pandocinstalled")){
   if(FALSE){
    project_reporter(source_info, graph.width , graph.height )
    return("Error: Pandoc is not installed on this computer")}else {
  #require(R2HTML)
  project.info <- get.project.info.si(source_info)
  project.graph <- project.info$graph
  programs <- subset(project.info$tree,!duplicated(source.file),select=c("source.file","source.file.path","source.file.description"))
  
  programs$source.file.fullname <- file.path(programs$source.file.path,programs$source.file)
  
  run.times <- ddply(project.info$tree,"source.file",function(x){
    
    last.run.time <- max(as.POSIXct(x$target.mod.time)-as.POSIXct(x$source.run.time),na.rm=TRUE)
    
    
    return(data.frame(last.run.time.sec=last.run.time))
    
    
  })
  
  tab.out <- merge(programs,run.times,by="source.file")
  
  tab.out$source.link <- make.hyperlink(tab.out$source.file.fullname,tab.out$source.file)
  
  program.split <- split(project.info$tree,project.info$tree$source.file)
  
  summaries.out <- lapply(program.split, program.io.table)
  outputs <- list()
  for (source.iter in names(summaries.out)) {
    temp <- summaries.out[[source.iter]]
    temp$File <- make.hyperlink(temp$Fullname, temp$File)
    outputs[[source.iter]] <- subset(temp, select = c("IO", 
                                                      "File", "Description"))
  }

  
tab.out0 <- subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec"))

tab2 <<- data.frame(tab.out0[,2:3])

print(tab2)

colnames(tab2) <- make.names(names(tab2))

tab2 <- table(1:13,11:23)

myReport$add(tab2)

myReport$title<-paste(source_info$project.id,'Project Summary')
myReport$author<-paste("IT2","notme")

myReport$format<-'html'
myReport$export("project_summary",open=FALSE)
#  R2HTML::HTMLStop()
  }
}