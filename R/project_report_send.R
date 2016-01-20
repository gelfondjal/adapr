#' Make plot of network within html documents.
#' Summarize all programs.
#' Make a readme file at top project directory
#' copy to target.directory
#' Uses pander and pandoc unlike project_report
#' @param target.directory Location to send project report
#' @param source_info Source information list
#' @param send.data Logical to send data directory or not
#' @param graph.width Sankey Plot dimensions
#' @param graph.height Sankey Plot dimensions
#' @details Dose not assume source_info in workspace
#' @export
#' 
project_report_send <- function (target.directory=source_info$project.path,source_info,send.data=FALSE, graph.width = 960, graph.height = 500) 
{
  library(pander)
  library(devtools)
  if(!require("rCharts")){install_github('rCharts','ramnathv')}
  library(rCharts)
  library(plyr)
  
  make.relative.hyperlink <- function(files,links){
    
    files <- gsub("^/","",gsub(source_info$project.path,"",files,fixed=TRUE))
    
    link.command <- rep("", length(files))
    for (file.iter in 1:length(files)) {
      link.command[file.iter] <- paste0("<a href=\"file:", 
                                        files[file.iter], "\">", links[file.iter], "</a>")
    }
    return(link.command)
    
  }
  
  
  
  
  #See if Pander is installed
  setwd(source_info$results.dir)
  panderOptions("table.split.table",Inf)
  evalsOptions("cache.dir",source_info$results.dir)
  try({rm(myReport)})
  myReport <<- Pandoc$new()
  author <- ""
  try({
  git_binary_path <- git_path(NULL)
  author  <- system2(git_binary_path, paste("config --global user.name"),stdout=TRUE)
  })
  try(pandocinstalled<-myReport$export("test",open=FALSE))
  if(!exists("pandocinstalled")){
#   if(FALSE){
    project_reporter(source_info, graph.width , graph.height )
    return("Error: Pandoc is not installed on this computer")}else {
  #require(R2HTML)
  project.info <- get.project.info.si(source_info)
  project.graph <- project.info$graph
  E(project.graph)$weight = 0.1
  edgelist <- get.data.frame(project.graph)
  colnames(edgelist) <- c("source", "target", "value")
  edgelist$source <- as.character(edgelist$source)
  edgelist$target <- as.character(edgelist$target)
  sankeyPlot <- rCharts$new()
  sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey")
  sankeyPlot$set(data = edgelist, nodeWidth = 15, nodePadding = 10, 
                 layout = 32, width = graph.width, height = graph.height)
  project.graph.file <- file.path(source_info$results.dir, 
                                  "full_networks.html")
  sankeyPlot$save(project.graph.file, cdn = TRUE)
  support.names <- subset(project.info$all.files, description == 
                            "Support file")$fullname.abbr
  edgelist <- subset(edgelist, !(source %in% support.names) & 
                       !grepl("Session_info", edgelist$source, fixed = TRUE) & 
                       !grepl("Session_info", edgelist$target, fixed = TRUE))
  sankeyPlot <- rCharts$new()
  sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey")
  sankeyPlot$set(data = edgelist, nodeWidth = 15, nodePadding = 10, 
                 layout = 32, width = graph.width, height = graph.height)
  reduced.project.graph.file <- file.path(source_info$results.dir, 
                                          "reduced_networks.html")
  sankeyPlot$save(reduced.project.graph.file, cdn = TRUE)
  programs <- subset(project.info$tree, !duplicated(source.file), 
                     select = c("source.file", "source.file.path", "source.file.description"))
  programs$source.file.fullname <- file.path(programs$source.file.path, 
                                             programs$source.file)
  run.times <- ddply(project.info$tree, "source.file", function(x) {
    last.run.time <- max(as.POSIXct(x$target.mod.time) - 
                           as.POSIXct(x$source.run.time), na.rm = TRUE)
    return(data.frame(last.run.time.sec = last.run.time))
  })
  tab.out <- merge(programs, run.times, by = "source.file")
  tab.out$source.link <- make.relative.hyperlink(tab.out$source.file.fullname, 
                                        tab.out$source.file)
  sorted.names <- V(project.info$graph)$file[topological.sort(project.info$graph)]
  sorted.names <- sorted.names[sorted.names %in% tab.out$source.file]
  tab.out <- tab.out[match(sorted.names, tab.out$source.file), 
                     ]
  program.split <- split(project.info$tree, project.info$tree$source.file)
  summaries.out <- lapply(program.split, program.io.table)
  
  outputs <- list()
  for (source.iter in names(summaries.out)) {
    temp <- summaries.out[[source.iter]]
    temp$File <- make.relative.hyperlink(temp$Fullname, temp$File)
    outputs[[source.iter]] <- subset(temp, select = c("IO", 
                                                      "File", "Description"))
  }

  
myReport$title<-paste(source_info$project.id,'Project Summary')
myReport$author<-paste("IT2",author)
#  R2HTML::HTML(subset(tab.out, select = c("source.link", "source.file.description", 
#                                          "last.run.time.sec")), caption = paste(source_info$project.id, 
#                                                                                 "Project Summary"), captionalign = "top")
tab.out0 <- subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec"))
#rownames(tab.out0) <- 1:nrow(tab.out0)


tabtopander <<- tab.out0
rownames(tabtopander) <<- 1:nrow(tabtopander)

myReport$add(tabtopander)

#print(subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec")))
#  R2HTML::HTML(data.frame(Big_Graph = make.hyperlink(project.graph.file, 
#                                                     "Project Graph")), align = "center")

tabtopander <<- data.frame(Big_Graph = make.relative.hyperlink(project.graph.file, "Project Graph"))
rownames(tabtopander) <<- 1:nrow(tabtopander)
myReport$add(tabtopander)

#  R2HTML::HTML(data.frame(No_support_file_graph = make.hyperlink(reduced.project.graph.file, 
#                                                                 "Project Graph")), align = "center")
tabtopander <<- data.frame(No_support_file_graph = make.relative.hyperlink(reduced.project.graph.file,"Project Graph"))
rownames(tabtopander) <<- 1:nrow(tabtopander)
myReport$add(tabtopander)




for (namer in names(outputs)){
  myReport$add.paragraph(namer)
  
  tabtopander <<- subset(outputs[[namer]], Description !="Support file")
  rownames(tabtopander) <<- 1:nrow(tabtopander)
  
  myReport$add( tabtopander)
}

myReport$add.paragraph("Support files")
rownames(tabtopander) <<- 1:nrow(tabtopander)

tabtopander <<-subset(rbind.fill(outputs), (!duplicated(File)) &(Description == "Support file"))

myReport$add( tabtopander)

myReport$format<-'html'
myReport$export("project_summary",open=FALSE)

file.copy(file.path(source_info$results.dir,"project_summary.html"),file.path(source_info$project.path,"project_summary.html"))

all.files <- list.files(source_info$project.path,recursive=TRUE)

if(!send.data){
  
  all.files <- all.files[!grepl("^Data",all.files)]
  
}
  
tempwd <- getwd()

setwd(source_info$project.path)

#target.directory <- "/Volumes/WORKING/Gelfond_Swap"

if(target.directory!=source_info$project.path){
  
  targetdir <- file.path(target.directory,paste0("Results_",source_info$project.id))
  
  dir.create(targetdir)
  
  udirs <- setdiff(unique(dirname(all.files)),".")

  sapply(file.path(targetdir,udirs ),function(x){dir.create(x,recursive=TRUE,showWarnings=FALSE)})
  
  nfiles <- length(all.files)
  
  for(file.iter in 1:nfiles){

    fn <- all.files[file.iter]
    
    dir <- dirname(all.files[file.iter])
    
    file.copy(fn,file.path(targetdir,dir))
    
    
  }
  
  
}


setwd(tempwd)


#  R2HTML::HTMLStop()
  }
}