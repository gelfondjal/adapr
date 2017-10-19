#' Make plot of network within html documents.
#' Summarize all programs.
#' Make a readme file at top project directory
#' copy to target.directory
#' Uses pander and pandoc unlike project_report
#' @param target.directory Location to send project report
#' @param si Source information list
#' @param send.data Logical to send data directory or not
#' @param graph.width Sankey Plot dimensions
#' @param graph.height Sankey Plot dimensions
#' @details Not for direct use. Does not assume source_info in workspace
#' @export
#' @examples 
#'\dontrun{
#'  source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#' projectReportSend(si=source_info)
#'} 
#'
projectReportSend <- function (target.directory=get("source_info")$project.path,si,send.data=FALSE, graph.width = 960, graph.height = 500) 
{
  ##ibrary(pander)
  make.relative.hyperlink <- function(directory.to.clip,files,links){
    
    files <- gsub("^/","",gsub(directory.to.clip,"",files,fixed=TRUE))
    
    link.command <- rep("", length(files))
    
    if(length(files)==0){stop("projectReportSend: Error no files to link")}
    
    for (file.iter in 1:length(files)) {
      link.command[file.iter] <- paste0("<a href=\"file:", 
                                        files[file.iter], "\">", links[file.iter], "</a>")
    }
    return(link.command)
    
  }
  
target.directory2 <- file.path(target.directory,paste0("Results_",si$project.id))
 
dir.create(target.directory2,recursive=TRUE,showWarnings=FALSE)
targetfile <- paste0("project_summary",".Rmd")
#targetdirectory <- si$results.dir
createMarkdown(target.file=targetfile,target.dir=target.directory2,style="html_document",description="\n",si,overwrite=TRUE)
mdtoremove <- file.path(target.directory2,targetfile)
project.info <- getProjectInfoSI(si)
project.graph <- project.info$graph

reduced.project.graph.file <- file.path(target.directory2, 
                                       "reduced_networks.png")
#sankeyPlot$save(reduced.project.graph.file, cdn = TRUE)
grDevices::png(reduced.project.graph.file,graph.width,graph.height)
programGraph <- createProgramGraph(si$project.id)
print(programGraph$ggplot)
grDevices::graphics.off()
programs <- subset(project.info$tree, !duplicated(project.info$tree$source.file), 
                   select = c("source.file", "source.file.path", "source.file.description"))
programs$source.file.fullname <- file.path(programs$source.file.path, 
                                           programs$source.file)
run.times <- plyr::ddply(project.info$tree, "source.file", function(x) {
  last.run.time <- max(as.POSIXct(x$target.mod.time) - 
                         as.POSIXct(x$source.run.time), na.rm = TRUE)
  return(data.frame(last.run.time.sec = last.run.time))
})
tab.out <- merge(programs, run.times, by = "source.file")
tab.out$source.link <- make.relative.hyperlink(si$project.path,tab.out$source.file.fullname, 
                                      tab.out$source.file)
sorted.names <- igraph::V(project.info$graph)$file[igraph::topological.sort(project.info$graph)]
sorted.names <- sorted.names[sorted.names %in% tab.out$source.file]
tab.out <- tab.out[match(sorted.names, tab.out$source.file), ]
program.split <- split(project.info$tree, project.info$tree$source.file)
summaries.out <- lapply(program.split, programIOTable)
outputs <- list()
for (source.iter in names(summaries.out)) {
  temp <- summaries.out[[source.iter]]
  temp$File <- make.relative.hyperlink(si$project.path,temp$Fullname, temp$File)
  outputs[[source.iter]] <- subset(temp, select = c("IO", 
                                                    "File", "Description"))
  rownames(outputs[[source.iter]])    <- NULL                                                 
}
tab.out0 <- subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec"))
#rownames(tab.out0) <- 1:nrow(tab.out0)
tabtopander <- tab.out0
rownames(tabtopander) <- 1:nrow(tabtopander)
names(tabtopander) <- c("Source","Description","Last run time (sec)")
write("\n\n",file.path(target.directory2,targetfile),append=TRUE)
write(knitr::kable(tabtopander),file.path(target.directory2,targetfile),append=TRUE)
write("\n\n",file.path(target.directory2,targetfile),append=TRUE)
tabtopander <- data.frame(`Dependency Graph` = make.relative.hyperlink(target.directory2,reduced.project.graph.file,"Project Graph"))
rownames(tabtopander) <- 1:nrow(tabtopander)
write("\n\n",file.path(target.directory2,targetfile),append=TRUE)
write(knitr::kable(tabtopander),file.path(target.directory2,targetfile),append=TRUE)
write("\n\n",file.path(target.directory2,targetfile),append=TRUE)
for (namer in names(outputs)){
	
write("\n",file.path(target.directory2,targetfile),append=TRUE)
write(paste("#",namer,"\n"),file.path(target.directory2,targetfile),append=TRUE)
out <- subset(outputs[[namer]],outputs[[namer]]$Description!="Support file")
rownames(out) <- NULL
write(knitr::kable(out),file.path(target.directory2,targetfile),append=TRUE)
write("\n",file.path(target.directory2,targetfile),append=TRUE)
}
rmarkdown::render(file.path(target.directory2,targetfile))
   
  
  #See if Pander is installed
  #file.copy(file.path(temp.target,"project_summary.html"),file.path(target.directory,"project_summary.html"))
all.files <- list.files(si$project.path,recursive=TRUE)
if(!send.data){
  
  all.files <- all.files[!grepl("^Data",all.files)]
  
}
  
tempwd <- getwd()
setwd(si$project.path)

# Copy filesover
  
  targetdir <- file.path(target.directory,paste0("Results_",si$project.id))
  
  dir.create(targetdir)
  
  udirs <- setdiff(unique(dirname(all.files)),".")
  sapply(file.path(targetdir,udirs ),function(x){dir.create(x,recursive=TRUE,showWarnings=FALSE)})
  
  nfiles <- length(all.files)
  
  for(file.iter in seq_along(all.files)){
    fn <- all.files[file.iter]
    
    dir <- dirname(all.files[file.iter])
    
    file.copy(fn,file.path(targetdir,dir),overwrite=TRUE)
    
  }#loop over files
  
 
  
file.remove(mdtoremove)
setwd(tempwd)
}
