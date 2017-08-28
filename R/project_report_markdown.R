#' Make plot of network within html documents.
#' Summarize all programs.
#' @param source_info Source information list
#' @param graph.width Sankey Plot dimensions
#' @param graph.height Sankey Plot dimensions
#' @return output file
#' @details Dose not assume source_info in workspace
#' @export
#' @examples 
#'\dontrun{
#'  source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#'  projectReportMarkdown(source_info)
#'} 
#'
projectReportMarkdown<-
function (source_info, graph.width = 960, graph.height = 500) 
{
si <- source_info
targetfile <- paste0("project_summary",".Rmd")
targetdirectory <- si$results.dir
createMarkdown(target.file=targetfile,target.dir=targetdirectory,style="html_document",description="\n",si,overwrite=TRUE)
project.info <- getProjectInfoSI(si)
project.graph <- project.info$graph
# START Make Sankey Plot
# E(project.graph)$weight = 0.1
# edgelist <- get.data.frame(project.graph)
# colnames(edgelist) <- c("source", "target", "value")
# edgelist$source <- as.character(edgelist$source)
# edgelist$target <- as.character(edgelist$target)
# sankeyPlot <- rCharts$new()
# sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey")
# sankeyPlot$set(data = edgelist, nodeWidth = 15, nodePadding = 10, 
#                layout = 32, width = graph.width, height = graph.height)
# project.graph.file <- file.path(source_info$results.dir, 
#                                 "full_networks.html")
# sankeyPlot$save(project.graph.file, cdn = TRUE)
# support.names <- subset(project.info$all.files, description == 
#                           "Support file")$fullname.abbr
# edgelist <- subset(edgelist, !(source %in% support.names) & 
#                      !grepl("Session_info", edgelist$source, fixed = TRUE) & 
#                      !grepl("Session_info", edgelist$target, fixed = TRUE))
# sankeyPlot <- rCharts$new()
# sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey")
# sankeyPlot$set(data = edgelist, nodeWidth = 15, nodePadding = 10, 
#                layout = 32, width = graph.width, height = graph.height)
# 
# END: Make Sankey Plot
reduced.project.graph.file <- file.path(source_info$results.dir, 
                                        "reduced_networks.png")
#sankeyPlot$save(reduced.project.graph.file, cdn = TRUE)
grDevices::png(reduced.project.graph.file,graph.width,graph.height)
programGraph <- createProgramGraph(source_info$project.id)
print(programGraph$ggplot)
grDevices::graphics.off()
programs <- subset(project.info$tree, !duplicated(project.info$tree$source.file), 
                   select = c("source.file", "source.file.path", "source.file.description"))
programs$source.file.fullname <- file.path(programs$source.file.path, 
                                           programs$source.file)
run.times <- plyr::ddply(project.info$tree, "source.file", function(x) {
  last.run.time <- max(difftime(as.POSIXct(x$target.mod.time) ,
                         as.POSIXct(x$source.run.time),units="secs"), na.rm = TRUE)
  return(data.frame(last.run.time.sec = last.run.time))
})
tab.out <- merge(programs, run.times, by = "source.file")
tab.out$source.link <- makeHyperlink(tab.out$source.file.fullname, 
                                      tab.out$source.file)
sorted.names <- igraph::V(project.info$graph)$file[igraph::topological.sort(project.info$graph)]
sorted.names <- sorted.names[sorted.names %in% tab.out$source.file]
tab.out <- tab.out[match(sorted.names, tab.out$source.file), ]
program.split <- split(project.info$tree, project.info$tree$source.file)
summaries.out <- lapply(program.split, programIOTable)
outputs <- list()
for (source.iter in names(summaries.out)) {
  temp <- summaries.out[[source.iter]]
  temp$File <- makeHyperlink(temp$Fullname, temp$File)
  outputs[[source.iter]] <- subset(temp, select = c("IO", 
                                                    "File", "Description"))
  rownames(outputs[[source.iter]])    <- NULL                                                 
}
tab.out0 <- subset(tab.out,select=c("source.link","source.file.description","last.run.time.sec"))
#rownames(tab.out0) <- 1:nrow(tab.out0)
tabtopander <- tab.out0
rownames(tabtopander) <- 1:nrow(tabtopander)
names(tabtopander) <- c("Source","Description","Last run time (sec)")
write("\n",file.path(targetdirectory,targetfile),append=TRUE)
write(knitr::kable(tabtopander),file.path(targetdirectory,targetfile),append=TRUE)
write("\n",file.path(targetdirectory,targetfile),append=TRUE)
tabtopander <- data.frame(`Dependency Graph` = makeHyperlink(reduced.project.graph.file,"Project Graph"))
rownames(tabtopander) <- 1:nrow(tabtopander)
write("\n\n",file.path(targetdirectory,targetfile),append=TRUE)
write(knitr::kable(tabtopander),file.path(targetdirectory,targetfile),append=TRUE)
write("\n\n",file.path(targetdirectory,targetfile),append=TRUE)
for (namer in names(outputs)){
	
write("\n",file.path(targetdirectory,targetfile),append=TRUE)
write(paste("#",namer,"\n"),file.path(targetdirectory,targetfile),append=TRUE)
out <- subset(outputs[[namer]],outputs[[namer]]$Description!="Support file")
rownames(out) <- NULL
write(knitr::kable(out),file.path(targetdirectory,targetfile),append=TRUE)
write("\n",file.path(targetdirectory,targetfile),append=TRUE)
}
fileout <- file.path(targetdirectory,targetfile)
rmarkdown::render(fileout)
paste("Made",si$project.id,"project summary.")
return(fileout)
}
#' Make plot of network within html documents.
#' Summarize all programs.
#' @param project.id Source information list
#' @param graph.width Sankey Plot dimensions
#' @param graph.height Sankey Plot dimensions
#' @details Dose not assume source_info in workspace
#' @return File path to report html file
#' @export
#'@examples 
#'\dontrun{
#' reportProject("adaprHome")
#'}  
reportProject <- function (project.id=getProject(), graph.width = 960, graph.height = 500){
  
  source_info <- pullSourceInfo(project.id)
  
  out <- projectReportMarkdown(source_info,graph.width, graph.height)
  
  out <- gsub("Rmd$","html",out)
  
  return(out)
  
}
