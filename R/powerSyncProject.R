#' #' Checks the synchronization project and runs scripts needed for synchronization.
#' #' If one script fails, the nondependent scripts are executed.
#' #' @param project.id is project to synchronize
#' #' @param ask logical whether to report estimated run time prior to execution
#' #' @return Character string with message about success for synchronization
#' #' @export
#' #' @examples 
#' #'\dontrun{
#' #' failoverSyncProject("adaprHome")
#' #'} 
#' #' 
#' #' 
#' #' 
#' failoverSyncProject <- function (project.id = getProject(), ask = FALSE) 
#' {
#'   source_info <- pullSourceInfo(project.id)
#'   test.sync0 <- syncTestSI(source_info)
#'   if (test.sync0$synchronize) {
#'     text <- paste(project.id, "Already synchonized")
#'     last.prog <- " "
#'   }  else {
#'     syncer <- sourceSyncSILoad(source_info)
#'     run.times <- syncer$run.times
#'     idSync.out <- syncer$idSync.out
#'     sync.out <- syncer$sync.out
#'     wait0 <- ceiling(as.numeric(sum(run.times$last.run.time.sec)) * 
#'                        1.5)
#'     if (ask) {
#'       print(idSync.out)
#'       execute <- readline(paste("Synchronizing project", 
#'                                 project.id, "will take about", ceiling(wait0/60), 
#'                                 "minutes. Proceed? y/n"))
#'       if (substring(tolower(execute), 1, 1) != "y") {
#'         print(paste("Not executed:", project.id, "will take about", 
#'                     ceiling(wait0/60), "minutes. Proceed? y/n"))
#'         out <- merge(subset(idSync.out, select = c("file", 
#'                                                    "run.order")), run.times, by.x = "file", by.y = "source.file")
#'         out <- out[order(out$run.order), ]
#'         return(out)
#'       }
#'     }
#'     
#'     
#'     ggproject <- createProgramGraph(getProject(),testSync=FALSE)
#'     ggproject$vertex$synccolor <- factor(ggproject$vertex$synccolor,levels=c("Synchronized","Not Synchronized","Running"))
#'     
#'     
#'     ggproject$vertex$synccolor[basename(ggproject$vertex$fullname) %in% idSync.out$file] <- "Not Synchronized"
#'     
#'     n.scripts.to.sync <- nrow(idSync.out)
#'     
#'     startmessage <- paste("Start sync approximate Time:", 
#'                           wait0, "seconds", n.scripts.to.sync, "scripts")
#'     full.time <- wait0
#'     last.prog <- "Go"
#'     source.iter <- 0
#'     
#'     print(rapidPlot(ggproject,startmessage))   
#'     
#'     
#'     while ((last.prog != "") & source.iter < nrow(idSync.out)) {
#'       
#'       source.iter <- source.iter + 1
#'       
#'       namer <- basename(ggproject$vertex$fullname)
#'       
#'       ggproject$vertex$synccolor[namer==idSync.out$file[source.iter]] <- "Running"
#'       
#'       
#'       messageOut <- paste(idSync.out$file[source.iter], paste0(source.iter, 
#'                                                                "/", n.scripts.to.sync), wait0, "seconds remaining")
#'       
#'       
#'       print(rapidPlot(ggproject,message=messageOut))    
#'       
#'       last.prog <- ""
#'       try({
#'         devtools::clean_source(file.path(idSync.out$path[source.iter], 
#'                                          idSync.out$file[source.iter]))
#'         last.prog <- idSync.out$file[source.iter]
#'         
#'         ggproject$vertex$synccolor[namer==idSync.out$file[source.iter]] <- "Synchronized"
#'         
#'       })
#'       
#'       if(last.prog==""){notRun <- c("notRun")}
#'       #print(run.times$last.run.time.sec[source.iter])
#'       
#'       
#'       
#'       wait0 <- wait0 - run.times$last.run.time.sec[source.iter]
#'     }
#'     
#'     
#'     print(rapidPlot(ggproject,message="Stopped"))    
#'     
#'     
#'     failure.script <- ifelse(source.iter <= nrow(idSync.out), 
#'                              as.character(idSync.out$file[source.iter]), "")
#'   }
#'   text <- paste("Sync successful for", project.id)
#'   if (last.prog == "") {
#'     text <- paste(last.prog, "failed sync for", project.id, 
#'                   "Script that failed:", failure.script)
#'   }
#'   else {
#'     if (getAdaprOptions()$git == "TRUE") {
#'       try({
#'         commitProject(paste("adapr Sync at", Sys.time()), 
#'                       project.id)
#'       })
#'     }
#'   }
#'   return(text)
#' }