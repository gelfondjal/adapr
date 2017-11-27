## server.R
library(shiny)
#require(rCharts)
library(adapr)
library(plyr) 
library(devtools)
#library(shinyIncubator)
library(knitr)
require(igraph)
source("helpers.R")
library(plotly)
library(shinydashboard)


shinyServer(function(input, output,session) {
    
  ##########################################################################################
  ### CREATE PROJECT TAB

  
  output$projectselected<-renderUI({
    helpText(paste("*",input$project.id,"*"))
  })
  
  
  output$selectUI<-renderUI({
    input$submitProject
    selectInput(inputId = "project.id", label="", choices=get_orchard()$project.id, 
                selected = get_orchard()$project.id[1])
  })
  
  
  output$redirectproject <- renderText({
    print("Redirecting Project")
  
    textoutRD <- ifelse(nrow(get_orchard())==0,"Configure Project Directories.","Waiting to create project.")
    if(input$IDredirectProject!=0){
      print("Redirecting Project")
      isolate({
        no.spaces<-make.names(input$project.id.redirect)
        no.spaces2<-gsub("\\.","_",no.spaces)        
        textoutRD <- ifelse( redirect.tree(no.spaces2, fixWindowsDashes(input$new.project.directory), fixWindowsDashes(input$new.swap.directory)),
                         paste("Redirected Tree",no.spaces2),"Failed")
      })
    }
    
    all.orchards <<- adapr::get_orchard() 
    
     textoutRD 
  })
  
  
  
  ### REPORT TAB

  output$projectselected3<-renderUI({
    helpText(paste("*",input$project.id,"*"))
  })
  
  output$selectAppUI<-renderUI({
    input$submitProject
    source_info <- pullSourceInfo(input$project.id)    
    app.list <- list.files(file.path(source_info$project.path,project.directory.tree$support,"Apps"))
    selectInput(inputId = "powerApp", label="Select App", choices=app.list, 
                selected = app.list[1])
  })
  
  output$projectus <- renderText({ 
    if(input$submitReport!=0){                                             
      isolate({  
        source_info <- pullSourceInfo(input$project.id)
        projectReportMarkdown(source_info) 
        browseURL(paste0("file://",(file.path(source_info$results.dir,"project_summary.html"))))
        paste("Creating report",input$project.id)   
      })
    }
  })  
  
  
  ##########################################################################################
  ### SYNCHRONIZE TAB 
  
  
  output$ProgramDAG_report <- renderPlot({ 
    if(input$make_report_graph!=0){
      
      report_graph <<- createProgramGraph(input$project.id)
     
      report_tree <<- readDependency(pullSourceInfo(input$project.id)$dependency.dir)
      report_graph$ggplot
     
      #p <- ggplotly(temp$ggplot)
      #print(p)
    }          
  
  })
  
  output$ProgramDAG_report_info <- renderPrint({
    
    if(input$make_report_graph==0){
      
      cat("Click to run, select to get details")
    }else{
    
    nout. <- nearPoints(report_graph$vertex, input$ProgramDAG_click_report, xvar = "x", yvar = "y", threshold = 20)
    #cat("Running...")
    if(nrow(nout.) >= 1){
      clean_source(nout.$fullname)
      cat("Completed")
    }else{
      cat("Click to run, select to get details")
    }
    }
    
  })
  
  #output$ProgramDAG_report_info_1 <- renderPrint({
  #  nout. <- nearPoints(report_graph$vertex, input$ProgramDAG_click_report, xvar = "x", yvar = "y", threshold = 20)
  #  if(nrow(nout.) >= 1){
  #    cat("Running...")  
  #  }
    #cat("Click on Examine Program Graph")
  #})  
  
  output$ProgramDAG_report_select_graph <- renderPlot( {
        
    if(input$make_report_graph==0){
      return(NULL)
      #proggraphout <- ggplot(data.frame(x=1,y=1,label="waiting"),aes(x=x,y=x,label=label))+geom_text()+
      #theme(legend.position="bottom",
      #                      axis.line=element_blank(),axis.text.x=element_blank(),
      #                      axis.text.y=element_blank(),axis.ticks=element_blank(),
      #                      axis.title.x=element_blank(),
      #                      axis.title.y=element_blank(),
      #                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
      #                      panel.grid.minor=element_blank(),plot.background=element_blank())
      #proggraphout   
        
    }else{
      #nearPoints(temp$vertex, input$ProgramDAG_brush, threshold = 100, maxpoints = 1)     
      #brushedPoints(report_graph$vertex, input$ProgramDAG_brush_report)
            
      out0. <- brushedPoints(report_graph$vertex, input$ProgramDAG_brush_report)
                                
      iotable0 <- subset(report_tree,source.file==basename(out0.$fullname))
      
      ioprogram <- data.frame(File=iotable0$source.file[1],Description=iotable0$source.file.description[1],Dependency="R Script",Path=iotable0$source.file.path[1])
      
      iotable0 <- subset(iotable0,select=c("target.file","target.description","dependency","target.path"))
      
      names(iotable0) <- c("File","Description","Dependency","Path")
      
      iotable0 <- subset(iotable0,Description!="Support file")
      
      iotable0 <- iotable0[order(iotable0$Dependency),]
      
      iotable0 <- rbind(ioprogram,iotable0)
      
  #    iotable0$y <- nrow(iotable0) - (cumsum(iotable0$Dependency=="in")*(iotable0$Dependency=="in") + cumsum(iotable0$Dependency=="out")*(iotable0$Dependency=="out")) + 0.5*(iotable0$Dependency=="out")
  #    iotable0$x <- ifelse(iotable0$Dependency=="R Script",0,ifelse(iotable0$Dependency=="in",-1,1))
  
  
      iotable0 <- iotable0[order(iotable0$Dependency),]
  
      iotable0$y <- nrow(iotable0):1
      iotable0$x <- 0

      proggraphout <- ggplot(iotable0,aes(x=x,y=y,label=File,color="white",fill=Dependency))+
                      geom_label(size=6.5,nudge_y=0,color="white")+
                      geom_label(aes(x=x,y=y,label=Description,fill=NULL),nudge_y=-0.25)+
                      scale_x_continuous(limits=c(-3,3)) 
                      
      proggraphout <- proggraphout+theme_bw()+
                      theme(legend.position="bottom",
                            axis.line=element_blank(),axis.text.x=element_blank(),
                            axis.text.y=element_blank(),axis.ticks=element_blank(),
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank(),
                            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),plot.background=element_blank())
      
      iotable0$fullname <- file.path(iotable0$Path,iotable0$File)
      
      iotable0 <<- iotable0
      
      proggraphout
      
      }
    
  })
  
  output$brush_info_select <- renderPrint({
    if(input$make_report_graph==0){
      #nearPoints(report_graph$vertex, input$ProgramDAG_brush_report, xvar="x", yvar="y")
      #rownames(report_graph$vertex)
      #names(report_graph$vertex)
      cat("Click on Examine Project and select program")
    }else{
      #nearPoints(temp$vertex, input$ProgramDAG_brush, threshold = 100, maxpoints = 1)     
      out. <- brushedPoints(iotable0, input$ProgramDAG_brush_report_browse)#
      
      clicked <- nearPoints(iotable0, input$ProgramDAG_click_report_browse, threshold = 10, maxpoints = 1)
      
    if(nrow(clicked)){
      browseURL(clicked$fullname[1])
    }
      #out.
      #print(out.)
      if(nrow(out.)>0){
        #print(paste0("Running ",out.$fullname))
      #  clean_source(out.$fullname)
        
        print(basename(out.$fullname[1]))
        
        browseURL(dirname(out.$fullname[1]))
      }else{
        cat("nothing selected")
      }
    }
  })
  
  output$ProgramDAG_report_select <- renderTable( {
    if(input$make_report_graph==0){
      if(!exists("iotable")){iotable <<- data.frame(Input="Input files",Output="Output files")}
    }else{
    #nearPoints(temp$vertex, input$ProgramDAG_brush, threshold = 100, maxpoints = 1)     
    brushedPoints(report_graph$vertex, input$ProgramDAG_brush_report)
    out. <- brushedPoints(report_graph$vertex, input$ProgramDAG_brush_report)

    }

      iotable <- subset(report_tree,source.file==basename(out.$fullname))
        
      print(iotable)
    
      ioprogram <- data.frame(File=iotable$source.file[1],Description=iotable$source.file.description[1],Dependency="R Script")
    
      ioprogram$Hyperlink <- make.hyperlink(file.path(iotable$source.file.path[1],iotable$source.file[1]),rep("Link",nrow(ioprogram)))
    
      iotable$Hyperlink <- make.hyperlink(file.path(iotable$target.path,iotable$target.file),rep("Link",nrow(iotable)))
    
      iotable <- subset(iotable,select=c("target.file","target.description","dependency","Hyperlink"))
    
      names(iotable) <- c("File","Description","Dependency","Hyperlink")
    
      iotable <- subset(iotable,Description!="Support file")
    
      iotable <- iotable[order(iotable$Dependency),]
    
      iotable <- rbind(ioprogram,iotable)
    
      rownames(iotable) <- NULL
    
      iotable <<- iotable
      
      return(iotable)
      
 
  }, sanitize.text.function = function(x) x)
  
  
  output$runApp <- renderText({ 
    text.out <- "Waiting"
    if(input$submitRunApp!=0){                                             
      isolate({  
        source_info <- pullSourceInfo(input$project.id)
        
        app.dir <- file.path(source_info$project.path,project.directory.tree$support,"Apps",input$powerApp)
      
        portL <- floor(runif(1)*9998+1)
        
        app.command <- paste0("shiny::runApp('",app.dir,"',port=",portL,",host='127.0.0.1')")
        
        run.command <- paste("library(adapr)",app.command,sep="\n")
        
        tf <- tempfile()
        
        write(run.command,tf)
        browseURL(paste0("http://127.0.0.1:",portL))
        clean_source(tf)
    
        text.out <- paste("Run",input$powerApp)
      })
      
    } 
    text.out 
  }) 

  
  output$selectIDfile <- renderPrint({ 
   # print("Hello")
    if(input$IDfile!=0){                                             
      isolate({  
        provenanceOutput <-  gitProvenance(input$project.id)
        print(((provenanceOutput)))
      })
    } 
  }) 
  
  
  
  
  ##########################################################################################
  ### SYNCHRONIZE TAB

  
  output$projectselected4<-renderUI({
    helpText(paste("*",input$project.id,"*"))
  })
  
#  output$syncPlot <- renderImage({
#    source_info <- pullSourceInfo(input$project.id)
#    filename <- file.path(source_info$project.path,project.directory.tree$results,
#                          "tree_controller.R","sync_updater.png")
#    filename <- gsub("\\\\","/",filename)
#    # Return a list containing the filename and alt text
#    list(src = filename,
#         alt = paste("Waiting to sync"))  
#  }, deleteFile = FALSE)
  
  output$syncTest <- renderText({ 
    if(input$submitSyncTest!=0){                                             
      isolate({  
        source_info <- pullSourceInfo(input$project.id)
        test.sync0 <- syncTestSI(source_info)
        print(paste(input$project.id,ifelse(identical(test.sync0$synchronize,TRUE),"Syncrhonized","Not Synchronized")))
      })
    } 
  }) 
  
  output$progressbar<-renderText({
    if(input$submitSyncRun!=0){
      isolate({
        text<-paste("Waiting to synchronize",input$project.id) 
        source_info <- pullSourceInfo(input$project.id)
        test.sync0 <- syncTestSI(source_info)
        
        if(test.sync0$synchronize){
          text <- paste(input$project.id,"Already synchonized")
        }else{
        
        syncer <- sourceSyncSILoad(source_info)
        
        run.times <- syncer$run.times
        
        ID.sync.out <- syncer$idSync.out
        
        sync.out <- syncer$sync.out
     
        wait0<-ceiling(as.numeric(sum( run.times$last.run.time.sec))*1.5)
        
        
        #progress <- shiny::Progress$new()
        #on.exit(progress$close())
      
        n.scripts.to.sync <- nrow(ID.sync.out)
        
        startmessage <- paste("Start sync approximate Time:", wait0, "seconds",n.scripts.to.sync,"scripts")
        
        withProgress(message=startmessage, expr={
        
#        progress$set(message=paste("Start sync",startmessage),value=0)
        
        Sys.sleep(3)
        
        
        full.time <- wait0
        last.prog <- "Go"
        source.iter <- 0
        while((last.prog != "") & source.iter < nrow(ID.sync.out)){
        source.iter <- source.iter + 1
        #for (source.iter in 1:nrow(ID.sync.out)) {
        
          runmessage <- paste(ID.sync.out$file[source.iter],paste0(source.iter,"/",n.scripts.to.sync),wait0,"seconds remaining")
          
          incProgress(source.iter/nrow(ID.sync.out),message=runmessage)
          
         # progress$set(message=paste("Start sync",startmessage),value=0)
        
          last.prog <- ""
         
      
          try({
            
          clean_source(file.path(ID.sync.out$path[source.iter],ID.sync.out$file[source.iter]))
            
          #run.program(input$project.id,ID.sync.out$file[source.iter],TRUE)  
            
          #Sys.sleep(3)
          
          last.prog <- ID.sync.out$file[source.iter]
          
          
          })
          
          print(wait0)
          
          print(run.times$last.run.time.sec[source.iter] )
          
          wait0 <- wait0 - run.times$last.run.time.sec[source.iter] 
          
          
          
        } #loop over running scripts
        
        })#END progress bar
        
        failure.script <- ifelse(source.iter <= nrow(ID.sync.out),as.character(ID.sync.out$file[source.iter]),"")
        
        #withProgress(session,min=1,max=3,expr={
      #    setProgress(message = 'Synchronizing',detail=paste("Approximate Time:", wait, "seconds"),value=2)
        #  test.sync <- source.sync.si(source_info,run=TRUE,TRUE)
      #    setProgress(value=3)
      
      text<-paste(startmessage,"Sync successful for",input$project.id,ifelse(adapr_options$git=="TRUE","& git committed",""))
      if(last.prog==""){    
        text<-paste(last.prog,"failed sync for",input$project.id,"Check",failure.script)
          }
        }#IF NOT SYNCHONIZED
      text
        #})
      })
    }
  })
  
  output$CommitOut <- renderText({ 
    if(input$submitCommitRun!=0){                                             
      
      isolate({  
      	
      	if(adapr_options$git=="TRUE"){
      	
        source_info <- pullSourceInfo(input$project.id)
        #test.sync <- source.sync.si(source_info,run=TRUE)
        
        test.sync0 <- syncTestSI(source_info)$synchronized
        synccheck <- ifelse(test.sync0,"SYNCHRONIZED","NOT SYNCd")
        
        commited <- commitProject(paste(synccheck,input$commit.message),input$project.id)
        #  test.sync <- syncTestSI(source_info)

        
        }else{
        	
        	print("ADAPR OPTION git FALSE \n git not active")
        	
        	}
        
      })
      
      
    }
  })    

  output$ProgramDAG <- renderPlot({
    if(input$makeGraph!=0){
    temp <<- createProgramGraph(input$project.id)
    temp$ggplot
    #p <- ggplotly(temp$ggplot)
    #print(p)
    }
  })

  

  output$click_info <- renderPrint({
    #temp <- createProgramGraph(input$project.id)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(temp$vertex, input$ProgramDAG_click, addDist = TRUE, threshold = 100, maxpoints = 1)
  })

  output$brush_info <- renderPrint({
    if(input$makeGraph==0){
      cat("Click on Update Project Plot")
    }else{
      #nearPoints(temp$vertex, input$ProgramDAG_brush, threshold = 100, maxpoints = 1)     
      brushedPoints(temp$vertex, input$ProgramDAG_brush)
      out. <- brushedPoints(temp$vertex, input$ProgramDAG_brush)#nearPoints(temp$vertex, input$ProgramDAG_brush, threshold = 100, maxpoints = 1)
      #out.
      #print("hello")
      if(nrow(out.)>0){
        #print(paste0("Running ",out.$fullname))
        clean_source(out.$fullname)
        cat("file executed")
      }else{
        cat("nothing selected")
      }
    }
  })
  

  ##########################################################################################
  ### SEND TAB


  output$projectselected5<-renderUI({
    helpText(paste("*",input$project.id,"*"))
  })
  
  output$Programs <- renderTable({
    # Retrieve publication table
    source_info <- pullSourceInfo(input$project.id)
    publication.file <- file.path(source_info$project.path,project.directory.tree$support,"files_to_publish.csv")
    publication.table <- get_publication_table(input$project.id)    
    outtable <- publication.table
    print("Publication table retrieved")
    if(nrow(publication.table)==0){
      outtable <- data.frame(Path="No files to publish",Description="Choose a file")
    }
    
    if(input$submitPublish!=0){ 
      isolate({
            
      if(input$filename.send!=0){
      
      print("Checking file to send")
        
      filetosend <- input$filename.send# filetosend <- "Results/read_data.R/read_data.html"
      
      possible.paths <- get_filelist(input$project.id)
      file.not.exist <- 0
      if(!(filetosend %in% publication.table$Path)){
      print("File not in publication path, add to list")  
      
      if(file.exists(file.path(source_info$project.path,filetosend))){ 
        print(paste("Sending file",filetosend))
        print(filetosend)        
        publication.table <- rbind(publication.table,subset(possible.paths,Path==filetosend))        
        print(publication.table)
        write.csv(publication.table,publication.file,row.names=FALSE)
      }else{
        file.not.exist <- 1
        }
      
      }#if new file
        
      }#if file selected
      
      if(nrow(publication.table)>0){
        publication.table <- publication.table[order(basename(publication.table$Path)),]
      }else{publication.table <- data.frame(Path="No files to publish",Description="Choose a file")}
      
      if(file.not.exist==1){publication.table <- data.frame(Path="Cannot find file to publish",Description="Choose a file")}
      
      #file.copy(file.path(source_info$project.path,publication.table$Path),get.project.publish.path(input$project.id),overwrite=TRUE)
      
      outtable <- publication.table
                
      }) #isolate
    }# if action button
    
    outtable
    
  })
  

  output$PublishedText <- renderText({
    
    pubout <- "Waiting to publish."
    
    if(input$publish.button!=0){
    isolate({
      source_info <- pullSourceInfo(input$project.id)
      
      publication.table <- getPubResults(input$project.id)  
      
      
      if(nrow(publication.table)>0){
        
      publication.table <- publication.table[order(basename(publication.table$Path)),]  
      file.copy(file.path(source_info$project.path,publication.table$Path),get.project.publish.path(input$project.id),overwrite=TRUE)
      pubout <- paste("Published",input$project.id, "files",Sys.time(),"to",get.project.publish.path(input$project.id))
      
      }else{
        pubout <- "Nothing to publish at this time."
      }
    
    })
      
    }#
    
    pubout
    
  })

  
  output$Sent <- renderText({ 
    if(input$submitSend!=0){                                             
      isolate({  
        source_info <- pullSourceInfo(input$project.id)
        target.directory <- get.project.publish.path(input$project.id)
		    project_report_send_rmd(target.directory=target.directory,source_info,send.data=ifelse(input$send.data=="TRUE",TRUE,FALSE),
									 graph.width = 960, graph.height = 500) 
        paste("Project",input$project.id,"published on",Sys.time(),get.project.publish.path(input$project.id)) 
      })
    } 
  })   
  
  
    
  ##########################################################################################
  ###CONFIGURE TAB
  

  ##########################################################################################
 
})
