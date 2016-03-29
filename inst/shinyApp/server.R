## server.R
library(shiny)
require(rCharts)
library(adapr)
library(plyr) 
library(devtools)
library(shinyIncubator)
library(knitr)
require(igraph)
source("helpers.R")


shinyServer(function(input, output,session) {
    
  ##########################################################################################
  ### CREATE PROJECT TAB

  
  output$projectselected<-renderUI({
    helpText(paste(input$project.id,"project selected."))
  })
  
  output$createproject <- renderText({
    textout <- ifelse(nrow(get_orchard())==0,"Configure Project Directories.","Waiting to create project.")
    if(input$submitProject!=0){
      isolate({
        no.spaces<-make.names(input$project.id.make)
        no.spaces2<-gsub("\\.","_",no.spaces)
        textout <- ifelse( plant.tree(no.spaces2, input$project.directory, input$swap.directory),
                           paste("Made project",no.spaces2),"Failed")
      })
    }
    
    all.orchards <<- adapr::get_orchard() #read.csv(file.path(path.expand.2("~"), "ProjectPaths", "projectid_2_directory.csv"), as.is = TRUE)
    
    textout  
  })
  
  output$selectUI<-renderUI({
    input$submitProject
    selectInput(inputId = "project.id", label="", choices=get_orchard()$project.id, 
                selected = get_orchard()$project.id[1])
  })
  
  
  ##########################################################################################
  ### PROGRAMS AND LIBRARIES TAB

  
  output$projectselected2<-renderUI({
    helpText(paste(input$project.id,"project selected."))
  })
  
  output$myChart <- renderTable({
    temp <- subset(all.orchards,project.id==input$project.id,select="project.path")
    names(temp) <- "Project Filepath"
    rownames(temp) <- "Location:"
    temp
  })
  
  output$text1 <- renderText({
    if(input$submit!=0){
      isolate({
        no.spaces<-make.names(input$filename)
        no.spaces2<-gsub("\\.","_",no.spaces)
        program.name<-gsub("(.*)_(.*)","\\1.\\2",no.spaces2)
        if(sprout.program(input$project.id,source.file.name=program.name,input$description)){
          temp <- subset(all.orchards,project.id==input$project.id,select="project.path")
          clean_source(file.path(temp,project.directory.tree$analysis,program.name),quiet=TRUE) 
          #run program in a new r session after you make it
          paste(program.name,"Created")
        }
        else {"Failed"}
      })
    }
  })
  
  output$libraryTable <- renderTable({
    if(input$submitLibrary!=0){ 
      source_info <- pull_source_info(input$project.id)    
      library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
      library.table <- read.csv(library.file,as.is=FALSE)
      library.table <- library.table[order(library.table$Package),]
      if(is.null(library.table$specific)){library.table$specific <- FALSE}
      library.table$specific <- as.logical(library.table$specific )
      #    print(library.file)
      #   load.install.library.file(library.data.file)
      library.table }
  })
  
  
  output$addLibraryTable <- renderTable({
    source_info <- pull_source_info(input$project.id)    
    library.file <- file.path(source_info$project.path,project.directory.tree$support,"common_libs.csv")
    if(input$submitLibrary!=0){                                             
      isolate({  
        if(!(input$library.name %in% c("","mylibrary"))){
          subgroup <- data.frame(Package=input$library.name,repos=input$library.install,
                                 specific=as.logical(input$library.specific))  
          load.install.library.file(library.file,subgroup)
          print(read.csv(library.file))
          subgroup
        }# if real library 
      })
    }   
  })
  

  ##########################################################################################
  ### REPORT TAB

  
  output$projectselected3<-renderUI({
    helpText(paste(input$project.id,"project selected."))
  })
  
  output$selectAppUI<-renderUI({
    input$submitProject
    source_info <- pull_source_info(input$project.id)    
    app.list <- list.files(file.path(source_info$project.path,project.directory.tree$support,"Apps"))
    selectInput(inputId = "powerApp", label="Select App", choices=app.list, 
                selected = app.list[1])
  })
  
  output$projectus <- renderText({ 
    if(input$submitReport!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        project_report_markdown(source_info) 
        browseURL(paste0("file://",(file.path(source_info$results.dir,"project_summary.html"))))
        paste("Creating report",input$project.id)   
      })
    }
  })  
  
  output$runApp <- renderText({ 
    text.out <- "Waiting"
    if(input$submitRunApp!=0){                                             
      isolate({  
        source_info <- pull_source_info(input$project.id)
        
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

  
  ##########################################################################################
  ### SYNCHRONIZE TAB

  
  output$projectselected4<-renderUI({
    helpText(paste(input$project.id,"project selected."))
  })
  
#  output$syncPlot <- renderImage({
#    source_info <- pull_source_info(input$project.id)
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
        source_info <- pull_source_info(input$project.id)
        test.sync0 <- sync.test.si(source_info)
        print(paste(input$project.id,ifelse(identical(test.sync0$synchronize,TRUE),"Syncrhonized","Not Synchronized")))
      })
    } 
  }) 
  
#  output$syncer <- renderPlot({ 
#    if(input$submitSyncRun!=0){                                             
#      #isolate({  
#      source_info <- pull_source_info(input$project.id)
#      filename <- file.path(source_info$project.path,project.directory.tree$results,
#                            "tree_controller.R","sync_updater.png")
#      htmlfile <- file.path(source_info$project.path,project.directory.tree$results,
#                            "tree_controller.R","syncrhonize.html")
#      html.out <- paste("<img src=sync_updater.png>")
#      write(html.out,htmlfile)
#      browseURL(paste0("file://",htmlfile))
#      test.sync <- source.sync.si(source_info,run=TRUE,TRUE)
#      #  test.sync <- sync.test.si(source_info)
#      #   print(test.sync)
#      # print("Hello")
#      # })
#    }
#  })
  output$progressbar<-renderText({
    if(input$submitSyncRun!=0){
      isolate({
        text<-paste("Waiting to synchronize",input$project.id) 
        source_info <- pull_source_info(input$project.id)
        test.sync0 <- sync.test.si(source_info)
        
        if(test.sync0$synchronize){
          text <- paste(input$project.id,"Already synchonized")
        }else{
        
        syncer <- source_sync_si_load(source_info)
        
        run.times <- syncer$run.times
        
        ID.sync.out <- syncer$ID.sync.out
        
        sync.out <- syncer$sync.out
     
        wait0<-ceiling(as.numeric(sum( run.times$last.run.time.sec))*1.5)
        
        
        progress <- shiny::Progress$new()
        on.exit(progress$close())
      
        n.scripts.to.sync <- nrow(ID.sync.out)
        
        startmessage <- paste("Approximate Time:", wait0, "seconds",n.scripts.to.sync,"scripts")
        
        progress$set(message=paste("Start sync",startmessage),value=0)
        
        Sys.sleep(3)
        
        
        full.time <- wait0
        
        for (source.iter in 1:nrow(ID.sync.out)) {
        
          runmessage <- paste(ID.sync.out$file[source.iter],paste0(source.iter,"/",n.scripts.to.sync),wait0,"seconds remaining")
          
          progress$inc(1/2,detail=runmessage)
         # progress$set(message=paste("Start sync",startmessage),value=0)
        
          last.prog <- ""
         
      
          try({
          clean_source(file.path(ID.sync.out$path[source.iter],ID.sync.out$file[source.iter]))
            
          #Sys.sleep(3)
          
          last.prog <- ID.sync.out$file[source.iter]
          
          
          })
          
          print(wait0)
          
          print(run.times$last.run.time.sec[source.iter] )
          
          wait0 <- wait0 - run.times$last.run.time.sec[source.iter] 
          
          
          
        }
        
        #withProgress(session,min=1,max=3,expr={
      #    setProgress(message = 'Synchronizing',detail=paste("Approximate Time:", wait, "seconds"),value=2)
        #  test.sync <- source.sync.si(source_info,run=TRUE,TRUE)
      #    setProgress(value=3)
      
      text<-paste(startmessage,"Sync successful for",input$project.id)
      if(last.prog==""){    
        text<-paste(last.prog,"failed sync for",input$project.id)
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
        source_info <- pull_source_info(input$project.id)
        #test.sync <- source.sync.si(source_info,run=TRUE)
        
        test.sync0 <- sync.test.si(source_info)
        synccheck <- ifelse(test.sync0,"SYNCHRONIZED","NOT SYNCd")
        
        setwd(source_info$project.path)
        analysis.dir <- file.path(source_info$project.path,project.directory.tree$analysis)
        all.programs <- matrix(list.files(analysis.dir,recursive=TRUE,full.names=TRUE))
        add <-  apply(all.programs,1,function(x){git.add(source_info$project.path,filename=x)})
        commited <- git.commit(analysis.dir,paste(synccheck,input$commit.message))
        #  test.sync <- sync.test.si(source_info)
        #   print(test.sync)
        print(paste("Git","\n",add,"\n",commited))
      })
    }
  })


output$"ProgramDAG" <- renderPlot({
  if(input$makeGraph!=0){
  temp <- create_program_graph(input$project.id)
  
  temp$ggplot
  }
})
  

  ##########################################################################################
  ### SEND TAB


  output$projectselected5<-renderUI({
    helpText(paste(input$project.id,"project selected."))
  })
  
  output$Programs <- renderTable({
    # Retrieve publication table
    source_info <- pull_source_info(input$project.id)
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
      source_info <- pull_source_info(input$project.id)
      
      publication.table <- get_publication_table(input$project.id)  
      
      
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
        source_info <- pull_source_info(input$project.id)
        target.directory <- get.project.publish.path(input$project.id)
		    project_report_send_rmd(target.directory=target.directory,source_info,send.data=ifelse(input$send.data=="TRUE",TRUE,FALSE),
									 graph.width = 960, graph.height = 500) 
        paste("Project",input$project.id,"published on",Sys.time(),get.project.publish.path(input$project.id)) 
      })
    } 
  })   
  
  
    
  ##########################################################################################
  ###CONFIGURE TAB
  
  
  output$Git<-renderUI({ 
    textout <- "Waiting to check Git"
    if(input$submitGitCheck!=0){
      temp <- git.configure.test()
      if(temp==0){
        textout<-"Git is configured."
      }
      else{
        textout<-a("Git is not configured. Click here to get Git",href="http://git-scm.com/downloads")
      }
    }
    textout  
  })

  output$Gitlogin<-renderText({
    textout<-"Waiting to log into Git"
    if(input$submitGitLogin!=0){
      login<-git.configure(input$git.username,input$git.email)
      if(login[[1]]==0&login[[2]]==0){
        textout<-"Login successful"
      }
      else{textout<-"Login failed."} 
    }
    textout
  })
  
  
  output$First.project<-renderText({
    textout<-ifelse(nrow(get_orchard())==0,"Configure Project Directories.","Project Directory Configured.")
    if((input$submitFirst.project%%2)!=0){
      login<-first.project(input$project1.directory,input$publish.directory)
      if(login){
        textout<-"Project Directories Identified"
      }
      else{
        textout<-paste("Project Not Started!",input$submitFirst.project)
      } 
    }
    textout
  })
  
  
  
  output$IT2<-renderText({
    textout<-"Waiting to install latest adapr"
    if (input$installit2!=0){
      install_github("adapr", "gelfondjal")
      library(adapr)
      textout<-"adapr has been installed."
    }
    textout
  })
  
  
  ##########################################################################################
 
})
