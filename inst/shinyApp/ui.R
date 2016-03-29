## ui.R

library(adapr)
library(shinyIncubator)

shinyUI(fluidPage(theme="style.css",pageWithSidebar(

  headerPanel('Stat Navigator'),
                      
  sidebarPanel(
    
    tags$head(
      tags$style(type="text/css", "input.shiny-bound-input {width:300px;}")
    ),
    
         
    conditionalPanel(condition="input.conditionedPanels == 'Select Project'",
                     htmlOutput("projectselected"),
                     helpText(h3("Choose an Existing Project")),
                     htmlOutput("selectUI"),
                     br(),
                     helpText(h3("Create a New Project")),
                     textInput('project.id.make', "Project ID:", value = "Example project"),
                     textInput('project.directory', "Project directory:", 
                               value= gsub("(^.*)(\\/.*$)","\\1",get_orchard()$project.path[1])),
                     br(),
                     textInput("swap.directory", label = "Publish directory:",
                               value=gsub("(^.*)(\\/.*$)","\\1",get_orchard()$swap.directory[1])),
                     br(),br(),
                     actionButton("submitProject","Create project"),  
                     br(),br(),
                     img(src="treePic.jpg",height=360,width=280)
                     ),
                                                                                                                 
    conditionalPanel(condition="input.conditionedPanels == 'Programs & Libraries'", 
                     htmlOutput("projectselected2"),
                     helpText(h3("Make Program")),
                     textInput('filename', "Filename:", value = "MyProgram.R"),
                     br(),
                     textInput("description", label = "Program description:", value = "Description of your program..."),
                     br(),br(),
                     actionButton("submit","Create program"),
                     br(),br(),
                     img(src="makeProgramPic.jpg",height=240,width=300),
                     br(),
                     helpText(h3("Manage Libraries")),
                     br(),
                     textInput("library.name", label = "Library Name", value = "mylibrary"),
                     textInput("library.install", label = "Install Command (bioC or '' for CRAN)", value = ""),
                     selectInput('library.specific', label="Library for specific program?", c("FALSE","TRUE"),"FALSE"),                                                                            
                     br(),br(),
                     actionButton("submitLibrary","Add Library"),
                     br(),br()
                     ),
                                                                                                                                                                                                                                                                                  
    conditionalPanel(condition="input.conditionedPanels == 'Report'", 
                     htmlOutput("projectselected3"),
                     actionButton("submitReport","Create report"),
                     br(),br(),
                     actionButton("submitRunApp","Run App"),br(),br(),
                     htmlOutput("selectAppUI"),
                  
                     img(src="projectReport.jpg",height=340,width=240)
                     ),
    
    conditionalPanel(condition="input.conditionedPanels == 'Synchronize'",
                     htmlOutput("projectselected4"),
                     actionButton("submitSyncTest","Check Sync"),
                     br(),br(),
                     actionButton("submitSyncRun","Synchronize Now"),
                     br(),br(),
                     textInput('commit.message', "Commit Message", value = "Message"),
                     br(),br(),
                     actionButton("submitCommitRun","Commit Project"),
                     br(),br(),
                     actionButton("makeGraph","Plot Project"),
                     br(),br(),
                     img(src="syncTree.jpg",height=320,width=260)
                     ),
 
    conditionalPanel(condition="input.conditionedPanels == 'Send'",
                     htmlOutput("projectselected5"),
                     textInput('filename.send', "Project File:", value="Results/read_data.R/read_data.html"),
                     br(),br(),
                     actionButton("submitPublish","Add File to Publish"),
                     br(),br(),
                     actionButton("publish.button","Publish files"),
                     br(),br(),
                     selectInput('send.data',"Send Data?",c("FALSE","TRUE"),"FALSE"),
                     actionButton("submitSend","Publish Project"),
                     br(),br(),
                     img(src="sendBranch.jpg",height=275,width=450)
                     ),
  
  
      conditionalPanel(condition="input.conditionedPanels == 'Configure'", 
                       
                    helpText(h3("Configure First Project")),
                    textInput("project1.directory","Project Directory",value=project.path.start),
                    textInput("publish.directory","Publish Directory:",value=publish.path.start),
                    actionButton("submitFirst.project","Setup First Project"),
                    br(),br(),
                       
                     helpText(h3("See if Git is working")),
                     actionButton("submitGitCheck","Check Git"),
                     br(),br(),
                     helpText(h3("Log into Git")),
                     textInput("git.username","Username:",value=gitAuthor),
                     textInput("git.email","Email:",value=gitEmail),
                     br(),
                     actionButton("submitGitLogin","Login"),
                     br(),br(),
                     helpText(h3("Install the latest version of adapr")),
                     actionButton("installit2","Install"),
              
                    
                     img(src="configure.jpg",height=330, width=300)
                     )
    ),
  
  
  mainPanel(
        
    #shinyIncubator::progressInit(),
    tabsetPanel(type = "tabs",
      tabPanel("Select Project",br(),br(),tableOutput("createproject")),
      tabPanel("Programs & Libraries",br(),br(),tableOutput("myChart"), tableOutput('text1'),
               tableOutput('libraryTable'),tableOutput('addLibraryTable')),
      tabPanel("Report",br(),br(), tableOutput("projectus"),tableOutput("runApp")),
      tabPanel("Synchronize",br(),br(),tableOutput("syncTest"),br(),br(), textOutput("syncText"),
               textOutput("progressbar"),br(),br(),textOutput("CommitOut"),plotOutput("ProgramDAG")),
      tabPanel("Send",br(),br(), tableOutput("Programs"),br(),br(),textOutput("PublishedText"), textOutput("Sent")),
      tabPanel("Configure",br(),br(),uiOutput("First.project"),br(),uiOutput("Git"),br(),textOutput("Gitlogin"),br(),br(),textOutput("IT2")),
      id="conditionedPanels" 
    )
  )

)))

