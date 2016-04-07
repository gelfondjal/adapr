library(plotly)
library(shinydashboard)


header <- dashboardHeader(
  disable = FALSE,
  title = "ADAPR"
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Select Project", tabName = "selectproject", icon = icon("bars")),
    menuItem("Make Programs", tabName = "programslib", icon = icon("glyphicon glyphicon-edit", lib = "glyphicon")),
    menuItem("Run Report", tabName = "report", icon = icon("file")),
    menuItem("Synchronize", tabName = "synch", icon = icon("glyphicon glyphicon-refresh", lib = "glyphicon")),
    menuItem("Send Results", tabName = "send", icon = icon("envelope")),
    menuItem("Configure", tabName = "config", icon = icon("toggle-off"))
  )
)      


body <- dashboardBody(
  tabItems(
    tabItem("selectproject",
      fluidRow(
        column(width=3,
          box(title = "Choose an Existing Project", status = "info", width = 12, collapsible = FALSE, solidHeader = TRUE,
            htmlOutput("selectUI")
          ),
          helpText(h1(strong("OR"),align='center')),
          box(title = "Create a New Project", status = "info", width = 12, collapsible = FALSE, solidHeader = TRUE,
            textInput('project.id.make', "Project ID:", value = "Example project"),
            textInput('project.directory', "Project directory:",
                      value= gsub("(^.*)(\\/.*$)","\\1",get_orchard()$project.path[1])),                      
            br(),
            textInput("swap.directory", label = "Publish directory:",
                      value=gsub("(^.*)(\\/.*$)","\\1",get_orchard()$swap.directory[1])),
            br(),br(),
            actionButton("submitProject","Create project")
          )
        ),
        column(width=9,
          h3(strong(uiOutput("projectselected"))),
          box(status = "info", width = 12, height = 525, collapsible = FALSE, solidHeader = FALSE,
            h4(tableOutput("createproject"))
          )
        )    
      )
    ),
    tabItem("programslib",
      fluidRow(
        column(width=3,
          box(title = "1. Make Program", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            textInput('filename', "Filename:", value = "MyProgram.R"),
            br(),
            textInput("description", label = "Program description:", value = "Description of your program..."),
            br(),br(),
            actionButton("submit","Create program")
          ),
          box(title = "2. Manage Libraries", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            textInput("library.name", label = "Library Name", value = "mylibrary"),
            textInput("library.install", label = "Install Command (bioC or '' for CRAN)", value = ""),
            selectInput('library.specific', label="Library for specific program?", c("FALSE","TRUE"),"FALSE"),                                                                            
            br(),br(),
            actionButton("submitLibrary","List/Add Library")
          )
        ),
        #column(width=1,
        #  tags$img(src='arrow.png',height='60',width='130')
          #helpText(h1("-------->"))
        #),
        column(width=9,
        h3(strong(htmlOutput("projectselected2"))),
          box(status = "info", width = 12, height = 635, collapsible = FALSE, solidHeader = FALSE,
            tableOutput("myChart"),
            tableOutput('text1'),
            tableOutput('libraryTable'),
            tableOutput('addLibraryTable')
          )
        )
      )
    ),
    tabItem("report",
      fluidRow(
        column(width=3,
          box(title = "Report", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            actionButton("submitReport","Create report"),br(),br() ,actionButton("make_report_graph","Examine Program Graph")            
          ),
        #),
        #column(width=3,
          box(title = "App", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            actionButton("submitRunApp","Run App"),br(),br(),
            htmlOutput("selectAppUI")
          )
        ),
        column(width=9,
        h3(strong(htmlOutput("projectselected3"))),
        box(status = "info", width = 12, height = 260, collapsible = FALSE, solidHeader = FALSE,
            h4(tableOutput("projectus")),
            h4(tableOutput("runApp"))
         ),
        box(title = NULL, status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = FALSE,
            plotOutput("ProgramDAG_report", height = 500,
                       # Equivalent to: click = clickOpts(id = "plot_click")
                       click = "ProgramDAG_click_report",
                       brush = brushOpts(
                         id = "ProgramDAG_brush_report",
                         resetOnNew = TRUE
                       ))
            
        ),
        box(title = NULL, status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = FALSE,
            
            plotOutput("ProgramDAG_report_select_graph", height = 500,
                       # Equivalent to: click = clickOpts(id = "plot_click")
                       click = "ProgramDAG_click_report_browse",
                       brush = brushOpts(
                         id = "ProgramDAG_brush_report_browse",
                         resetOnNew = TRUE
                       )),
            verbatimTextOutput("brush_info_select")
            
        ),
        
        
        
        box(title = NULL, status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = FALSE,
            tableOutput("ProgramDAG_report_select")
            
        )
        )
      )
    ),  
    tabItem("synch",
      fluidRow(
        column(width=3,
          box(title = "Synchronize", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            actionButton("submitSyncTest","Check Sync"),
            br(),br(),
            actionButton("submitSyncRun","Synchronize Now"),
            br(),br(),
            textInput('commit.message', "Commit Message", value = "Message"),
            br(),br(),
            helpText("Version Control Snapshot:"),
            actionButton("submitCommitRun","Commit Project")
          ),
          box(title = "Plot", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            actionButton("makeGraph","Update Project Plot")
          )
        ),
        column(width=9,
        h3(strong(htmlOutput("projectselected4"))),
        box(status = "info", width = 12, height = 300, collapsible = FALSE, solidHeader = FALSE,
            tableOutput("syncTest"),
            br(),h4(textOutput("syncText")),
            br(),h4(textOutput("progressbar")),
            br(),h4(textOutput("CommitOut"))
         ),
         box(title = "Project Graph", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            verbatimTextOutput("brush_info"),
            plotOutput("ProgramDAG", height = 500,
                       # Equivalent to: click = clickOpts(id = "plot_click")
                       click = "ProgramDAG_click",
                       brush = brushOpts(
                       id = "ProgramDAG_brush",
                       resetOnNew = TRUE
                       ))
            
        )
      )
    )),  
    tabItem("send",
      fluidRow(
        column(width=3,
          box(title = "Send stuff from your project", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            #helpText(h4("Your choices:")),
            #textOutput('variables'),
            #br(),br(),
            textInput('filename.send', "Result File:", value="Path from main results directory"),
            actionButton("submitPublish","Add File to Publish"),
            actionButton("publish.button","Publish files"),
            br(),br(),br(),br(),
            selectInput('send.data',"Send Data?",c("FALSE","TRUE"),"FALSE"),
            actionButton("submitSend","Publish Project")
          )
        ),
        column(width=9,
        h3(strong(htmlOutput("projectselected5"))),
          box(status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = FALSE,
            tableOutput("Programs"),
            br(),h4(textOutput("PublishedText")),
            br(),h4(textOutput("Sent")),
            br(),helpText(h4("Your choices to publish:")),
                 textOutput('variables')
          )
        )
      )
    ),
    tabItem("config",
      fluidRow(
        column(width=3,
          box(title = "Configure", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,
            helpText(h3("Configure First Project")),
            textInput("project1.directory","Project Directory",value=project.path.start),
            textInput("publish.directory","Publish Directory:",value=publish.path.start),
            actionButton("submitFirst.project","Setup First Project")
          ),
          box(title = "Git", status = "info", width = 12, height = NULL, collapsible = FALSE, solidHeader = TRUE,             
            helpText(h3("See if Git is working")),
            actionButton("submitGitCheck","Check Git"),
            br(),br(),
            helpText(h4("Log into Git")),
            textInput("git.username","Username:",value=gitAuthor),
            textInput("git.email","Email:",value=gitEmail),
            br(),
            actionButton("submitGitLogin","Login")
          )
        ),
        column(width=9,
          box(status = "info", width = 12, collapsible = FALSE, solidHeader = FALSE,
              h4(uiOutput("First.project")),
              br(),h4(uiOutput("Git")),
              br(),h4(textOutput("Gitlogin")),
              br(),h4(textOutput("IT2"))
            ),
          box(title = "Install the latest version of adapr", width = 3, height = NULL, collapsible = FALSE, solidHeader = TRUE, 
              actionButton("installit2","Install")
          )
        )
      )
    )
  )
)
shinyUI(dashboardPage(header, sidebar, body, skin = 'black'))