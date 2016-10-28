#' Launches Main app
#' @export
#' 
#' 

adaprApp <- function (){
  library(shiny)
  library(shinydashboard)
  shiny::runApp(
    system.file('adapr21',                                                    
                package='adapr')) 

  
}

