#' Launches Main app
#' @export
#' 
#' 
adaprApp <- function (){
  #ibrary(shiny)
  #ibrary(shinydashboard)
  shiny::runApp(
    system.file('adapr21',                                                    
                package='adapr')) 
  
}
