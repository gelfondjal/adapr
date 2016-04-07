#' Launches shiny app
#' @export
#' 
#' 

shinyTree <- function (){
  shiny::runApp(
    system.file('shinyApp',                                                    
                package='adapr')) 
  
  
}




#' Launches adapr21 app
#' @export
#' 
#' 

adapr21 <- function (){
  shiny::runApp(
    system.file('adapr21',                                                    
                package='adapr')) 
  library(shinydashboard)
  
}

