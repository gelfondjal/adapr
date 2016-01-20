#' Launches shiny app
#' @export
#' 
#' 

shinyTree <- function (){
  shiny::runApp(
    system.file('shinyApp',                                                    
                package='IT2')) 
  
  
}