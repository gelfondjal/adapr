#' Updates latest adapr from gitHub.
#' @return message from github
#' @export
#' @examples 
#'\dontrun{
#' adaprUpdate()
#'} 
adaprUpdate <- function(){
  
  output <- devtools::install_github("gelfondjal/adapr")
  
  return(output)
  
}
