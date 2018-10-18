#' Updates latest adapr from GitHub.
#' @return message from github
#' @export
#' @examples 
#'\dontrun{
#' # Requires internet connection to access GitHub.
#' adaprUpdate()
#'} 
adaprUpdate <- function(){
  
  output <- remotes::install_github("gelfondjal/adapr")
  
  return(output)
  
}
