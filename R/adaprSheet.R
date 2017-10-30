#' Browse adapr cheat sheet
#' @details Opens pdf from R package within system
#' @export
#' @examples 
#'\dontrun{
#' # Requires pandoc location or RStudio
#' adaprSheet()
#'
#'} 
#'

adaprSheet <- function(){
  return(utils::browseURL(  system.file('cheatsheet_adapr.pdf',package='adapr')))
}