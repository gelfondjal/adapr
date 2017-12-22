#' Browse adapr cheat sheet
#' @details Opens pdf from R package within system. Requires browser assigned to open pdf file.
#' @export
#' @examples 
#' # Requires browser assigned to open pdf file.
#' \dontrun{
#' adaprSheet()
#' }
#'

adaprSheet <- function(){
  return(utils::browseURL(  system.file('cheatsheet_adapr.pdf',package='adapr')))
}