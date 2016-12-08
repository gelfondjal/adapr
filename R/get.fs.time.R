#' Retrun time of file system
#' @description Writes to temporary file and extracts mod time with file.info
#' @return The file system write time
#' @export
#' @examples 
#'\dontrun{
#' get.fs.time() 
#'} 
#'
get.fs.time <- function(){
  
    # Get the filesystem write time
    tf <- tempfile()
    write(0,tf)
    timeout <- file.info(tf)$mtime
    file.remove(tf)
    return(timeout)
    
}