#' Retrun time of file system
#' @description Writes to temporary file and extracts mod time with file.info
#' @param directory path to directorly
#' @return The file system write time
#' @export
#' @examples 
#'\dontrun{
#' getFileSysTime() 
#'} 
#'
getFileSysTime <- function(directory=""){
  
    # Get the filesystem write time
    if(directory==""){
      tf <- tempfile()
    }else{
      tf <- file.path(directory,"ADAPR_WILL_DELETE.me")
    }
    write(0,tf)
    timeout <- file.info(tf)$mtime
    file.remove(tf)
    return(timeout)
    
}
