#' Return function for writing common file types
#' @param filename file to be written
#' @return function for writing file
#' @details Uses the file suffix to return write.csv, png, pdf, save
#' @export 
#' @examples 
#'\dontrun{
#'  identical(utils::write.csv,guess.write.fcn("data.csv"))
#'} 
guess.write.fcn <- function(filename){
  
  # returns the function for reading a file with the suffix in x
  
  suffix <- tolower(gsub(".*\\.","",filename))
  
  if(suffix=="png"){return(grDevices::png)}
  if(suffix=="pdf"){return(grDevices::pdf)}
  if(suffix=="csv"){return(utils::write.csv)}
  if(suffix=="rdata"){return(save)}
  
  
  return(FALSE)
  
}