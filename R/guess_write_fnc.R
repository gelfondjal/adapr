#' Return function for writing common file types
#' @param filename file to be written
#' @return function for writing file
#' @details Uses the file suffix to return write.csv, png, pdf, save
#' @export 
#' 
guess.write.fcn <- function(filename){
  
  # returns the function for reading a file with the suffix in x
  
  suffix <- tolower(gsub(".*\\.","",filename))
  
  if(suffix=="png"){return(png)}
  if(suffix=="pdf"){return(pdf)}
  if(suffix=="csv"){return(write.csv)}
  if(suffix=="rdata"){return(save)}
  
  
  return(FALSE)
  
}