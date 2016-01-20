#' Return function for reading common file types
#' @param filename for file to be read
#' @return function for reading 
#' @details Uses the file suffix to return csv, read.delim, read.xls
#' @export
#' 
guess.read.fcn <- function(filename){
  
  # returns the function for reading a file with the suffix in x
  
  suffix <- tolower(gsub(".*\\.","",filename))
  
  if(suffix=="csv"){return(read.csv)}
  if(suffix=="txt"){return(read.delim)}
  if(suffix %in% c("xls","xlsx")){return(read.xls)}
  
  return(FALSE)
  
}

