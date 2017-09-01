#' Given Project name, Return project publish directory
#' @param originals character vector of file paths to copy
#' @param targetdir directory to copy to
#' @details Only copies if there is a discrepancy in the file hash
#' @return vector of logicals indicating copy status
#' 
#' 
# hashcopy <- function(originals,targetdir){
#   
#   
# out <- rep(NA,length(originals))
# step <- 1
#   
# for(original in originals){  
#   
# rewriteTF <- TRUE
# out <- FALSE
# target <- file.path(targetdir,basename(original))
# if(file.exists(target)){
#   
#   hash <- Digest(original,algo="sha1",file=TRUE,serialize=FALSE)   
#   hash.target <- Digest(target,algo="sha1",file=TRUE,serialize=FALSE)
#   
#   rewriteTF <- hash!=hash.target
#   
# }# check for target diff
# out[step] <- rewriteTF
# if(rewriteTF){
#   
#  out[step] <-  file.copy(originals,target,overwrite=TRUE)
#   
# }# if target diff
# step <-step + 1 
# }#loop over originals
# return(out)
# }
