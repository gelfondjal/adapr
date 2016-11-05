
hashcopy <- function(originals,targetdir){
  
  
out <- rep(NA,length(orginals))
step <- 1
  
for(original in orginals){  
  
rewriteTF <- TRUE
out <- FALSE

target <- file.path(targetdir,basename(orginal))

if(file.exists(target)){
  
  hash <- Digest(original,algo="sha1",file=TRUE,serialize=FALSE)   
  hash.target <- Digest(target,algo="sha1",file=TRUE,serialize=FALSE)
  
  rewriteTF <- hash!=hash.target
  
}# check for target diff


out[step] <- rewriteTF

if(rewriteTF){
  
 out[step] <-  file.copy(originals,target,overwrite=TRUE)
  
}# if target diff

step <-step + 1 

}#loop over originals

return(out)


}