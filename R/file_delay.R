filetime <- function(testdir){
  
 testdir <- "/Volumes/WORKING2/Projects/Goros/test_adapr/Data"
  
 # testdir <- "/Users/Gelfond/Documents/Projects/Need2Know/Subprojects/N2K2/Data"
  
  
  tempfile <- file.path(testdir,"DELETEME.NOW")
  cputime <- Sys.time()
    
  write(cputime,tempfile)
  
  filetime <- file.info(tempfile)$ctime
  
  file.remove(tempfile)
  
  delay <- cputime - filetime
  
  print(delay)
  
  return(delay)
  
  
  
  
  
}
