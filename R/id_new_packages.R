#' Lower level function that captures packages that are explicitly loaded, not loaded automatically
#' @param library.data.file CSV File with a set of library names and repository locations
#' @return Libraries loaded that were not automatically loaded
#' @details Captures unaccounted for library within library information file. Not for direct use.
#' @export
#' 
#' 
idPackages <- function(library.data.file){
 
 #library.data.file= "/Users/Gelfond/Documents/Projects/Aflatoxin/Programs/support_functions/common_libs.csv"
 
  packages.info.all <- utils::read.csv(library.data.file,as.is=TRUE)
  
  adapr_packs <- c("adapr","stats","graphics","grDevices","utils","base")
   notadapr <- setdiff(devtools::loaded_packages()$package,adapr_packs)
   
   missing <- setdiff(notadapr,packages.info.all$Package)   					   					
  
   #Only load nonspecific packages if subgroup is null
  
  if(length(missing)){
    
    packageInfo <- devtools::session_info(include_base = TRUE)$packages
    
    packageInfo <- subset(packageInfo,packageInfo$package %in% missing)
    
    packageInfo$repos <- ifelse(packageInfo$source=="Bioconductor","bioC",NA)
    
    packages.info <- rbind(packages.info.all,data.frame(Package=packageInfo$package,repos=packageInfo$repos,specific=TRUE))
    utils::write.csv(packages.info,library.data.file,row.names=FALSE)
    
  }    
  
 
  return(missing)	
  
  
}
