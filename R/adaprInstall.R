#' Install adapr in a library
#' adaprInstall.R
#' @param library.location file path to library
#' @param betaTF logical indicating whether to install from github or CRAN.
#' @return logical for succesful creation or not
#' @details Installs dependencies
#' @export
#' @examples 
#'\dontrun{
#'  adaprInstall(library.location=getProjectLibrary("adaprHome") ,betaTF=FALSE)
#'} 
#'
adaprInstall <- function(library.location=getProjectLibrary("adaprHome"),betaTF=getAdaprOptions()$adaprBeta){

successTF <- FALSE 

 try({

  devtools::install(adaprDependencies(),lib=library.location,dependencies = c("Depends","Imports"))

 },silent = TRUE)
#
# #search_item <- paste("package", "adapr", sep = ":")
# #while(search_item %in% search())
# #{
# #  detach(search_item, unload = TRUE, character.only = TRUE)
# #}

 try({

 if(betaTF){
   devtools::install_github("gelfondjal/adapr",lib=library.location,dependencies = c("Depends","Imports"))
 }else{
   utils::install.packages("adapr",lib=library.location,dependencies = c("Depends","Imports"))
 }



 successTF <- TRUE

 })

   require(adapr)
  
return(successTF)

}