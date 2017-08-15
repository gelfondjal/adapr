#' Dependency class
#' @importFrom methods new
#' @export
# Make dependency class
dependency <- methods::setRefClass("dependency",
                          fields = list( data = "data.frame"),
                          methods = list(
                            update = function(df.update) {
                              ## the following string documents the edit method
                              'Updates the dependency object with a read in or write out
	'
                              if(nrow(df.update)==0){
                                data <<- df.update
                                invisible(data)
                              }
                              
                              new_row <- data[1,] # Retrieves part of dependency information that does not change
                              
                              for(col.iter in names(df.update)){new_row[[col.iter]] <- df.update[[col.iter]]}
                              
                              data <<- plyr::rbind.fill(data,new_row)
                              
                              invisible(data)
                            })
)
