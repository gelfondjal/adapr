#' Project directory tree structure
#' contains the relative directory structure
#' included analysis, data, texidr, dependency.dir, support functions, and library bank 
#' locations
#' @export
#' 
project.directory.tree <- list("analysis"="Programs","data"="Data",results="Results",tex.dir="texdir",dependency.dir = file.path("Programs","Dependency"),
                               "support"=file.path("Programs","support_functions"),library.bank="Libraries")
