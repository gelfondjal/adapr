#' Lists the branches available for loading in the adapr project
#' @param si is source_info object
#' @return dataframe of descriptions available branches
#' @export
#' @examples 
#'\dontrun{
#' source_info <- create_source_file_dir("adaprHome","tree_controller.R")
#` listBranches()
#'} 
listBranches <- function(si=get("source_info")){
  
  file_data <- si$all.files
  
  warning("deprecated: see list.branches")
  
  if(length(file_data$file)==0){
    print("No available branch files")
    return(NULL)
  }
  
  file_sub  <- subset(file_data,grepl("Rdata$",file_data$file),select = c("file","path","description"))
  
  if(length(file_sub$file)==0){
    print("No available branches")
    return(NULL)
  }
  
  file_sub$path <- gsub(".*/","",file_sub$path)
  return(file_sub)
  
}


#' Lists the branches available for loading in the adapr project
#' @param project.id project to find branches within
#' @return dataframe of descriptions available branches
#' @export
#' @examples 
#' \dontrun{
#' list.branches("adaprHome")
#'} 
#'
#' 
list.branches <- function(project.id=get.project()){
  
  file_data <- list(file=NULL)
  
  si <- pull_source_info(project.id)
  
  dependency.dir <- si$dependency.dir
  
  try({
    treedf <- Harvest.trees(dependency.dir)
    not.this.source <- subset(treedf,(!is.na(dependency)))
    file_data$file<- Condense.file.info(not.this.source)
  },silent=TRUE)
  

  if(length(file_data$file)==0){
    print("No available branch files")
    return(NULL)
  }
  
  file_sub  <- subset(file_data$file,grepl("Rdata$",file_data$file$file),select = c("file","path","description"))
  
  if(length(file_sub$file)==0){
    print("No available branches")
    return(NULL)
  }
  
  file_sub$path <- gsub(".*/","",file_sub$path)
  return(file_sub)
  
}


#' Lists the R scripts in the adapr project
#' @param  project.id project.id
#' @return dataframe of R scripts and descriptions
#' @details Deprecated see list.programs
#' @export
#' 
listScripts<- function(project.id=get("source_info")$project.id){
  warning("deprecated: see list.programs")
  
  trees <- Harvest.trees(file.path(get.project.path(project.id),project.directory.tree$dependency.dir))
  
  programs <- subset(trees,!duplicated(file.path(trees$source.file.path,trees$source.file)),
                    select=c("source.file","source.file.description"))
  
  
  return(programs)
  
}

#' Lists the R scripts in the adapr project
#' @param  project.id project.id
#' @return dataframe of R scripts and descriptions
#' @export
#' @examples 
#' \dontrun{
#' list.programs("adaprHome")
#'} 
#'
list.programs <- function(project.id=get.project()){

  trees <- Harvest.trees(file.path(get.project.path(project.id),project.directory.tree$dependency.dir))
  
  programs <- subset(trees,!duplicated(file.path(trees$source.file.path,trees$source.file)),
                     select=c("source.file","source.file.description"))
  
  return(programs)
  
}











#' Lists the data files available for reading in the adapr project
#' @param si is source_info object
#' @return description of data files
#' @details Deprecated. See list.datafiles()
#' @export
#' 
listDatafiles <- function(si=get("source_info")){
  
  warning("deprecated: see list.datafiles")
  
  allfiles <- data.frame(file=list.files(si$data.dir,recursive=TRUE,full.names = 1),stringsAsFactors = FALSE)
  
  allfiles$path <- dirname(substring(as.character(allfiles$file),nchar(si$project.path)+2,nchar(as.character(allfiles$file))))
  
  allfiles$file <- basename(allfiles$file)

    
  file_data <- si$all.files
  file_sub <- data.frame()
  try({
  file_sub <- subset(file_data,file_data$path==si$data.dir,select = c("file","path","description"))
  })
  
  if(is.null(file_sub)){
    return(allfiles)
    }
  
  
  if(nrow(file_sub)==0){
    
    return(allfiles)
    
  }

  file_sub$path <- gsub(".*/","",file_sub$path)
  
  allfiles <- merge(allfiles,file_sub,by=c("file","path"),all.x=TRUE)
  
  return(allfiles)
  
}

#' Lists the data files available for reading in the adapr project
#' @param project.id Project to look for data files within
#' @return description of data files
#' @export
#' @examples 
#' \dontrun{
#' list.datafiles("adaprHome")
#'} 
#'



list.datafiles <- function(project.id=get.project()){
  
  
  si <- pull_source_info(project.id)
  
  si$data.dir <- file.path(si$project.path,project.directory.tree$data)
  allfiles <- data.frame(file=list.files(si$data.dir,recursive=TRUE,full.names = 1),stringsAsFactors = FALSE)
  
  allfiles$path <- dirname(substring(as.character(allfiles$file),nchar(si$project.path)+2,nchar(as.character(allfiles$file))))
  
  allfiles$file <- basename(allfiles$file)
  
  file_data <- list(file=NULL)
  
  dependency.dir <- si$dependency.dir
  
  try({
    treedf <- Harvest.trees(dependency.dir)
    not.this.source <- subset(treedf,(!is.na(dependency)))
    file_data$file <- Condense.file.info(not.this.source)
  },silent=TRUE)
  
  
  file_sub <- data.frame()
  try({
    file_sub <- subset(file_data$file,file_data$file$path==si$data.dir,select = c("file","path","description"))
  })
  
  if(is.null(file_sub)){
    return(allfiles)
  }
  
  
  if(nrow(file_sub)==0){
    
    return(allfiles)
    
  }
  
  file_sub$path <- gsub(".*/","",file_sub$path)
  
  allfiles <- merge(allfiles,file_sub,by=c("file","path"),all.x=TRUE)
  
  return(allfiles)
  
}





#' Opens results directory
#' @param si is source_info object
#' @param project.id character string specifies project 
#' @param rscript character string specifies the R script result directory to open
#' @details Deprecated: use show.results(). Uses BrowseURL to open results directory
#' @export
#' 
showResults <- function(si=get("source_info"),project.id="",rscript=""){
  warning("deprecated: see show.results")
  if(project.id==""){
    utils::browseURL(si$results.dir)
  }else{
    resultdir <- file.path(get.project.path(project.id),project.directory.tree$results,rscript)
    utils::browseURL(resultdir)
  }
}



#' Opens results directory of project or R script within a project
#' @param project.id character string specifies project 
#' @param rscript character string specifies the R script result directory to open
#' @details Use BrowseURL to open results directory
#' @export
#' @examples 
#' \dontrun{
#' show.results("adaprHome")
#'} 
#'
show.results <- function(project.id=get.project(),rscript=""){
  
  si <- pull_source_info(project.id)
  
  if(rscript==""){
    utils::browseURL(file.path(get.project.path(project.id),project.directory.tree$results))
  }else{
    resultdir <- file.path(get.project.path(project.id),project.directory.tree$results,rscript)
    utils::browseURL(resultdir)
  }
}


#' Opens project directory
#' @param si is source_info object
#' @param project.id character string specifies project to open
#' @details Use BrowseURL to open project directory
#' @export
#' 
showProject <- function(si=get("source_info"),project.id = ""){
  warning("deprecated: see show.project")
  if(project.id==""){
    utils::browseURL(si$project.path)
  }else{
    utils::browseURL(get.project.path(project.id))
  }
}



#' Opens project directory
#' @param project.id character string specifies project to open
#' @details Use BrowseURL to open project directory.
#' @export
#' @examples 
#' \dontrun{
#' show.project("adaprHome")
#'} 
#'
 
show.project <- function(project.id =get.project()){
 
    utils::browseURL(get.project.path(project.id))

}


