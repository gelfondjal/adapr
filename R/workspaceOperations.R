#' Returns all input and output files in a project.
#' @param project.id is the project id to lo
#' @return data.frame with information about project's I/O files
#' @export
#' 
getProjectFiles <- function(project.id=getProject()){
  #
  # Description:  collects a projects files and file metadata
  #
  
  
  file_data <- list(file=NULL)
  
  si <- pullSourceInfo(project.id)
  
  dependency.dir <- si$dependency.dir
  
  # Search for branches (intermediate results loaded by other R scripts)
  
  try({
    treedf <- readDependency(dependency.dir)
    not.this.source <- subset(treedf,(!is.na(dependency)))
    file_data$file<- condenseFileInfo(not.this.source)
  },silent=TRUE)
  
  if(length(file_data$file)==0){
    print("No available files")
    return(NULL)
  }
  
  return(treedf)
  
  
}# END function: getProjectFiles


#' Writes R script workspace to file in script's Results directory
#' @param objectList optional character vector of R object names to write to file
#' @param fileOut optional fileName to write workspace to
#' @param description character string describing workspace
#' @param limit positive integer indicating how many R script workspaces are preserved within this project
#' @param preserve logical indicating whether the workspace file is protected form deletion regarding the limit parameter.
#' @return return form base::save from workspace write to file.
#' @details Writes workspace to file, by default it writes all R objects except source_info object.
#' The function is useful for interrogating freshly written R scripts without having to rerun. Also, can be used to
#' pass multiple objects from one R script to the next. The limit parameter limits the file memory space that these workspace
#' dumps can leave behind by deleting older workspace dumps. The preserve logical protects the workspace dump from deletion.
#' The preserve argument should be TRUE if other R scripts depend on the workspace! Workspace file ends end the file suffix ".ws"
#' @export
#' @examples
#'\dontrun{
#' # Within R script: don't preserve, but save last 4 workspaces
#' writeWorkspace(limit=4)
#' # Within R script: preserve, workspace is used by another R script
#' writeWorkspace(objectList=c("my","favorite","Robjects"),
#' fileOut="usefulRObjects",description="load me",preserve=TRUE)
#'} 
#'


writeWorkspace <- function(objectList=ls(envir=parent.frame()),fileOut=NA,description="",limit=2,preserve=FALSE){
  
  description <- ifelse(description=="","workspace dump:preserved",description)
  
  project <- getSourceInfo()$project.id
  
  fileOut <- ifelse(is.na(fileOut),paste0(getSourceInfo()$file$db.name,".ws"),fileOut)
  
  if(preserve){	
    
    objectList <- setdiff(objectList,"source_info")
    
    out <- Write(objectList,fileOut,description,function(obj,file0){base::save(list=obj,file=file0)})
  }else{
    lister <- objectList
    print(lister)
    out <- base::save(list=lister,file=file.path(resultsDir(),fileOut))
    
  }
  if(!is.na(limit)){
    
    workspaces <- listWorkspaces(project)
    
    fileList <- subset(workspaces,!workspaces$preserved)
    
    if(nrow(fileList)>limit){
      
      # Destroy oldest workspace
      
      fileList <- fileList[order(fileList$target.mod.time,decreasing=TRUE),]
      
      destroy <- fileList[(limit+1):nrow(fileList),]
      
      file.remove(file.path(destroy$filepath))
      
    }
    
  }
  
  return(out)
}


#' Lists the R script workspaces in the project's Results directory that are available for loading.
#' @param project character project ID to list the workspaces from
#' @return data.frame with information about project's workspace files
#' @export
#' @examples
#'\dontrun{
#' setProject("adaprHome")
#' listWorkspaces()
#'} 
#'  

listWorkspaces <- function(project=getProject()){
  
  fileList <- getProjectFiles(project)
  fileList <- subset(fileList,(fileList$dependency=="out")&(grepl("\\.ws$",fileList$target.file)))
  
  fileList <- subset(fileList,select=c("source.file","target.file","target.description","target.mod.time"))
  
  fileList$preserved <- TRUE
  
  fileListDumps <- grep("\\.ws$",list.files(file.path(getProjectPath(project),project.directory.tree$results),recursive=TRUE,full.names=TRUE),value=TRUE)
  fileListDumps <- data.frame(target.file=fileListDumps,target.description="file dump, not preserved",preserved=FALSE,stringsAsFactors=FALSE)	
  
  fileListDumps$target.mod.time <- as.character(file.info(file=fileListDumps$target.file)$mtime)
  
  fileListDumps$source.file <- basename(dirname(fileListDumps$target.file))
  
  fileListDumps$target.file <- basename(fileListDumps$target.file)
  
  
  out <- merge(fileList,fileListDumps,all=TRUE)
  
  out$filepath <- file.path(getProjectPath(project),project.directory.tree$results,out$source.file,out$target.file)
  
  out <- out[order(out$preserved,decreasing=TRUE),]
  
  out <- subset(out,!duplicated(out$filepath))
  
  out$fileExists <- file.exists(out$filepath)
  
  return(out)
  
}

#' Loads the R script workspace int
#' @param script character string of R script to load the workspace from. By default loads the current R script's workspace.
#' @param file character string of workspace file to load. If missing then loads last workspace of current R script.
#' @param project character project ID to load the workspaces from
#' @return output from base::load of workspace file
#' @details Used within an adapr R script or in the console. If file is stated then script parameter is ignored.
#' @export
#' @examples
#'\dontrun{
#' # To load a dumped workspace
#' loadWorkspace(script="mylastRscript.R",project="myProjectID")
#' # To load a preserved workspace from another R script
#' loadWorkspace(file="myFavoriteObjects.ws")
#'} 
#'  

loadWorkspace <- function(file=NA,script=getSourceInfo()$file$file,project=getProject()){
  
  filer <- ifelse(is.na(file),paste0(gsub("\\.","_",script),".ws"),file)
  
  fullName <- file.path(getProjectPath(project),project.directory.tree$results,script,filer)
  
  #testhat::expect_true(file.exists(fullName))
  
  out <- base::load(fullName,envir=parent.frame())
  
  file.info <- getFileInfo(options()$adaprScriptInfo, data = "", 
                           file0 = "", path.grep = fullName)
  
  df.update <- data.frame(target.file = file.info[["file"]], 
                          target.path = file.info[["path"]], target.description = file.info[["description"]], 
                          dependency = "in", stringsAsFactors = FALSE)
  
  options()$adaprScriptInfo$dependency$update(df.update)
  
  return(out)
}
