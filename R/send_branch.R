#' Copy dependent programs to swap directory
#' @param branch_cut filename of the base of the branch to send
#' @param all logical indicating whether to send all branches in project 
#' @export
#' 

send.branch <- function(branch_cut,all=FALSE){
  
  project_info <- get.project.info.si(source_info)
  
  if(!all){
    
    v.name <- V(project_info$graph)$name[V(project_info$graph)$file==branch_cut]
    
    branch <- na.exclude(graph.bfs(project_info$graph,v.name,unreachable=FALSE)$order)
    
    branch.names <- V(project_info$graph)$file[branch]  # These are in order
    
    
    programs <- subset(subset(project_info$tree,source.file %in% c(branch_cut,branch.names)),!duplicated(source.file))
    
    
    
  }else{
    
    branch <- na.exclude(topological.sort(project_info$graph))
    
    branch.names <- V(project_info$graph)$file[branch]  # These are in order
    
    programs <- subset(subset(project_info$tree,source.file %in% c(branch.names)),!duplicated(source.file))
    
  }	
  
  
  branch.names <- branch.names[branch.names %in% programs$source.file]
  
  program.paths <- file.path(programs$source.file.path,branch.names)
  
  dependency.file.paths <- file.path(programs$source.file.path,"Dependency",paste0(branch.names,".txt"))
  
  swap.directory <- get.project.swap.directory(source_info$project.id)
  
  branch.dir <- file.path(swap.directory,"Branches",branch_cut)
  
  dir.create(branch.dir,recursive=TRUE,showWarnings=FALSE)
  
  
  prog.dir <-file.path(branch.dir,"Programs")
  dep.dir <- file.path(branch.dir,"Dependency")
  sup.dir <- file.path(branch.dir,"support_functions_dir")
  
  dir.create(prog.dir,showWarnings=FALSE)
  dir.create(dep.dir ,showWarnings=FALSE)
  
  dir.create(sup.dir ,showWarnings=FALSE)
  
  
  branch.info.out <- data.frame(branch.names,dependency=paste0(branch.names,".txt"))
  
  write.table(branch.info.out,file.path(branch.dir,paste0(branch_cut,".txt")))
  
  for(prog.iter in program.paths){file.copy(prog.iter,file.path(prog.dir,basename(prog.iter)),overwrite=TRUE)}		
  
  for(dep.iter in dependency.file.paths){file.copy(dep.iter,file.path(dep.dir,basename(dep.iter)),overwrite=TRUE)}		
  
  # copy.support.functions
  
  file.copy(source_info$support.dir,file.path(sup.dir),recursive=TRUE,overwrite=TRUE)
  
  
}
