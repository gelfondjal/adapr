#' Make plot of project programs only
#' Summarize all programs.
#' @param project.id Project id of program
#' @return List of data.frame of programs vertices, data.frame of edges, ggplot ,rgrapher=igraph
#' @details Deprecated. See graph.project()
#'@examples 
#'\dontrun{
#'  create_program_graph_v2("adaprHome")
#'} 
#'  
 
create_program_graph_v2<- function(project.id){
	
# computes transitively connected subpgraph of project DAG
# given a project id (project.id)
# Uses nicer plot parameters
# #equire(ggplot2)
# #equire(plyr)	
# 
# si <- pull_source_info(project.id)
# 
# projinfo <- get.project.info.si(si)
# 
# unsync.vertex <- c("",as.character(sync.test.si(si)$sources.to.sync$fullname.abbr))
# 
# projgraph <- projinfo$graph
# 
# sources <- unique(projinfo$tree$source.file)
# 
# vertexnames <- subset(projinfo$all.files,file %in%sources)$fullname.abbr
# 
# 
# synccolors <- c("aquamarine3","darkorange2")
# names(synccolors) <- c("Synchronized", "Not Synchronized")
# 
# 
# 
# if(length(vertexnames)==1){
# 
# dfo <- data.frame(v=vertexnames[1],x=0,y=0)	
# 
# text.nudge0 <- 0.15
# dotsize0 <- 10
# text.size0 <- 10
# 
# dfo$synccolor <- as.character(ifelse(dfo$v %in% unsync.vertex,"Not Synchronized","Synchronized"))
# 
# dfo$synccolor <- factor(dfo$synccolor,levels=c("Synchronized","Not Synchronized"))
# 
# dfo <- merge(dfo,subset(projinfo$all.files,select=c("fullname.abbr","fullname","description")),by.x="v",by.y="fullname.abbr")
# 
# proj.gg <- ggplot(dfo,aes(x=x,y=y,label=basename(as.character(v))))+
# geom_point(aes(colour=dfo$synccolor),size=dotsize0,alpha=0.7)+
# geom_point(shape = 1,size = dotsize0,colour = "grey70", stroke=2)+
#  geom_text(nudge_y=text.nudge0,size=text.size0,color="black")+scale_x_continuous(limits=c(-1,1))+scale_y_continuous(limits=c(-1,1))+
# theme(axis.line=element_blank(),axis.text.x=element_blank(),
# axis.text.y=element_blank(),axis.ticks=element_blank(),
# axis.title.x=element_blank(),
# axis.title.y=element_blank(),legend.position="bottom",
# panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
# panel.grid.minor=element_blank(),plot.background=element_blank())+ggtitle(paste(project.id,"- R Script Graph"))+theme(text=element_text(size=20))
# 
# proj.gg <- proj.gg+ scale_color_manual(name = element_blank(), # or name = element_blank()
# labels = c("Synchronized", "Not Synchronized"),
# values = synccolors)
# 
# isg <- induced_subgraph(projgraph,vertexnames)
# 
# runorder <- data.frame(v=topological.sort(isg)$name,run.order=1:length(vertexnames))
# dfo <- merge(dfo,runorder,by='v')
# 
# 	
#             
#              
# return(list(vertex=dfo ,edges=NA,ggplot=proj.gg,rgrapher=isg))
# } 
#  
# lo <- layout.sugiyama(projgraph)
#  
# tp <- function(x){
#  
#   x <- x[,2:1]
#  
#   x[,1] <- max(x[,1])- x[,1]
#  
#   return(x)
# }
#   
# 
#   
#     
# vertex <- vertexnames[2]  
# vertexTo <- vertexnames[4]
# 
# longgraph <- NULL
# 
# for(vertex in vertexnames){
# 	
# 	shortgraph <- data.frame(from=vertex,to=vertex)
# 	
# 	tos <- c()
# 	
# 	for(vertexTo in vertexnames){
# 		
# 		shortguy <- shortest_paths(projgraph,vertex,vertexTo)
# 		
# 		if(length(shortguy$vpath[[1]])==3){
# 			
# 			tos <- c(vertexTo,tos)
# 			
# 		}# if one step path
# 
# 		}# loop over targets	
# 			
# 	   if(length(tos)>0){
# 	   	
# 	   	 shortgraph <- rbind(shortgraph,data.frame(from=vertex,to=tos))
# 	   }# if any targets connected
# 		
# 	
# 	longgraph <- rbind(longgraph,shortgraph)
# 	
# } 
# 
#   
# isg <- simplify(graph.data.frame(longgraph))
#  
# #plot(isg) 
#  
# #plot(isg,layout=tp(layout.sugiyama(isg)$layout))
#  
# isgdf <- igraph::as_data_frame(isg)
# 
# noedges <- 0
#   
# if(nrow(isgdf)==0){
#   
#   noedges <- 1
#   isgdf <- igraph::as_data_frame(graph.data.frame(longgraph))
#   
#   
# }
#  
# dfo <- tp(layout.sugiyama(isg)$layout)
# colnames(dfo) <- c("x","y")
#  
# dfo <- data.frame(dfo,v=V(isg)$name)
#  
# todfo <- dfo
# names(todfo)[1:2] <- c("x2","y2")
#  
# tos <- merge(isgdf,todfo,by.x="to",by.y="v")
#  
# froms <- merge(tos,dfo,by.x="from",by.y="v")
# 
# ranger <- range(c(froms$x,froms$x2))
# 
# span <- 0.1*abs(diff(ranger))
# 
# horizontal.range <- c(ranger[1]-span,ranger[2]+span)
# 
# rangery <- range(c(froms$y,froms$y2))
# 
# graph.height <- length(unique(c(froms$y,froms$y2)))
# 
# graph.width <- length(unique(c(froms$y,froms$y2)))
#               
#               
#               
# dotsize0 <- 10
# if(graph.height>5){dot.size <-1+10/graph.height}else{dot.size0 <- 10}              
# 
# text.nudge0 <- 0.05*abs(diff(rangery))
# 
# 
# text.nudge0 <- dotsize0/20
# 
# #if(graph.height>5){text.nudge0 <- 0.05 +text.nudge0/graph.height}              
# 
# text.size0 <- 5
# 
# if(graph.width>5){text.size0 <-2 + 2*text.size0/graph.width}              
# 
# dfo$synccolor <- as.character(ifelse(dfo$v %in% unsync.vertex,"Not Synchronized","Synchronized"))
# 
# dfo$synccolor <- factor(dfo$synccolor,levels=c("Synchronized","Not Synchronized"))
# 
# dfo <- merge(dfo,subset(projinfo$all.files,select=c("fullname.abbr","fullname","description")),by.x="v",by.y="fullname.abbr")
# 
# 
# proj.gg <- ggplot(dfo,aes(x=x,y=y,label=basename(as.character(v))))+
#  geom_point(aes(colour=dfo$synccolor),size=dotsize0,alpha=0.7)+
# geom_point(shape = 1,size = dotsize0,colour = "grey70", stroke=2)+
#  geom_text(vjust=-0.5,size=text.size0,color="black")
# 
# proj.gg <- proj.gg + annotate(geom="label",x=dfo$x,y=dfo$y-0.125,label=dfo$description)
# 
# 
# if(noedges==0){
# 
# proj.gg <- proj.gg+annotate(geom="segment",x=froms$x,y=froms$y,xend=froms$x2,yend=froms$y2,
#         arrow=arrow(length=unit(0.2,"cm"),type="closed"),alpha=0.5/ifelse(graph.width>5,5,1))
#  
# }  
# proj.gg <- proj.gg +scale_x_continuous(limits=horizontal.range)+theme(axis.line=element_blank(),axis.text.x=element_blank(),
# axis.text.y=element_blank(),axis.ticks=element_blank(),
# axis.title.x=element_blank(),
# axis.title.y=element_blank(),legend.position="bottom",
# panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
# panel.grid.minor=element_blank(),plot.background=element_blank())+ggtitle(paste(project.id,"- R Script Graph"))+theme(text=element_text(size=20))
# 
# proj.gg <- proj.gg+ scale_color_manual(name = element_blank(), # or name = element_blank()
# labels = c("Synchronized", "Not Synchronized"),
# values =synccolors)
# isg <- induced_subgraph(projgraph,vertexnames)
# runorder <- data.frame(v=topological.sort(isg)$name,run.order=1:length(vertexnames))
# dfo <- merge(dfo,runorder,by='v')
#             
# 
# return(list(vertex=dfo,edges=froms,ggplot=proj.gg,rgrapher=isg))


} #
 
 
