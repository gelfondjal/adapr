#' Make project graph with sync status already computed.
#' @param previousGraph output value of createProjectGraph
#' @param project.id character string of project id examined
#' @param message string subtitle added to graph
#' @details Used by syncProject() to disply synchronization progress
#' @export
#' @examples 
#'\dontrun{
#' # Requires pandoc location or RStudio
#' graphData <- graphProject("adaprHome")
#' rapidPlot(graphData)
#'
#'} 
#'
#'

rapidPlot <- function(previousGraph,project.id = getProject(),message="Running"){
  
  synccolors <- c("aquamarine3", "darkorange2","red")
  names(synccolors) <- c("Synchronized", "Not Synchronized","Running")
  
  
  dfo <- previousGraph$vertex
  froms <- previousGraph$edges
  
  #noedges <- length(previousGraph$edges)==0
  noedges <- ifelse(is.na(previousGraph$edges)[1],TRUE,length(previousGraph$edges)==0)
  
  vertexnames <- unique(previousGraph$v$v)
  dotsize0 <- 10
  if (length(vertexnames) == 1) {
    
    text.nudge0 <- 0.15
    dotsize0 <- 10
    text.size0 <- 10
    
    horizontal.range <- c(-1.25, 1.25)
    rangery <- c(-1, 1) * 0.5
    graph.height <- 1 * 0.5
    graph.width <- 1 * 0.5
    span <- 1
  }else{
    
    ranger <- range(c(dfo$x, froms$x2))
    span <- 0.25 * abs(diff(ranger))
    horizontal.range <- c(ranger[1] - span, ranger[2] + span)
    rangery <- range(c(froms$y, froms$y2))
    graph.height <- length(unique(c(froms$y, froms$y2)))
    graph.width <- length(unique(c(froms$y, froms$y2)))
    
  }
  
  dot.size0 <- 10
  if (graph.height > 5) {
    dot.size <- 1 + 10/graph.height
  }else {
    dot.size0 <- 10
  }
  text.nudge0 <- 0.05 * abs(diff(rangery))
  text.nudge0 <- dotsize0/20
  text.size0 <- 4
  if (sum(!(c("x", "y", "v") %in% names(dfo)))) {
    x <- NULL
    y <- NULL
    v <- NULL
    stop("rapidPlot (adapr) error: cannot find vertex")
  }
  if (graph.width > 5) {
    text.size0 <- 2 + 2 * text.size0/graph.width
  }
  
  
  proj.gg <- ggplot2::ggplot(dfo, ggplot2::aes(x = x, y = y, 
                                               label = basename(as.character(v)))) + ggplot2::geom_point(ggplot2::aes(colour = dfo$synccolor), 
                                                                                                         size = dotsize0, alpha = 0.7) + ggplot2::geom_point(shape = 1, 
                                                                                                                                                             size = dotsize0, colour = "grey70", stroke = 2) + ggplot2::geom_text(vjust = -0.5, 
                                                                                                                                                                                                                                  size = text.size0, color = "black")
  proj.gg <- proj.gg + ggplot2::annotate(geom = "label", x = dfo$x, 
                                         y = dfo$y - ifelse(length(vertexnames) > 1, 0.125, 0.1), 
                                         label = dfo$description, size = text.size0)
  if (length(vertexnames) == 1) {
    proj.gg <- proj.gg + ggplot2::scale_y_continuous(limits = rangery)
  }
  if (noedges == 0) {
    proj.gg <- proj.gg + ggplot2::annotate(geom = "segment", 
                                           x = froms$x, y = froms$y, xend = froms$x2, yend = froms$y2, 
                                           arrow = ggplot2::arrow(length = ggplot2::unit(0.2, 
                                                                                         "cm"), type = "closed"), alpha = 0.5/ifelse(graph.width > 
                                                                                                                                       5, 5, 1))
  }
  proj.gg <- proj.gg + ggplot2::scale_x_continuous(limits = horizontal.range) + 
    ggplot2::theme(axis.line = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), 
                   axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), legend.position = "bottom", 
                   panel.background = ggplot2::element_blank(), panel.border = ggplot2::element_blank(), 
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                   plot.background = ggplot2::element_blank()) + ggplot2::ggtitle(paste(project.id),subtitle=message) + ggplot2::theme(text = ggplot2::element_text(size = 20))
  proj.gg <- proj.gg + ggplot2::scale_color_manual(name = ggplot2::element_blank(), 
                                                   values = synccolors)
  
  return(proj.gg)    
  
}
