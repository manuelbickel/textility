

#TODO will be included in forthcoming functions of the package to plot networks with igraph...
# .onLoad <- function(libname, pkgname){
#   # add additional igraph shape
#   # a circle with variable frame border width (not available in the standard shapes)
#   # https://groups.google.com/forum/?hl=fil#!topic/network-analysis-with-igraph/r5c9bkjduCs
#   # a copy of the above link is also available at
#   # https://stackoverflow.com/questions/48725525/change-line-type-of-vertex-border
#   mycircle <- function(coords, v=NULL, params) {
#     vertex.color <- params("vertex", "color")
#     if (length(vertex.color) != 1 && !is.null(v)) {
#       vertex.color <- vertex.color[v]
#     }
#     vertex.size  <- 1/200 * params("vertex", "size")
#     if (length(vertex.size) != 1 && !is.null(v)) {
#       vertex.size <- vertex.size[v]
#     }
#     vertex.frame.color <- params("vertex", "frame.color")
#     if (length(vertex.frame.color) != 1 && !is.null(v)) {
#       vertex.frame.color <- vertex.frame.color[v]
#     }
#     vertex.frame.width <- params("vertex", "frame.width")
#     if (length(vertex.frame.width) != 1 && !is.null(v)) {
#       vertex.frame.width <- vertex.frame.width[v]
#     }
#
#     mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
#            vertex.size, vertex.frame.width,
#            FUN=function(x, y, bg, fg, size, lwd) {
#              symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
#                      circles=size, add=TRUE, inches=FALSE)
#            })
#   }
#
#   add.vertex.shape("circle2", clip = igraph.shape.noclip,
#                    plot=mycircle, parameters=list(vertex.frame.color=1,
#                                                   vertex.frame.width=1))
# }
