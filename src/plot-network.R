# TODO: Add comment
# 
# Author: nejat
################################################################################


################################################################################
# Functions used to plot networks.
# 
# 11/2015 Vincent Labatut
################################################################################


library(package="igraph", lib.loc=LIBRARY.LOC.PATH)



################################################################################
# Plot the specified signed graph, generating files of the specified formats.
# WARNING: we encountered a problem with the way how we create node shapes through 'symboles' function in igraph.
#           So, 'shape.membership' should be always NA: IT IS DEPRECEATED.
#
# g: signed graph to plot.
# plot.file: base name (and path) of the generated files.
# format: format(s) to handle.
#
# returns: the same graph, with the spatial positions stored as nodal attributes x and y.
################################################################################
plot.network <- 
		function(g, membership=NA, plot.title=NA, shape.membership=NA, neg.imb.contributions, pos.imb.contributions, node.id.label.enabled)
{	
		    
	# ############################################################################
	# #### MISC ###### CUSTOM IGRAPH SHAPES
	# ############################################################################
	# 
	# miniSquare <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size <- 1/130 * params("vertex", "size")
	# 	#if (length(vertex.size) != 1 && !is.null(v)) {
	# 	#  vertex.size <- vertex.size[v]
	# 	#}
	# 	
	# 	symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
	# 			squares = 0.05,
	# 			#stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
	# 			add=TRUE, inches=FALSE)
	# }
	# # clips as a circle
	# add.vertex.shape("miniSquare", clip=igraph.shape.noclip,
	# 		plot=miniSquare)
	# 
	# 
	# 
	# ############################################################################
	# 
	# miniCircle <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size <- 1/130 * params("vertex", "size")
	# 	#if (length(vertex.size) != 1 && !is.null(v)) {
	# 	#  vertex.size <- vertex.size[v]
	# 	#}
	# 	
	# 	symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
	# 			circles = 0.025,
	# 			#stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
	# 			add=TRUE, inches=FALSE)
	# }
	# # clips as a circle
	# add.vertex.shape("miniCircle", clip=igraph.shape.noclip,
	# 		plot=miniCircle)
	# 
	# 
	# ############################################################################
	# 
	# mydiamond <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size <- 1/130 * params("vertex", "size")
	# 	#if (length(vertex.size) != 1 && !is.null(v)) {
	# 	#  vertex.size <- vertex.size[v]
	# 	#}
	# 	
	# 	#symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
	# 	symbols(x=coords[,1]+0.03, y=coords[,2]+0.03, bg=vertex.color,
	# 			stars=cbind(NA, NA, NA, vertex.size, vertex.size, vertex.size),
	# 			add=TRUE, inches=FALSE)
	# }
	# # clips as a circle
	# #add.vertex.shape("diamond", clip=vertex.shapes("rectangle")$clip, plot=mydiamond)
	# add.vertex.shape("diamond", clip=igraph.shape.noclip, plot=mydiamond)
	# 
	# 
	# ############################################################################
	# 
	# mytrianglePointDown <- function(coords, v=NULL, params) {
	# 
	# 	vertex.color <- params("vertex", "color")
	# 
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 
	# 	vertex.size <- 1/100 * params("vertex", "size")
	# 	#if (length(vertex.size) != 1 && !is.null(v)) {
	# 	#  vertex.size <- vertex.size[v]
	# 	#}
	# 	
	# 	symbols(x=( coords[,1]-0.05 ), y=coords[,2]+0.02, bg=vertex.color,
	# 			stars=cbind(vertex.size, NA,  NA, NA, NA, vertex.size),
	# 			#stars=cbind(vertex.size, vertex.size, vertex.size),
	# 			add=TRUE, inches=FALSE)
	# }
	# # clips as a circle
	# add.vertex.shape("triangleDown", clip=igraph.shape.noclip,
	# 		plot=mytrianglePointDown)
	# 
	# ##########################################################”
	# 
	# mytrianglePointUp <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size <- 1/100 * params("vertex", "size")
	# 	#if (length(vertex.size) != 1 && !is.null(v)) {
	# 	#  vertex.size <- vertex.size[v]
	# 	#}
	# 	
	# 	symbols(x=( coords[,1]+0.05 ), y=coords[,2]-0.02, bg=vertex.color,
	# 			stars=cbind(NA, NA, vertex.size, vertex.size, NA, NA),
	# 			#stars=cbind(vertex.size, vertex.size, vertex.size),
	# 			add=TRUE, inches=FALSE)
	# }
	# # clips as a circle
	# add.vertex.shape("triangleUp", clip=igraph.shape.noclip,
	# 		plot=mytrianglePointUp)
	# 
	# 
	# 
	# 
	# # generic square vertex shape, with a parameter for number of rays
	# mysquare <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size  <- 1/200 * params("vertex", "size")
	# 	if (length(vertex.size) != 1 && !is.null(v)) {
	# 		vertex.size <- vertex.size[v]
	# 	}
	# 	norays <- params("vertex", "norays")
	# 	if (length(norays) != 1 && !is.null(v)) {
	# 		norays <- norays[v]
	# 	}
	# 	
	# 	mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
	# 			FUN=function(x, y, bg, size, nor) {
	# 				symbols(x=x, y=y, bg=bg,
	# 						stars=matrix(c(size,size), nrow=1, ncol=nor*2),
	# 						add=TRUE, inches=FALSE)
	# 			})
	# }
	# # no clipping, edges will be below the vertices anyway
	# add.vertex.shape("mysquare", clip=igraph.shape.noclip,
	# 		plot=mysquare, parameters=list(vertex.norays=10))
	# 
	# 
	# 
	# # generic star vertex shape, with a parameter for number of rays
	# mystar <- function(coords, v=NULL, params) {
	# 	vertex.color <- params("vertex", "color")
	# 	if (length(vertex.color) != 1 && !is.null(v)) {
	# 		vertex.color <- vertex.color[v]
	# 	}
	# 	vertex.size  <- 1/200 * params("vertex", "size")
	# 	if (length(vertex.size) != 1 && !is.null(v)) {
	# 		vertex.size <- vertex.size[v]
	# 	}
	# 	norays <- params("vertex", "norays")
	# 	if (length(norays) != 1 && !is.null(v)) {
	# 		norays <- norays[v]
	# 	}
	# 	
	# 	mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
	# 			FUN=function(x, y, bg, size, nor) {
	# 				symbols(x=x, y=y, bg=bg,
	# 						stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
	# 						add=TRUE, inches=FALSE)
	# 			})
	# }
	# # no clipping, edges will be below the vertices anyway
	# add.vertex.shape("star", clip=igraph.shape.noclip,
	# 		plot=mystar, parameters=list(vertex.norays=5))
	# 
	# 
	# 
	# vshapes=
	# 		c(
	# 			"miniSquare",
	# 			"miniCircle", 
	# 			"square",
	# 			"triangleUp",
	# 			"triangleDown", 
	# 			"diamond",
	# 			"star",
	# 			"circle",
	# 			"sphere", 
	# 			"rectangle"
	# 		)
	# V(g)$shapes=vertex.shapes() 
	
	
	# pie.portions <- lapply(1:length(shape.membership), 
	#					function(x) rep(1,4)) #equal pie: %25,%25,%25,%25 
	
	############################################################################
	### Main code
	############################################################################
	COMPRESS <- FALSE # PDF icin
	
	
	# setup node parameters
	vertex.sizes <- 10
	vertex.labels = NA
	vertex.label.colors = "black"
	vertex.label.cexs = 1
	

	
	if(!all(is.na(neg.imb.contributions)) && !all(is.na(pos.imb.contributions))){
		# TODO: the use of labeling is a temporary solution. 
		# Use vertex.frame.color and vertex.frame.width instead
		vertex.labels <- rep(NA, vcount(g))
		vertex.label.colors = rep("black", vcount(g))
		indx=which(neg.imb.contributions > 0)
		vertex.labels[indx]="o"
		vertex.label.colors[indx] = "orange"
		indx=which(pos.imb.contributions > 0)	
		vertex.labels[indx]="o"
		vertex.label.colors[indx] = "SkyBlue2"
		
		
		# When there are 1 displayed graph, this config is not bad
		#vertex.label.cexs = 
		#	(neg.imb.contributions+pos.imb.contributions)*7 + 4
	
		# When there are 2 displayed graphs, this config is not bad
		vertex.label.cexs = 
				(neg.imb.contributions+pos.imb.contributions)*3 + 1.5
	} else if(node.id.label.enabled && !all(is.na(V(g)$MEPid))){ # aslinde ELSE IF yerine IF koymali. Ama su anlik boyle. TODO
		vertex.labels <- V(g)$MEPid
		vertex.label.cexs = 0.5
		vertex.label.colors = "black"
	}
	
	
	
	
	# setup link parameters
	if(ecount(g)>0)
	{	edge.colors <- rep(NA,ecount(g))
		edge.colors[E(g)$weight<0] <- adjustcolor("RED", alpha.f=0.5)
		edge.colors[E(g)$weight>0] <- adjustcolor("GREEN", alpha.f=0.5)
		edge.widths <- abs(E(g)$weight)*10
	}
	
	# set up node colors
	if(all(is.na(membership)))
		vertex.colors <- "SkyBlue2" # default igraph color
	else {	
		palette <- rainbow(length(unique(membership)))
		vertex.colors <- palette[membership]

		# =================================================================
		# make the isolated nodes white
		# =================================================================
		# if(any(degree(g) == 0)){ # if not an isolated node(s) (2 ve ya 3 tane isolated node ayni clusterda da bulunabilir)			
		# 	iso.index = which(degree(g) == 0)
		# 	vertex.colors[iso.index] = "white"
		# }
		iso.index = which(membership == CLU.NO.FOR.ALL.ISOLATED.NODES)
		if(length(iso.index)>0)
		    vertex.colors[iso.index] = "white"
		
	}
	
	
	# setup layout (only if the graph doesn't already have one)
	lyt = NA
	v.att <- list.vertex.attributes(graph=g)

	
	if(!("x" %in% v.att) | !("y" %in% v.att)) 
	{	
		gpos <- delete.edges(graph=g,edges=which(E(g)$weight<0))
#		lyt <- layout.kamada.kawai(graph=gpos)
		
		lyt <- layout.fruchterman.reingold(graph=gpos)
		
#		lyt <- layout.circle(graph=gpos)
		# store spatial positions as nodal attributes
		V(g)$x <- lyt[,1]
		V(g)$y <- lyt[,2]
	}
	else
		lyt <- cbind(V(g)$x,V(g)$y)
	
	
	## =======================================================================
	# TODO: maybe you can apply it for non-circular layout

	# http://stackoverflow.com/questions/38999656/increasing-spaces-between-vertices-for-r-igraph
	# Function to increase node separation (for explanatory details,
	# See this link for details: http://stackoverflow.com/a/28722680/496488
	layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
		g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
		#E(g)$weight <- 1
		
		attr <- cbind(id=1:vcount(g), val=wc)
		g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
		
#		l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
		l <- layout.fruchterman.reingold(g, weights=E(g)$weight)[1:vcount(graph),]
		return(l)
	}
	## =======================================================================
	

	
	# vertex.shapes = vshapes[rep(8, vcount(g))] # vshapes[8] = "circle"
	# if(!all(is.na(shape.membership)))
	#     vertex.shapes = vshapes[shape.membership]
	# 
	# 	indx = which(membership == CLU.NO.FOR.ALL.ISOLATED.NODES)
	# if(length(indx)>0)
	#     vertex.shapes[indx] = "sphere" # define an existing vertx shape for isolated nodes, otherwise there is an error
	
	
	# create the plot
	if(ecount(g)>0)
	{	
		return(
			plot(
				g,
				vertex.shape=NULL,
				vertex.shape=NULL, #vertex.shapes,
				layout=lyt,
				vertex.size=vertex.sizes,
				vertex.label=vertex.labels, 
				vertex.color=vertex.colors, 
				vertex.label.color=vertex.label.colors, 
				vertex.label.cex=vertex.label.cexs,
				edge.color=edge.colors, 
				edge.width=edge.widths
#				main=plot.title # BUNU KULLANMA, font size orantisiz cikiyo
			)
		)
	}
	else
	{	# TODO update plot options as the previous one
		return(
			plot(
				g, 
				layout=lyt,
				vertex.size=vertex.sizes, 
				vertex.label=vertex.label,
				vertex.color=vertex.colors
			)
		)
	}
	
}
