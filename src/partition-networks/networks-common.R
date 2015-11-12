#############################################################################################
# Functions used by several network-related scripts.
# 
# 11/2015 Vincent Labatut
#############################################################################################
library("igraph")



#############################################################################################
# Loads all three versions of the graphs contained in the specified folder: signed, positive
# and complementary negative.
#
# folder: the subfolder containing the graphs.
# returns: a list containing the three graphs (named signed, pos, neg).
#############################################################################################
retrieve.graphs <- function(subfolder)
{	# load the complementary negative graph
	graph.file.neg <- paste(NETWORKS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,".graphml",sep="")
	g.neg <- NA
	if(!file.exists(graph.file.neg))
		cat("Graph file ",graph.file.neg," not found\n",sep="")
	else
		g.neg <- suppressWarnings(read.graph(file=graph.file.neg, format="graphml"))
	
	# load the positive graph
	graph.file.pos <- paste(NETWORKS.FOLDER,"/",subfolder,POSITIVE.FILE,".graphml",sep="")
	g.pos <- NA
	if(!file.exists(graph.file.pos))
		cat("Graph file ",graph.file.pos," not found\n",sep="")
	else
		g.pos <- suppressWarnings(read.graph(file=graph.file.pos, format="graphml"))
	
	# load the original graph
	graph.file <- paste(NETWORKS.FOLDER,"/",subfolder,SIGNED.FILE,".graphml",sep="")
	g <- NA
	if(!file.exists(graph.file))
		cat("Graph file ",graph.file," not found\n",sep="")
	else
		g <- suppressWarnings(read.graph(file=graph.file, format="graphml"))
	
	# build and return the result list
	result <- list(neg=g.neg, pos=g.pos, signed=g)
	return(result)
}
