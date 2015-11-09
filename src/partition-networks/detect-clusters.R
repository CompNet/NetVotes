#############################################################################################
# Functions to apply various community detection methods on the positive networks, and possibly
# the complementary negative graphs.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
library("igraph")

source("src/define-constants.R")
source("src/plot-tools/plot-bars.R")



#############################################################################################
# Algorithms info
#############################################################################################
COMDET.VALUES <- c()
COMDET.NAMES <- c()
COMDET.FUNCTIONS <- c()
COMDET.EDGEBETW <- "EB"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.EDGEBETW)
	COMDET.NAMES[COMDET.EDGEBETW] <- "EdgeBetweenness"
	COMDET.FUNCTIONS[COMDET.EDGEBETW] <- function(g) edge.betweenness.community(
		graph=g, edge.betweenness=FALSE, merges=FALSE,
		bridges=FALSE, modularity=TRUE, membership=TRUE)
COMDET.INFOMAP <- "IM"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.INFOMAP)
	COMDET.NAMES[COMDET.INFOMAP] <- "InfoMap"
	COMDET.FUNCTIONS[COMDET.INFOMAP] <- function(g) infomap.community(
		graph=g, modularity = TRUE)
COMDET.LABELPROP <- "LP"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.LABELPROP)
	COMDET.NAMES[COMDET.LABELPROP] <- "LabelPropagation"
	COMDET.FUNCTIONS[COMDET.LABELPROP] <- function(g) label.propagation.community(
		graph=g, 
		initial=NULL, fixed=NULL)
COMDET.LOUVAIN <- "LV"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.LOUVAIN)
	COMDET.NAMES[COMDET.LOUVAIN] <- "Louvain"
	COMDET.FUNCTIONS[COMDET.LOUVAIN] <- function(g) multilevel.community (
		graph=g, weights = NULL)
COMDET.WALKTRAP <- "WT"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.WALKTRAP)
	COMDET.NAMES[COMDET.WALKTRAP] <- "WalkTrap"
	COMDET.FUNCTIONS[COMDET.WALKTRAP] <- function(g) walktrap.community(
		graph, steps=4, 
		merges=FALSE, modularity=TRUE, membership=TRUE)
				
				

#############################################################################################
# Detects the community structure of the specified network, using the specified algorithm,
# and record the results as tables and plots.
#
# g: graph to process.
# algo.name: (normalized) name of the community detection algorithm.
# folder: folder in which to write the result files.
# modularity: existing table, to be completed depending on the obtained modularity.
# returns: the updated modularity table.
#############################################################################################
perform.community.detection <- function(g, algo.name, folder, modularity)
{	# apply the community detection algorithm
	coms <- COMDET.FUNCTIONS[algo.name](g)
	
	# record the membership vector
	data <- membership(coms)
	table.file <- paste(folder,"-",algo.name,"-membership.txt",sep="")
	write.table(x=data, file=table.file, row.names=FALSE, col.names=FALSE)
	
	# update the modularity table
	data <- modularity(coms)
	modularity[algo.name] <- data

	# record the community sizes
	data <- sizes(coms)
	table.file <- paste(folder,"-",algo.name,"-comsizes.txt",sep="")
	write.table(x=data, file=table.file, row.names=FALSE, col.names=FALSE)
	# plot them
	table.file <- paste(folder,"-",algo.name,"-comsizes",sep="")
	plot.unif.indiv.count.bars(plot.file, bar.names, 
		counts=data, dispersion=NA, proportions=FALSE, areas=FALSE, 
		y.lim=c(0,NA), 
		x.label="Community", y.label="Count", 
		plot.title="Community sizes", 
		x.rotate=FALSE, format=c("PDF","PNG",NA))
	
	return(modularity)
}



#############################################################################################
# Loads both graphs (positive and complementary negative) and applies all community detection
# algorithms. Then records the results as text files and plots.
#
# folder: folder containing the graph files, and used to create the result files.
#############################################################################################
detect.communities <- function(folder)
{	# load both graphs
	fp <- paste(folder,POSTIVE.FILE,sep="")
	graph.file <- paste(fp,".graphml",sep="")
	gp <- read.graph(file=graph.file, format="graphml")
	fn <- paste(folder,COMP.NEGATIVE.FILE,sep="")
	graph.file <- paste(fn,".graphml",sep="")
	gn <- read.graph(file=graph.file, format="graphml")
	
	# init modularity matrices
	mp <- matrix(NA,nrow=length(COMDET.VALUES),ncol=1)
	nrow(mp) <- COMDET.VALUES
	mfp <- paste(fp,"-modularity.txt",sep="")
	mn <- matrix(NA,nrow=length(COMDET.VALUES),ncol=1)
	nrow(mn) <- COMDET.VALUES
	mfn <- paste(fn,"-modularity.txt",sep="")
	
	# apply all community detection algorithms
	for(algo.name in COMDET.VALUES)
	{	# positive graph
		cat("Applying ",COMDET.NAMES[algo.name]," to the positive graph\n",sep="")
		mp <- perform.community.detectionfunction(gp, algo.name, fp, mp)
		write.table(x=mp, file=mfp, row.names=TRUE, col.names=FALSE)
		
		# complementary negative graph
		cat("Applying ",COMDET.NAMES[algo.name]," to the complementary negative graph\n",sep="")
		mn <- perform.community.detectionfunction(gn, algo.name, fn, mn)
		write.table(x=mn, file=mfn, row.names=TRUE, col.names=FALSE)
	}
}
