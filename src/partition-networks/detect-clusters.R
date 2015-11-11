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
COMDET.FUNCTIONS <- list()
COMDET.EDGEBETW <- "EB"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.EDGEBETW)
	COMDET.NAMES[COMDET.EDGEBETW] <- "EdgeBetweenness"
	COMDET.FUNCTIONS[[COMDET.EDGEBETW]] <- function(g) edge.betweenness.community(
		graph=g, edge.betweenness=FALSE, merges=FALSE,
		bridges=FALSE, modularity=TRUE, membership=TRUE)
COMDET.INFOMAP <- "IM"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.INFOMAP)
	COMDET.NAMES[COMDET.INFOMAP] <- "InfoMap"
	COMDET.FUNCTIONS[[COMDET.INFOMAP]] <- function(g) infomap.community(
		graph=g, modularity = TRUE)
COMDET.LABELPROP <- "LP"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.LABELPROP)
	COMDET.NAMES[COMDET.LABELPROP] <- "LabelPropagation"
	COMDET.FUNCTIONS[[COMDET.LABELPROP]] <- function(g) label.propagation.community(
		graph=g, 
		initial=NULL, fixed=NULL)
COMDET.LOUVAIN <- "LV"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.LOUVAIN)
	COMDET.NAMES[COMDET.LOUVAIN] <- "Louvain"
	COMDET.FUNCTIONS[[COMDET.LOUVAIN]] <- function(g) multilevel.community (
		graph=g, weights = NULL)
COMDET.WALKTRAP <- "WT"
	COMDET.VALUES <- c(COMDET.VALUES, COMDET.WALKTRAP)
	COMDET.NAMES[COMDET.WALKTRAP] <- "WalkTrap"
	COMDET.FUNCTIONS[[COMDET.WALKTRAP]] <- function(g) walktrap.community(
		graph, steps=4, 
		merges=FALSE, modularity=TRUE, membership=TRUE)
				
				

#############################################################################################
# Detects the community structure of the specified network, using the specified algorithm,
# and record the results as tables and plots. The graph Graphml file is also updated by
# defining nodal attributes corresponding to the detected communities. 
#
# g: graph to process.
# algo.name: (normalized) name of the community detection algorithm.
# net.path: path of the file corresponding to the processed network.
# part.folder: folder in which to write the result files.
# modularity: existing table, to be completed depending on the obtained modularity.
# returns: the updated modularity table.
#############################################################################################
apply.community.detection.algorithm <- function(g, algo.name, net.path, part.folder, modularity)
{	# apply the community detection algorithm
	coms <- COMDET.FUNCTIONS[[algo.name]](g)
	
	# record the membership vector
	data <- membership(coms)
	table.file <- paste(part.folder,"-",algo.name,"-membership.txt",sep="")
	write.table(x=data, file=table.file, row.names=FALSE, col.names=FALSE)
	# update graph (graphml only) with detected communities
	g <- set.vertex.attribute(graph=g, name=algo.name, value=data)
	write.graph(graph=g, file=net.path, format="graphml")
	
	# update the modularity table
	data <- modularity(coms)
	modularity[algo.name] <- data

	# record the community sizes
	data <- sizes(coms)
	table.file <- paste(part.folder,"-",algo.name,"-comsizes.txt",sep="")
	write.table(x=data, file=table.file, row.names=FALSE, col.names=FALSE)
	# plot them
	table.file <- paste(part.folder,"-",algo.name,"-comsizes",sep="")
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
# folder: subfolder containing the graph files, and used to create the result folder.
#############################################################################################
perform.community.detection <- function(subfolder)
{	# load both graphs
	net.folder.pos <- paste(NETWORKS.FOLDER,"/",subfolder,POSTIVE.FILE,sep="")
	graph.file.pos <- paste(net.folder.pos,".graphml",sep="")
	g.pos <- NA
	if(!file.exists(graph.file.pos))
		cat("Graph file ",graph.file.pos," not found >> no community detection for this one\n",sep="")
	else
		g.pos <- read.graph(file=graph.file.pos, format="graphml")
	net.folder.neg <- paste(NETWORKS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,sep="")
	graph.file.neg <- paste(net.folder.neg,".graphml",sep="")
	g.neg <- NA
	if(!file.exists(graph.file.neg))
		cat("Graph file ",graph.file.neg," not found >> no community detection for this one\n",sep="")
	else
		g.neg <- read.graph(file=graph.file.neg, format="graphml")
	
	# init modularity matrices
	part.folder.pos <- paste(PARTITIONS.FOLDER,"/",subfolder,POSTIVE.FILE,sep="")
	mat.pos <- matrix(NA,nrow=length(COMDET.VALUES),ncol=1)
	rownames(mat.pos) <- COMDET.VALUES
	mat.file.pos <- paste(part.folder.pos,"-modularity.txt",sep="")
	part.folder.neg <- paste(PARTITIONS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,sep="")
	mat.neg <- matrix(NA,nrow=length(COMDET.VALUES),ncol=1)
	rownames(mat.neg) <- COMDET.VALUES
	mat.file.neg <- paste(part.folder.neg,"-modularity.txt",sep="")
	
	# apply all community detection algorithms
	for(algo.name in COMDET.VALUES)
	{	# positive graph
		if(!all(is.na(g.pos)))
		{	cat("Applying ",COMDET.NAMES[algo.name]," to the positive graph\n",sep="")
			mat.pos <- apply.community.detection.algorithm(g.pos, algo.name, graph.file.pos, part.folder.pos, mat.pos)
			write.table(x=mat.pos, file=mat.file.pos, row.names=TRUE, col.names=FALSE)
		}
		
		# complementary negative graph
		if(!all(is.na(g.neg)))
		{	cat("Applying ",COMDET.NAMES[algo.name]," to the complementary negative graph\n",sep="")
			mat.neg <- apply.community.detection.algorithm(g.neg, algo.name, graph.file.neg, part.folder.neg, mat.neg)
			write.table(x=mat.neg, file=mat.file.neg, row.names=TRUE, col.names=FALSE)
		}
	}
}



#############################################################################################
# Generate all possible networks for all time periods and domains, for the specified thresholds 
# and agreement scores. 
#
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# subfolder: subfolder used to store the generated files.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
#############################################################################################
detect.communities <- function(neg.thresh=NA, pos.thresh=NA, score.file, subfolder, domains, dates)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Detect communities for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph folder
			folder <- paste(subfolder,"/",score.file,"/",dom,"/",DATE.STR.T7[date],
					"/","negtr=",neg.thresh,"-postr=",pos.thresh,"/",sep="")
			dir.create(folder, recursive=TRUE, showWarnings=FALSE)
			
			# perform community detection
			perform.community.detection(folder)
		}
	}
}


#############################################################################################
# Generate all networks for the whole dataset, by country and by political group, for the 
# specified thresholds and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
#############################################################################################
detect.all.communities <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups)
{	# extract networks for all data
	if(everything)
	{	cat("Detect communities for all data","\n",sep="")
		subfolder <- "everything"
		detect.communities(neg.thresh, pos.thresh, score.file, subfolder, domains, dates)
	}
	
	# networks by political group
	cat("Detect communities by group","\n",sep="")
	subfolder <- "bygroup"
	for(group in groups)
	{	cat("Detect communities for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# setup folder
		grp.subfolder <- paste(subfolder,"/",group,sep="")
		
		# extract networks
		detect.communities(neg.thresh, pos.thresh, score.file, grp.subfolder, domains, dates)
	}
	
	# networks by home country
	cat("Detect communities by country","\n",sep="")
	subfolder <- "bycountry"
	for(country in countries)
	{	cat("Detect communities for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# setup folder
		cntr.subfolder <- paste(subfolder,"/",country,sep="")
		
		# extract networks
		detect.communities(neg.thresh, pos.thresh, score.file, cntr.subfolder, domains, dates)
	}
}
