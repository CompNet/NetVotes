#############################################################################################
# Functions to apply various community detection methods on the positive and complementary 
# negative networks, and various correlation clustering methods to the signed networks.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/partition-networks/networks-common.R")
				
				

#############################################################################################
# Partitions the specified network, using the specified algorithm, and record the result
# as a table.  
#
# g: graph to process.
# algo.name: (normalized) name of the community detection algorithm.
# part.folder: folder in which to write the result files.
#############################################################################################
apply.partitioning.algorithm <- function(g, algo.name, part.folder)
{	#cat("n=",vcount(g), " m=",ecount(g), " d=",graph.density(g), sep="")
	#cat(" connected=", is.connected(g,mode="weak"), sep="")
	#cat("\n", sep="")
	
	# apply the community detection algorithm
	coms <- NA
	if(algo.name==COMDET.ALGO.EDGEBETW)
		coms <- edge.betweenness.community(
					graph=g, edge.betweenness=FALSE, merges=FALSE,
					bridges=FALSE, modularity=FALSE, membership=TRUE)
	else if(algo.name==COMDET.ALGO.INFOMAP)
		coms <- infomap.community(
					graph=g, modularity=FALSE)
	else if(algo.name==COMDET.ALGO.LABELPROP)
		coms <- label.propagation.community(
					graph=g, 
					initial=NULL, fixed=NULL)
	else if(algo.name==COMDET.ALGO.LOUVAIN)
		coms <- multilevel.community (
					graph=g, weights=NULL)
	else if(algo.name==COMDET.ALGO.WALKTRAP)
	{	coms <- walktrap.community(
					graph=g, steps=4, 
					merges=TRUE, modularity=TRUE, membership=TRUE)
	}
	else if(algo.name==CORCLU.ALGO.PILS)
	{	#TODO external invocation (pILS is coded in C++)
	}
		
	# record the membership vector
	if(!all(is.na(coms)))
	{	mbrshp <- membership(coms)
		while(min(mbrshp)==0)
			mbrshp <- mbrshp + 1
		table.file <- paste(part.folder,"-membership.txt",sep="")
		write.table(x=mbrshp, file=table.file, row.names=FALSE, col.names=FALSE)
	}
	
	return(mbrshp)
}



#############################################################################################
# Loads all three graphs (signed, positive and complementary negative) and applies all 
# community detection and correlation clustering algorithms. Then, records the results as text 
# files and updates the graph file (Graphml format only) by defining nodal attributes corresponding 
# to the detected communities, for each considered algorithm.
#
# subfolder: subfolder containing the graph files, and used to create the result folder.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
#############################################################################################
perform.partitioning <- function(subfolder, comdet.algos, corclu.algos)
{	# load the graphs
	graphs <- retrieve.graphs(subfolder)
	folder <- paste(PARTITIONS.FOLDER,"/",subfolder,sep="")
	dir.create(folder, recursive=TRUE, showWarnings=FALSE)
	
	# apply all community detection algorithms
	for(algo.name in comdet.algos)
	{	# complementary negative graph
		if(!all(is.na(graphs$neg)))
		{	cat("Applying ",COMDET.ALGO.NAMES[algo.name]," to the complementary negative graph\n",sep="")
			part.folder.neg <- paste(folder,COMP.NEGATIVE.FILE,"-",algo.name,sep="")
			memb.neg <- apply.partitioning.algorithm(graphs$neg, algo.name, part.folder.neg)
			graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=algo.name, value=memb.neg)
		}
		
		# positive graph
		if(!all(is.na(graphs$pos)))
		{	cat("Applying ",COMDET.ALGO.NAMES[algo.name]," to the positive graph\n",sep="")
			part.folder.pos <- paste(folder,POSITIVE.FILE,"-",algo.name,sep="")
			memb.pos <- apply.partitioning.algorithm(graphs$pos, algo.name, part.folder.pos)
			graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=algo.name, value=memb.pos)
		}
	}
	
	# apply all correlation clustering algorithms
	for(algo.name in corclu.algos)
	{	if(!all(is.na(graphs$signed)))
		{	cat("Applying ",COMDET.ALGO.NAMES[algo.name]," to the signed graph\n",sep="")
			part.folder.signed <- paste(folder,SIGNED.FILE,"-",algo.name,sep="")
			memb <- apply.partitioning.algorithm(graphs$signed, algo.name, part.folder.signed)
			graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=algo.name, value=memb)
		}
	}
	
	# update graph (graphml only) with detected communities
	graph.file.neg <- paste(NETWORKS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,".graphml",sep="")
	write.graph(graph=graphs$neg, file=graph.file.neg, format="graphml")
	graph.file.pos <- paste(NETWORKS.FOLDER,"/",subfolder,POSITIVE.FILE,".graphml",sep="")
	write.graph(graph=graphs$pos, file=graph.file.pos, format="graphml")
	graph.file <- paste(NETWORKS.FOLDER,"/",subfolder,SIGNED.FILE,".graphml",sep="")
	write.graph(graph=graphs$signed, file=graph.file, format="graphml")
}



#############################################################################################
# Applies the selected partitioning algorithms for all time periods and domains, for the specified 
# thresholds and agreement scores. 
#
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# subfolder: subfolder used to store the generated files.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
#############################################################################################
partition.graphs <- function(neg.thresh=NA, pos.thresh=NA, score.file, subfolder, domains, dates, comdet.algos, corclu.algos)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Detect communities for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph subfolder
			folder <- paste(subfolder,"/",score.file,"/",dom,"/",DATE.STR.T7[date],
					"/","negtr=",neg.thresh,"-postr=",pos.thresh,"/",sep="")
			
			# perform community detection
			perform.partitioning(folder, comdet.algos, corclu.algos)
		}
	}
}


#############################################################################################
# Applies all selected partitioning algorithms, for the whole dataset, by country and by political 
# group, for the specified thresholds and agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: negative agreement values above this threshold are set to zero (i.e. ignored).
# pos.thresh: positive agreement values below this threshold are set to zero (i.e. ignored).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
#############################################################################################
partition.all.graphs <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos)
{	# extract networks for all data
	if(everything)
	{	cat("Detect communities for all data","\n",sep="")
		subfolder <- "everything"
		partition.graphs(neg.thresh, pos.thresh, score.file, subfolder, domains, dates, comdet.algos, corclu.algos)
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
		partition.graphs(neg.thresh, pos.thresh, score.file, grp.subfolder, domains, dates, comdet.algos, corclu.algos)
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
		partition.graphs(neg.thresh, pos.thresh, score.file, cntr.subfolder, domains, dates, comdet.algos, corclu.algos)
	}
}
