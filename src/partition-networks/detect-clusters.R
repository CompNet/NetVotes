#############################################################################################
# Functions to apply various community detection methods on the positive and complementary 
# negative networks, and various correlation clustering methods to the signed networks.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/partition-networks/networks-common.R")
source("src/partition-networks/load-membership.R")

				

#############################################################################################
# Partitions the specified network, using the specified algorithm, and record the result
# as a table.  
#
# g: graph to process.
# algo.name: (normalized) name of the community detection algorithm.
# part.folder: folder in which to write the result (partition) files.
# graph.folder: folder of the processed network (for external tools).
# returns: membership vector, i.e. cluster/community number for each node.
# plot.formats: formats of the plot files.
#############################################################################################
apply.partitioning.algorithm <- function(g, algo.name, part.folder, graph.folder, plot.formats)
{	#tlog("n=",vcount(g), " m=",ecount(g), " d=",graph.density(g), sep="")
	#tlog(" connected=", is.connected(g,mode="weak"), sep="")
	#tlog("\n", sep="")
	
	# apply the community detection algorithm
	coms <- NA
	mbrshp <- NA
	if(algo.name==COMDET.ALGO.EDGEBETW | algo.name==comdet.algo.ncg.value(COMDET.ALGO.EDGEBETW))
	{	# this implementation will use the weights and directions, if present
		coms <- edge.betweenness.community(
					graph=g, edge.betweenness=FALSE, merges=FALSE,
					bridges=FALSE, modularity=TRUE, membership=TRUE) # modularity needed (bug in igraph v0.7)
	}
	else if(algo.name==COMDET.ALGO.INFOMAP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.INFOMAP))
	{	# this implementation will use the weights and directions, if present
		coms <- infomap.community(
					graph=g, modularity=FALSE)
	}
	else if(algo.name==COMDET.ALGO.LABELPROP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.LABELPROP))
	{	# this implementation will use the weights and directions, if present
		coms <- label.propagation.community(
					graph=g, 
					initial=NULL, fixed=NULL)
	}
	else if(algo.name==COMDET.ALGO.LOUVAIN | algo.name==comdet.algo.ncg.value(COMDET.ALGO.LOUVAIN))
	{	# this implementation will use the weights, if present, but cannot use directions
		g2 <- as.undirected(g, mode="collapse")
		coms <- multilevel.community(
					graph=g, weights=NULL)
	}
	else if(algo.name==COMDET.ALGO.WALKTRAP | algo.name==comdet.algo.ncg.value(COMDET.ALGO.WALKTRAP))
	{	# this implementation will use the weights, if present, and ignores directions
		coms <- walktrap.community(
					graph=g, steps=4, 
					merges=TRUE, modularity=TRUE, membership=TRUE)
	}
	
	# apply the correlation clustering algorithm
	else if(algo.name==CORCLU.ALGO.PILS)
	{	# get the path of the input file (graph)
		net.file <- file.path(net.folder,SIGNED.FILE)
		# external invocation (pILS is coded in C++)
			# TODO add the external invocation of the application
		# load the resulting partition file
		part.file <- file.path(part.folder,"cc-result.txt") # TODO possibly necessary to fix the external file name
		mbrshp <- load.external.partition(part.folder, algo.name)
	}
	
	# record the result
	if(all(is.na(coms)))
		tlog("............WARNING: Problem while applying partitioning algorithm ",algo.name," on folder ",part.folder,"\n")
	else
	{	if(is.na(mbrshp))
			mbrshp <- membership(coms)
		while(min(mbrshp)==0)
			mbrshp <- mbrshp + 1
		# record the membership vector
		table.file <- file.path(part.folder,paste(algo.name,"-membership.txt",sep=""))
		write.table(x=mbrshp, file=table.file, row.names=FALSE, col.names=FALSE)
		# record a graphical representation of the detected partition
		idx <- regexpr(" -",g$name)[1]
		g$name <- paste(PART.ALGO.NAMES[algo.name],substring(g$name,idx,nchar(g$name)),sep="")
		plot.file <- file.path(part.folder,paste(algo.name,"-membership",sep=""))
		plot.network(g, membership=mbrshp, plot.file, format=plot.formats)
	}
	
	return(mbrshp)
}



#############################################################################################
# Loads all three graphs (signed, positive and complementary negative) and applies all 
# community detection and correlation clustering algorithms. Then, records the results as text 
# files and record a new graph file (Graphml format only) containing nodal attributes corresponding 
# to the detected communities, for each considered algorithm.
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domain: political domain currently processed.
# date: time period currently processed.
# country: state member currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied.
# plot.formats: formats of the plot files.
#############################################################################################
perform.partitioning <- function(thresh, score.file, domain, date, country, group, comdet.algos, corclu.algos, repetitions, plot.formats)
{	# load the graphs
	graphs <- retrieve.graphs(score=score.file, thresh, country, group, domain, period=date, comp=TRUE)
	graph.folder <- get.networks.path(score=score.file, thresh, country, group, domain, period=date)
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	tlog("........Processing iteration ",r,"/",repetitions,"\n",sep="")
		# setup iteration folder
		#folder <- paste(PARTITIONS.FOLDER,"/",subfolder,sep="")
		#r.folder <- paste(folder,r,"/",sep="")
		#dir.create(r.folder, recursive=TRUE, showWarnings=FALSE)
		if(repetitions>1)
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=r)
		else
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=NA)
		dir.create(part.folder, recursive=TRUE, showWarnings=FALSE)
		
		# apply all community detection algorithms
		for(algo.name in comdet.algos)
		{	neg.algo.name <- comdet.algo.ncg.value(algo.name)
			
			# setup attribute name
			if(repetitions>1)
			{	pos.att.name <- paste(algo.name,'-',r,sep="")
				neg.att.name <- paste(neg.algo.name,'-',r,sep="")
			}
			else
			{	pos.att.name <- algo.name
				neg.att.name <- neg.algo.name
			}
			
			# complementary negative graph
			if(!all(is.na(graphs$neg)))
			{	tlog("..........Applying ",COMDET.ALGO.NAMES[algo.name]," to the complementary negative graph\n",sep="")
				memb <- apply.partitioning.algorithm(graphs$neg, neg.algo.name, part.folder, graph.folder, plot.formats)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=neg.att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=neg.att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=neg.att.name, value=memb)
			}
			
			# positive graph
			if(!all(is.na(graphs$pos)))
			{	tlog("..........Applying ",COMDET.ALGO.NAMES[algo.name]," to the positive graph\n",sep="")
				memb <- apply.partitioning.algorithm(graphs$pos, algo.name, part.folder, graph.folder, plot.formats)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=pos.att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=pos.att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=pos.att.name, value=memb)
			}
		}
		
		# apply all correlation clustering algorithms
		for(algo.name in corclu.algos)
		{	if(!all(is.na(graphs$signed)))
			{	tlog("..........Applying ",COMDET.ALGO.NAMES[algo.name]," to the signed graph\n",sep="")
				memb <- apply.partitioning.algorithm(graphs$signed, algo.name, part.folder, graph.folder, plot.formats)
				graphs$neg <- set.vertex.attribute(graph=graphs$neg, name=att.name, value=memb)
				graphs$pos <- set.vertex.attribute(graph=graphs$pos, name=att.name, value=memb)
				graphs$signed <- set.vertex.attribute(graph=graphs$signed, name=att.name, value=memb)
			}
		}
	}

	# record graphs (Graphml only) with detected communities, in the partition folder (not the network one)
	part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=,date, repetition=NA)
	if(!all(is.na(graphs$neg)))
	{	graph.file.neg <- file.path(part.folder,paste(COMP.NEGATIVE.FILE,".graphml",sep=""))
		write.graph(graph=graphs$neg, file=graph.file.neg, format="graphml")
	}
	if(!all(is.na(graphs$pos)))
	{	graph.file.pos <- file.path(part.folder,paste(POSITIVE.FILE,".graphml",sep=""))
		write.graph(graph=graphs$pos, file=graph.file.pos, format="graphml")
	}
	if(!all(is.na(graphs$signed)))
	{	graph.file <- file.path(part.folder,paste(SIGNED.FILE,".graphml",sep=""))
		write.graph(graph=graphs$signed, file=graph.file, format="graphml")
	}
}



#############################################################################################
# Applies the selected partitioning algorithms for all time periods and domains, for the specified 
# thresholds and agreement scores. 
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: state member currently processed (or NA if none in particular).
# group: political gorup currently processed (or NA if none in particular).
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied.
# plot.formats: formats of the plot files.
#############################################################################################
partition.graphs <- function(thresh=NA, score.file, domains, dates, country, group, comdet.algos, corclu.algos, repetitions, plot.formats)
{	# consider each domain individually (including all domains at once)
#	for(dom in domains)
	foreach(dom=domains) %dopar%
	{	source("src/define-imports.R")
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	tlog("......Detect communities for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph subfolder
			#folder <- paste(subfolder,"/",score.file,
			#		"/","negtr=",thresh[1],"-postr=",thresh[2],
			#		"/",dom,"/",DATE.STR.T7[date],"/",sep="")
			
			# perform community detection
			perform.partitioning(thresh, score.file, dom, date, country, group, comdet.algos, corclu.algos, repetitions, plot.formats)
		}
	}
}


#############################################################################################
# Applies all selected partitioning algorithms, for the whole dataset, by country and by political 
# group, for the specified thresholds and agreement scores. 
#
# mep.details: description of each MEP.
# thresh: thresholds used for network extraction (vector of two values).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied (to assess the stability of the results).
# plot.formats: formats of the plot files.
#############################################################################################
partition.all.graphs <- function(mep.details, thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, repetitions, plot.formats)
{	tlog("***************************************************\n")
	tlog("****** PARTITIONING NETWORKS\n")
	tlog("***************************************************\n")
	
	# networks by political group
	tlog("..Detect communities by group","\n",sep="")
	for(group in groups)
	{	tlog("....Detect communities for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# extract networks
		partition.graphs(thresh, score.file, domains, dates, country=NA, group, comdet.algos, corclu.algos, repetitions, plot.formats)
	}
	
	# networks by home country
	tlog("..Detect communities by country","\n",sep="")
	for(country in countries)
	{	tlog("....Detect communities for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# extract networks
		partition.graphs(thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats)
	}

	# extract networks for all data
	if(everything)
	{	tlog("..Detect communities for all data","\n",sep="")
		partition.graphs(thresh, score.file, domains, dates, country=NA, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats)
	}
}
