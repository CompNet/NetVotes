#############################################################################################
# Estimates the quality of the detected partitions, using various measures designed for
# community detection and for correlation clustering.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/partition-networks/networks-common.R")
source("src/plot-tools/plot-bars.R")



#############################################################################################
# Records the size of the clusters from the specified partition, and plot them as a barplot.
#
# partition: membership vector.
# folder: start of the file name used when recording values and plots.
# plot.formats: formats of the plot files.
#############################################################################################
record.partition.stats <- function(partition, folder, plot.formats)
{	# record the community sizes
	comsz <- table(partition)
	coms <- rownames(comsz)
	comsz <- as.vector(comsz)
	table.file <- paste(folder,"-comsizes.txt",sep="")
	write.table(x=comsz, file=table.file, row.names=FALSE, col.names=FALSE)
	
	# plot them
	plot.file <- paste(folder,"-comsizes",sep="")
	plot.unif.indiv.count.bars(plot.file, bar.names=coms, 
		counts=comsz, dispersion=NA, proportions=FALSE, areas=FALSE, 
		y.lim=c(0,NA), 
		x.label="Community", y.label="Count", 
		plot.title="Community sizes", 
		x.rotate=FALSE, format=plot.formats)
}



#############################################################################################
# Processes a bunch of versions of the structural imbalance for the specified graph and partition.
#
# g: graph to consider.
# partition: integer vector representing the partition to consider.
# algo.name: name of the concerned partitioning algorithm.
# perf.table: the previously created table of performances.
# returns: the performance table completed with the imbalance values. 
#############################################################################################
process.structural.imbalance <- function(g, partition, algo.name, perf.table)
{	# compare the clusters of connected nodes
	edge.mat <- get.edgelist(g)
	clus.mat <- cbind(partition[edge.mat[,1]],partition[edge.mat[,2]])
	same.clus <- clus.mat[,1]==clus.mat[,2]
	
	# compare link signs and positions 
	neg.links <- E(g)$weight<0
	pos.links <- E(g)$weight>0
	neg.misplaced <- same.clus & neg.links
	pos.misplaced <- !same.clus & pos.links
	all.misplaced <- neg.misplaced | pos.misplaced
	
	# unweighted counts
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.CNT.NEG] <- length(E(g)[neg.misplaced])
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.CNT.POS] <- length(E(g)[pos.misplaced])
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.CNT.TOTAL] <- length(E(g)[all.misplaced])
	# weighted counts
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.CNT.NEG] <- sum(E(g)$weight[neg.misplaced])
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.CNT.POS] <- sum(E(g)$weight[pos.misplaced])
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.CNT.TOTAL] <- sum(E(g)$weight[all.misplaced])
	# unweighted proportions
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.NEG] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.NEG]/length(E(g)$weight[E(g)$weight<0])
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.POS] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.POS]/length(E(g)$weight[E(g)$weight>=0])
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.TOTAL] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.TOTAL]/ecount(g)
	# weighted proportions
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.NEG] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.NEG]/sum(abs(E(g)$weight[E(g)$weight<0]))
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.POS] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.POS]/sum(abs(E(g)$weight[E(g)$weight>=0]))
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.TOTAL] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.TOTAL]/sum(abs(E(g)$weight))
	
	return(perf.table)
}


#############################################################################################
# Processes all available performance measures for signed networks.
#
# g: graph to consider.
# partition: integer vector representing the partition to consider.
# algo.name: name of the concerned partitioning algorithm.
# perf.table: the previously created table of performances.
# returns: the updated result table containing the measured performance values. 
#############################################################################################
process.corclu.measures <- function(g, partition, algo.name, perf.table)
{	# process imbalance variants
	perf.table <- process.structural.imbalance(g, partition, algo.name, perf.table)
	
	# TODO possibly add other measures here
	
	return(perf.table)
}



#############################################################################################
# Processes several versions of the modularity for the specified graphs and partition.
#
# g.neg: complementary negative subgraph to consider.
# g.pos: positive subgraph to consider.
# partition: integer vector representing the partition to consider.
# algo.name: name of the concerned partitioning algorithm.
# perf.table: the previously created table of performances.
# returns: the performance table completed with the modularity values. 
#############################################################################################
process.modularity <- function(g.neg, g.pos, partition, algo.name, perf.table)
{	# evaluation on the complementary negative graph
	if(!all(is.na(g.neg)))
	{	perf.table[algo.name, COMDET.MEAS.MOD.UNW.NEG] <- modularity(x=g.neg, membership=partition, weights=NULL)
		perf.table[algo.name, COMDET.MEAS.MOD.WGT.NEG] <- modularity(x=g.neg, membership=partition, weights=E(g.neg)$weight)
	}
	
	# evaluation on the positive graph
	if(!all(is.na(g.pos)))
	{	perf.table[algo.name, COMDET.MEAS.MOD.UNW.POS] <- modularity(x=g.pos, membership=partition, weights=NULL)
		perf.table[algo.name, COMDET.MEAS.MOD.WGT.POS] <- modularity(x=g.pos, membership=partition, weights=E(g.neg)$weight)
	}
	
	return(perf.table)
}



#############################################################################################
# Processes all available performance measures for unsigned networks.
#
# g.neg: complementary negative subgraph to consider.
# g.pos: positive subgraph to consider.
# partition: integer vector representing the partition to consider.
# algo.name: name of the concerned partitioning algorithm.
# perf.table: the previously created table of performances.
# returns: the updated result table containing the measured performance values. 
#############################################################################################
process.comdet.measures <- function(g.neg, g.pos, partition, algo.name, perf.table)
{	# process modularity
	perf.table <- process.modularity(g.neg, g.pos, partition, algo.name, perf.table)
	
	# TODO possibly add other measures here
	
	return(perf.table)
}



#############################################################################################
# This methods receives a list of matrix. Each element of the list is a matrix corresponding
# to an iteration. In this matrix, the rows correspond to partitioning algorithms and the
# columns to performance measures.
# 
# The function generates a series of plots, including two types:
# - Algorithm-specific performance plots: performance obtained for one, detailed and for each 
#	iteration, for a specific measure.
# - Average performance plots: average performance obtained by each algo over all iterations, 
#	for a specific measure.
# 
# g: considered (signed) graph.
# perf.list: list containing all the (previously processed) performance values.
# avg.vals: list of two matrices, one whose elements correspond to averages processed over
# 			the "perf.list" list (element-wise), and the other which is similar but for
#			standard deviations (as processed by function "average.matrix.list").
# folder: folder containing the partitions.
# plot.formats: formats of the plot files.
#############################################################################################
plot.partition.perf <- function(g, perf.list, avg.vals, folder, plot.formats)
{	# init
	algos <- rownames(perf.list[[1]])
	measures <- colnames(perf.list[[1]])
	measure.groups <- list(
		wIc=c(CORCLU.MEAS.IMB.UNW.CNT.NEG,CORCLU.MEAS.IMB.UNW.CNT.POS,CORCLU.MEAS.IMB.UNW.CNT.TOTAL),
		uIc=c(CORCLU.MEAS.IMB.WGT.CNT.NEG,CORCLU.MEAS.IMB.WGT.CNT.POS,CORCLU.MEAS.IMB.WGT.CNT.TOTAL),
		wIp=c(CORCLU.MEAS.IMB.UNW.PROP.NEG,CORCLU.MEAS.IMB.UNW.PROP.POS,CORCLU.MEAS.IMB.UNW.PROP.TOTAL),
		uIp=c(CORCLU.MEAS.IMB.WGT.PROP.NEG,CORCLU.MEAS.IMB.WGT.PROP.POS,CORCLU.MEAS.IMB.WGT.PROP.TOTAL),
		uM=c(COMDET.MEAS.MOD.UNW.NEG,COMDET.MEAS.MOD.UNW.POS),
		wM=c(COMDET.MEAS.MOD.WGT.NEG,COMDET.MEAS.MOD.WGT.POS)
	)
	measure.group.names <- list(
		wIc="Weighted Imbalance (counts)",
		uIc="Unweighted Imbalance (counts)",
		wIp="Weighted Imbalance (prop.)",
		uIp="Unweighted Imbalance (counts)",
		uM="Weighted Modularity",
		wM="Unweighted Modularity"
	)
	
	# process each measure separately
	for(measure in measures)
	{	cat("Plotting measure ",COMDET.MEAS.NAMES[measure],"\n")
		
		# if there was several repetitions
		if(length(perf.list)>1)
		{	# generate the algo-specific perf plots (one bar for each repetition)
			for(algo in algos)
			{	data <- sapply(perf.list, function(m) m[algo,measure])
				plot.file <- file.path(folder,paste(measure,"-",algo,"-performances",sep=""))
				plot.unif.indiv.count.bars(plot.file, bar.names=1:length(perf.list), 
					counts=data, dispersion=NA, proportions=FALSE, areas=FALSE, 
					y.lim=PART.MEAS.BOUNDS[[measure]](g), 
					x.label="Repetition", y.label=PART.MEAS.NAMES[measure], 
					plot.title=paste(PART.MEAS.NAMES[measure]," for each repetition"), 
					x.rotate=FALSE, format=plot.formats)
			}
			
			# generate the average perf plot (one bar for each algo)
			plot.file <- file.path(measure,paste(folder,"-mean-performances",sep=""))
			plot.unif.indiv.count.bars(plot.file, bar.names=PART.ALGO.NAMES[algos], 
				counts=avg.vals[[1]][,measure], dispersion=avg.vals[[2]][,measure], proportions=FALSE, areas=FALSE, 
				y.lim=PART.MEAS.BOUNDS[[measure]](g), 
				x.label="Algorithm", y.label=PART.MEAS.NAMES[measure], 
				plot.title=paste("Average ",PART.MEAS.NAMES[measure]," value by partitioning algorithm"), 
				x.rotate=TRUE, format=plot.formats)
		}
		
		# if there was only a single repetition
		else
		{ 	plot.file <- file.path(folder,paste(measure,"-single-performances",sep=""))
			plot.unif.indiv.count.bars(plot.file, bar.names=PART.ALGO.NAMES[algos], 
				counts=perf.list[[1]][,measure], dispersion=NA, proportions=FALSE, areas=FALSE, 
				y.lim=PART.MEAS.BOUNDS[[measure]](g), 
				x.label="Algorithm", y.label=PART.MEAS.NAMES[measure], 
				plot.title=paste(PART.MEAS.NAMES[measure]," value by partitioning algorithm"), 
				x.rotate=TRUE, format=plot.formats)
		}
	}
	
	# compare imbalance values for all algos
	for(i in 1:length(measure.groups))
	{	measures <- measure.groups[[i]]
		bounds <- sapply(measures, function(measure) PART.MEAS.BOUNDS[[measure]](g))
		plot.file <- file.path(folder,paste("grouped-",names(measure.groups)[i],"-performances",sep=""))
		if(length(perf.list)>1)
		{	data.m <- lapply(algos, function(a) avg.vals[[1]][a,measures])
			data.sd <- lapply(algos, function(a) avg.vals[[2]][a,measures])
			plot.unif.grouped.count.bars(plot.file, group.names=PART.ALGO.NAMES[algos], bar.names=PART.MEAS.NAMES[measures],
				counts=data.m, dispersion=data.sd, proportions=FALSE,
				y.lim=c(min(bounds),max(bounds)),
				x.label="Algorithm", y.label="Performance", 
				plot.title=paste("Average ", measure.group.names[[i]]," values for all algorithms",sep=""), 
				x.rotate=TRUE, format=plot.formats)
		}
		else
		{	data <- lapply(algos, function(a) perf.list[[1]][a,measures])
			plot.unif.grouped.count.bars(plot.file, group.names=PART.ALGO.NAMES[algos], bar.names=PART.MEAS.NAMES[measures],
				counts=data, dispersion=NA, proportions=FALSE,
				y.lim=c(min(bounds),max(bounds)),
				x.label="Algorithm", y.label="Performance", 
				plot.title=paste(measure.group.names[[i]]," values for all algorithms",sep=""), 
				x.rotate=TRUE, format=plot.formats)
		}
	}
	
	# TODO possibly add other types of comparison plots
}



#############################################################################################
# Evaluates the performances of the selected correlation clustering method, using all 
# available measures. The function updates the specified table and records some partition stats.
#
# graphs: graphs to consider.
# part.folder: folder containing the partitions (and recorded files).
# algo.name: correlation clustering algorithm to treat.
# perf.table: table containing all the performances, to be updated.
# plot.formats: formats of the plot files.
#############################################################################################
evaluate.corclu.method <- function(graphs, part.folder, algo.name, perf.table, plot.formats)
{	# process all measures
	base.name <- file.path(part.folder,algo.name)
	partition.file <- paste(base.name,"-membership.txt",sep="")
	if(!file.exists(partition.file))
		cat("WARNING: Partition file '",partition.file,"' not found\n",sep="")
	else
	{	# load partition
		partition <- as.matrix(read.table(partition.file))
		
		# record stats
		record.partition.stats(partition, base.name, plot.formats)
		
		# complete perf table
		perf.table <- process.comdet.measures(graphs$neg, graphs$pos, partition, algo.name, perf.table)
		perf.table <- process.corclu.measures(graphs$signed, partition, algo.name, perf.table)
	}
}



#############################################################################################
# Evaluates the performances of the selected community detection method, using all 
# available measures. The function updates the specified table and records some partition stats.
#
# graphs: graphs to consider.
# part.folder: folder containing the partitions (and recorded files).
# algo.name: community detection algorithm to treat.
# perf.table: table containing all the performances, to be updated.
# plot.formats: formats of the plot files.
#############################################################################################
evaluate.comdet.method <- function(graphs, part.folder, algo.name, perf.table, plot.formats)
{	# partition obtained on the complementary negative subgraph
	neg.algo.name <- comdet.algo.ncg.value(algo.name)
	neg.base.name <- file.path(part.folder,neg.algo.name)
	neg.partition.file <- paste(neg.base.name,"-membership.txt",sep="")
	if(!file.exists(neg.partition.file))
		cat("WARNING: Partition file '",neg.partition.file,"' not found\n",sep="")
	else
	{	cat("Process complementary negative partition\n",sep="")
		# load partition
		neg.partition <- as.matrix(read.table(neg.partition.file))
		# record stats
		record.partition.stats(neg.partition, neg.base.name, plot.formats)
		# complete perf table
		perf.table <- process.comdet.measures(graphs$neg, graphs$pos, neg.partition, neg.algo.name, perf.table)
		perf.table <- process.corclu.measures(graphs$signed, neg.partition, neg.algo.name, perf.table)
	}
	
	# partition obtained on the positive subgraph
	pos.base.name <- file.path(part.folder,algo.name)
	pos.partition.file <- paste(pos.base.name,"-membership.txt",sep="")
	if(!file.exists(pos.partition.file))
		cat("WARNING: Partition file '",pos.partition.file,"' not found\n",sep="")
	else
	{	cat("Process positive partition\n",sep="")
		# load partition
		pos.partition <- as.matrix(read.table(pos.partition.file))
		# record stats
		record.partition.stats(pos.partition, pos.base.name, plot.formats)
		# complete perf table
		perf.table <- process.modularity(graphs$neg, graphs$pos, pos.partition, algo.name, perf.table)
		perf.table <- process.corclu.measures(graphs$signed, pos.partition, algo.name, perf.table)
	}
}



#############################################################################################
# Evaluates the partitions for the specified partitioning algorithms, for all possible networks, 
# for all time periods and domains, for the specified thresholds and agreement scores. 
#
# neg.thresh: agreement negative threshold used during cluster detection.
# pos.thresh: agreement positive threshold used during cluster detection.
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# comdet.algos: community detection algorithms to treat.
# corclu.algos: correlation clustering algorithms to treat.
# repetitions: number of times each algorithm has been applied.
# plot.formats: formats of the plot files.
#############################################################################################
evaluate.partitions <- function(neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, country, group, comdet.algos, corclu.algos, repetitions, plot.formats)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	#dom.folder <- paste(subfolder,"/",score.file,"/",
		#		"negtr=",neg.thresh,"-postr=",pos.thresh,"/",
		#		dom,"/",sep="") #TODO the thresholds should be top-level parameters
		date.perf.list <- list()
		
		# consider each time period (each individual year as well as the whole term)
		for(d in 1:length(dates))
		{	date <- dates[d]
			cat("Process performance measures for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph subfolder
			#date.folder <- paste(dom.folder,"/",DATE.STR.T7[date],"/",sep="")
			#
			# load all three versions of the graph
			cat("Load all three versions of the graph\n",sep="")
			graphs <- retrieve.graphs(score=score.file, neg.thresh, pos.thresh, country, group, domain=dom, period=date)
			
			# init the list used to process the average and plots
			if(repetitions>1)
				perf.list <- list()
			
			# merge the vectors of algos
			algo.names <- c(comdet.algos, corclu.algos)
			
			# the process might be repeated several times
			for(r in 1:repetitions)
			{	cat("Processing iteration ",r,"/",repetitions,"\n",sep="")
				# setup iteration folder
				if(repetitions>1)
					#r.subfolder <- paste(date.folder,r,"/",sep="")
					part.folder <- get.partitions.path(score=score.file, neg.thresh, pos.thresh, country, group, domain=dom, period=date, repetition=r)
				else
					#r.subfolder <- date.folder
					part.folder <- get.partitions.path(score=score.file, neg.thresh, pos.thresh, country, group, domain=dom, period=date, repetition=NA)
				
				# init the iteration performance table
				perf.table <- matrix(NA,nrow=2*length(comdet.algos)+length(corclu.algos),ncol=length(PART.MEAS.VALUES))
				rownames(perf.table) <- c(comdet.algos,comdet.algo.ncg.value(comdet.algos),corclu.algos)
				colnames(perf.table) <- PART.MEAS.VALUES
				
				# process measures
				for(algo.name in algo.names)
				{	cat("Process algorithm ",algo.name,"\n",sep="")
					
					# community detection method (must treat both positive and complementary negative graphs)
					if(algo.name %in% COMDET.ALGO.VALUES)
						perf.table <- evaluate.comdet.method(graphs, part.folder, algo.name, perf.table, plot.formats)
					
					# correlation clustering method (must treat only the signed graph)
					else
						perf.table <- evaluate.corclu.method(graphs, part.folder, algo.name, perf.table, plot.formats)
				}
				
				# record iteration table
				table.file <- file.path(part.folder,"performances.csv")
				write.csv2(perf.table, file=table.file, 
					row.names=c(COMDET.ALGO.NAMES[comdet.algos],comdet.algo.ncg.name(COMDET.ALGO.NAMES[comdet.algos]),CORCLU.ALGO.NAMES[corclu.algos]),
					col.names=PART.MEAS.NAMES[col.names(perf.table)])
				
				# update the list used to process average and plots
				if(repetitions>1)
					perf.list[[r]] <- perf.table
			}
			
			# record the average (over repetitions) tables and generate the plots
			if(repetitions>1)
			{	part.folder <- get.partitions.path(score=score.file, neg.thresh, pos.thresh, country, group, domain=dom, period=date, repetition=NA)
				
				# record average table
				avg.vals <- average.matrix.list(perf.list)
				table.file <- file.path(part.folder,"mean-performances.csv")
				write.csv2(avg.vals$avg, file=table.file, row.names=TRUE)
				table.file <- file.path(part.folder,"stdev-performances.csv")
				write.csv2(avg.vals$stev, file=table.file, row.names=TRUE)
				
				# plot all of this
				plot.partition.perf(graphs$signed, perf.list, avg.vals, subfolder=part.folder, plot.formats)
				
				date.perf.list[[d]] <- avg.vals
			}
			else
				date.perf.list[[d]] <- perf.list[[1]]
		}
		
		# generate overall tables and plots for each measure independently
		for(measure in PART.MEAS.VALUES)
		{	# build tables representing how each algorithm behaved on each considered period
			data.m <- matrix(NA,nrow=2*length(comdet.algos)+length(corclu.algos),ncol=length(dates))
			rownames(data.m) <- c(comdet.algos,comdet.algo.ncg.value(comdet.algos),corclu.algos)
			colnames(data.m) <- dates
			data.sd <- matrix(NA,nrow=2*length(comdet.algos)+length(corclu.algos),ncol=length(dates))
			for(d in 1:length(dates))
			{	if(repetitions>1)
				{	data.m[,dates[d]] <- date.perf.list[[d]][[1]][,measure]
					data.sd[,dates[d]] <- date.perf.list[[d]][[2]][,measure]
				}
				else
				{	data.m[,dates[d]] <- date.perf.list[[d]][,measure]
				}
			}
			# record these table(s)
			prefix <- paste(PARTITIONS.FOLDER,"/",dom.folder,sep="")
			part.folder <- get.partitions.path(score=score.file, neg.thresh, pos.thresh, country, group, domain=dom, period=NA, repetition=NA)
			if(repetitions>1)
			{	table.file <- file.path(part.folder,paste(measure,"-mean-performances.csv",sep=""))
				write.csv2(data.m, file=table.file, row.names=TRUE)
				table.file <- file.path(part.folder,paste(measure,"-stdev-performances.csv",sep=""))
				write.csv2(data.sd, file=table.file, row.names=TRUE)
			}
			else
			{	table.file <- file.path(part.folder,paste(measure,"-single-performances.csv",sep=""))
				write.csv2(data.m, file=table.file, row.names=TRUE)
			}
			
			# plot the table(s) as barplots whose bars represent periods and bar groups correspond to algorithms
			# (i.e. something quite similar to Israel's plot from the ENIC paper)
#			bounds <- sapply(measures, function(measure) PART.MEAS.BOUNDS[[measure]](g)) # TODO should actually process the actual bounds and use "y.lim=c(min(bounds),max(bounds))" later when calling the plot function
			if(repetitions>1)
			{	dm <- lapply(algos, function(a) data.m[[1]][a,])
				dsd <- lapply(algos, function(a) data.sd[[2]][a,])
				plot.file <- file.path(part.folder,paste(measure,"-mean-performances",sep=""))
				plot.unif.grouped.count.bars(plot.file, group.names=PART.ALGO.NAMES[algos], bar.names=DATE.STR.T7[dates],
					counts=dm, dispersion=dsd, proportions=FALSE,
					y.lim=PART.MEAS.BOUNDS[[measure]](NA),
					x.label=paste("Algorithm and time period",sep=""), y.label=PART.MEAS.NAMES[measure],
					plot.title=paste("Average ", PART.MEAS.NAMES[measure]," values for all algorithms and time periods",sep=""),
					x.rotate=TRUE, format=plot.formats)
			}
			else
			{	dm <- lapply(algos, function(a) data.m[[1]][a,])
				plot.file <- file.path(part.folder,paste(measure,"-single-performances",sep=""))
				plot.unif.grouped.count.bars(plot.file, group.names=PART.ALGO.NAMES[algos], bar.names=DATE.STR.T7[dates],
					counts=dm, dispersion=NA, proportions=FALSE,
					y.lim=PART.MEAS.BOUNDS[[measure]](NA),
					x.label=paste("Algorithm and time period",sep=""), y.label=PART.MEAS.NAMES[measure],
					plot.title=paste(PART.MEAS.NAMES[measure]," values for all algorithms and time periods",sep=""),
					x.rotate=TRUE, format=plot.formats)
			}
		}
	}
}


#############################################################################################
# Evaluates the partitions for the specified partitioning algorithms, for all networks, for 
# the whole dataset, by country and by political group, for the specified thresholds and 
# agreement scores. 
#
# mep.details: description of each MEP.
# neg.thresh: agreement negative threshold used during cluster detection.
# pos.thresh: agreement positive threshold used during cluster detection.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# comdet.algos: community detection algorithms to treat.
# corclu.algos: correlation clustering algorithms to treat.
# repetitions: number of times each algorithm has been applied.
# plot.formats: formats of the plot files.
#############################################################################################
evaluate.all.partitions <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, repetitions, plot.formats)
{	# process performance for all data
	if(everything)
	{	cat("Process performance measures for all data","\n",sep="")
		evaluate.partitions(neg.thresh, pos.thresh, score.file, domains, dates, country=NA, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats)
	}
	
	# process performance by political group
	cat("Process performance measures by group","\n",sep="")
	for(group in groups)
	{	cat("Process performance measures for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# process performance
		evaluate.partitions(neg.thresh, pos.thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats)
	}
	
	# process performance by home country
	cat("Process performance measures by country","\n",sep="")
	for(country in countries)
	{	cat("Process performance measures for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# process performance
		evaluate.partitions(neg.thresh, pos.thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, repetitions, plot.formats)
	}
}
