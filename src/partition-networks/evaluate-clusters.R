#############################################################################################
# Estimates the quality of the detected partitions, using various measures designed for
# community detection and for correlationc clustering.
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
#############################################################################################
record.partition.stats <- function(partition, folder)
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
		x.rotate=FALSE, format=c("PDF","PNG",NA))
}



#############################################################################################
# Loads the partition estimated by the external tools (their file format needs a specific
# conversion processing).
# 
# partition.file: name of the file containing the partition.
# algo.name: name of the concerned partitioning algorithm.
# returns: the corresponding partition as a membership vector.
#############################################################################################
load.corclu.partition <- function(partition.file, algo.name)
{	if(algo.name==CORCLU.ALGO.PILS)
	{	# open and read the file
		con <- file(file.name, "r")
		lines <- readLines(con)
		close(con)
	
		# process the file content
		i <- 4
		line <- lines[i]
		res <- list()
		while(line!="")
		{  # process current line
			#print(line)
			line <- strsplit(x=line, "[ ", fixed=TRUE)[[1]][2]
			line <- strsplit(x=line, " ]", fixed=TRUE)[[1]][1]
			nodes <- as.integer(strsplit(x=line," ", fixed=TRUE)[[1]]) + 1 # plus one because C++ starts counting from 0
			res[[length(res)+1]] <- nodes
			
			# process next line
			i <- i + 1
			line <- lines[i]      
		}
		
		# build the membership vector
		mx <- max(unlist(res))
		membership <- rep(NA,mx)
		for(i in 1:length(res))
		{  nodes <- res[[i]]
			membership[nodes] <- i 
		}
	}
	else
	{	#TODO for now, only pILS is treated, but other algorithms can go here
	}
	
	# record the partition using the internal format
	write.table(x=membership, file=partition.file, row.names=FALSE, col.names=FALSE)
	
	return(membership)
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
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.NEG] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.NEG]/ecount(g)
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.POS] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.POS]/ecount(g)
	perf.table[algo.name, CORCLU.MEAS.IMB.UNW.PROP.TOTAL] <- perf.table[CORCLU.MEAS.IMB.UNW.CNT.TOTAL]/ecount(g)
	# weighted proportions
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.NEG] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.NEG]/sum(E(g)$weight)
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.POS] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.POS]/sum(E(g)$weight)
	perf.table[algo.name, CORCLU.MEAS.IMB.WGT.PROP.TOTAL] <- perf.table[CORCLU.MEAS.IMB.WGT.CNT.TOTAL]/sum(E(g)$weight)
	
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
	
	#TODO possibly add other measures here
	
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
	
	#TODO possibly add other measures here
	
	return(perf.table)
}



#############################################################################################
# Evaluates the performances of the selected correlation clustering methods, using all 
# available measures. The results are recorded in a table.
#
# graphs: graphs to consider.
# subfolder: subfolder containing the networks and partitions.
# corclu.algos: correlation clustering algorithms to apply.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
evaluate.corclu.methods <- function(graphs, subfolder, corclu.algos, repetitions)
{	# init the list used to process the average
	if(repetitions>1)
		avg.list <- list()
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	cat("Processing iteration ",r,"/",repetitions,"\n",sep="")
		# setup iteration folder
		if(repetitions>1)
			r.subfolder <- paste(subfolder,r,"/",sep="")
		else
			r.subfolder <- folder
		
		# init the iteration performance table
		perf.table <- matrix(NA,nrow=length(CORCLU.ALGO.VALUES),ncol=length(PART.MEAS.VALUES))
		colnames(perf.table) <- PART.MEAS.VALUES
		rownames(perf.table) <- CORCLU.ALGO.VALUES
		
		# process measures
		for(algo.name in corclu.algos)
		{	# setup iteration folder
			if(repetitions>1)
				att.name <- paste(algo.name,'-',r,sep="")
			else
				att.name <- algo.name
			
			base.name <- paste(PARTITIONS.FOLDER,"/",r.subfolder,SIGNED.FILE,"-",algo.name,,sep="")
			partition.file <- paste(base.name,"-membership.txt",sep="")
			if(!file.exists(partition.file))
				cat("Partition file ",partition.file," not found\n",sep="")
			else
			{	# load partition
				partition <- load.corclu.partition(partition.file, algo.name)
				# record stats
				record.partition.stats(partition, base.name)
				# complete perf table
				perf.table <- process.comdet.measures(graphs$neg, graphs$pos, partition, algo.name, perf.table)
				perf.table <- process.corclu.measures(graphs$signed, partition, algo.name, perf.table)
			}
		}
		
		# record iteration table
		table.file <- paste(PARTITIONS.FOLDER,"/",r.subfolder,SIGNED.FILE,"-performances.csv",sep="")
		write.csv2(perf.table, file=table.file, row.names=TRUE)
		
		# update the list used to average
		if(repetitions>1)
			avg.list[[r]] <- perf.table
	}
	
	# record the average tables
	if(repetitions>1)
	{	tmp <- average.matrix.list(avg.list)
		table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,SIGNED.FILE,"-mean-performances.csv",sep="")
		write.csv2(tmp$avg, file=table.file, row.names=TRUE)
		table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,SIGNED.FILE,"-stdev-performances.csv",sep="")
		write.csv2(tmp$stev, file=table.file, row.names=TRUE)
	}
}



#############################################################################################
# Evaluates the performances of the selected community detection methods, using all 
# available measures. The results are recorded in a table.
#
# graphs: graphs to consider.
# subfolder: subfolder containing the networks and partitions.
# comdet.algos: community detection algorithms to apply.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
evaluate.comdet.methods <- function(graphs, subfolder, comdet.algos, repetitions)
{	# init the list used to process the average
	if(repetitions>1)
	{	neg.avg.list <- list()
		pos.avg.list <- list()
	}
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	cat("Processing iteration ",r,"/",repetitions,"\n",sep="")
		# setup iteration folder
		if(repetitions>1)
			r.subfolder <- paste(subfolder,r,"/",sep="")
		else
			r.subfolder <- subfolder
		
		# init the iteration performance tables
			# complementary negative subraph 
			neg.perf.table <- matrix(NA,nrow=length(COMDET.ALGO.VALUES),ncol=length(PART.MEAS.VALUES))
			colnames(neg.perf.table) <- PART.MEAS.VALUES
			rownames(neg.perf.table) <- COMDET.ALGO.VALUES
			# positive subgraph
			pos.perf.table <- matrix(NA,nrow=length(COMDET.ALGO.VALUES),ncol=length(PART.MEAS.VALUES))
			colnames(pos.perf.table) <- PART.MEAS.VALUES
			rownames(pos.perf.table) <- COMDET.ALGO.VALUES
		
		# process measures
		for(algo.name in comdet.algos)
		{	cat("Process algorithm ",algo.name,"\n",sep="")
			# setup iteration folder
			if(repetitions>1)
				att.name <- paste(algo.name,'-',r,sep="")
			else
				att.name <- algo.name
			
			# partition obtained on the complementary negative subgraph
			neg.base.name <- paste(PARTITIONS.FOLDER,"/",r.subfolder,COMP.NEGATIVE.FILE,"-",algo.name,sep="")
			neg.partition.file <- paste(neg.base.name,"-membership.txt",sep="")
			if(!file.exists(neg.partition.file))
				cat("Partition file ",neg.partition.file," not found\n",sep="")
			else
			{	cat("Process complementary negative partition\n",sep="")
				# load partition
				neg.partition <- as.matrix(read.table(neg.partition.file))
				# record stats
				record.partition.stats(neg.partition, neg.base.name)
				# complete perf table
				neg.perf.table <- process.comdet.measures(graphs$neg, graphs$pos, neg.partition, algo.name, neg.perf.table)
				neg.perf.table <- process.corclu.measures(graphs$signed, neg.partition, algo.name, neg.perf.table)
			}
			
			# partition obtained on the positive subgraph
			pos.base.name <- paste(PARTITIONS.FOLDER,"/",r.subfolder,POSITIVE.FILE,"-",algo.name,sep="")
			pos.partition.file <- paste(pos.base.name,"-membership.txt",sep="")
			if(!file.exists(pos.partition.file))
				cat("Partition file ",pos.partition.file," not found\n",sep="")
			else
			{	cat("Process positive partition\n",sep="")
				# load partition
				pos.partition <- as.matrix(read.table(pos.partition.file))
				# record stats
				record.partition.stats(pos.partition, pos.base.name)
				# complete perf table
				pos.perf.table <- process.modularity(graphs$neg, graphs$pos, pos.partition, algo.name, pos.perf.table)
				pos.perf.table <- process.corclu.measures(graphs$signed, pos.partition, algo.name, pos.perf.table)
			}
		}
		# record iteration tables
			# partition obtained on the complementary negative subraph 
			table.file <- paste(PARTITIONS.FOLDER,"/",r.subfolder,COMP.NEGATIVE.FILE,"-performances.csv",sep="")
			write.csv2(neg.perf.table, file=table.file, row.names=TRUE)
			# partition obtained on the postive subraph 
			table.file <- paste(PARTITIONS.FOLDER,"/",r.subfolder,POSITIVE.FILE,"-performances.csv",sep="")
			write.csv2(pos.perf.table, file=table.file, row.names=TRUE)
			
		# update the list used to average
		if(repetitions>1)
		{	neg.avg.list[[r]] <- neg.perf.table
			pos.avg.list[[r]] <- pos.perf.table
		}
	}
	
	# record the average tables
	if(repetitions>1)
	{	# partition obtained on the complementary negative subraph 
			tmp <- average.matrix.list(neg.avg.list)
			table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,"-mean-performances.csv",sep="")
			write.csv2(tmp$avg, file=table.file, row.names=TRUE)
			table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,COMP.NEGATIVE.FILE,"-stdev-performances.csv",sep="")
			write.csv2(tmp$stev, file=table.file, row.names=TRUE)
		# partition obtained on the postive subraph 
			tmp <- average.matrix.list(pos.avg.list)
			table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,POSITIVE.FILE,"-mean-performances.csv",sep="")
			write.csv2(tmp$avg, file=table.file, row.names=TRUE)
			table.file <- paste(PARTITIONS.FOLDER,"/",subfolder,POSITIVE.FILE,"-stdev-performances.csv",sep="")
			write.csv2(tmp$stev, file=table.file, row.names=TRUE)
	}
}



#############################################################################################
# Evaluates the partitions for the specified partitioning algorithms, for all possible networks, 
# for all time periods and domains, for the specified thresholds and agreement scores. 
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
# repetitions: number of times each algorithm must be applied.
#############################################################################################
evaluate.partitions <- function(neg.thresh=NA, pos.thresh=NA, score.file, subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Process performance measures for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph subfolder
			folder <- paste(subfolder,"/",score.file,"/",dom,"/",DATE.STR.T7[date],
					"/","negtr=",neg.thresh,"-postr=",pos.thresh,"/",sep="")
			
			# load all three versions of the graph
			cat("Load all three versions of the graph\n",sep="")
			graphs <- retrieve.graphs(folder)
			# evaluate community detection methods
			cat("Evaluate community detection methods\n",sep="")
			evaluate.comdet.methods(graphs, folder, comdet.algos, repetitions)
			# evaluate correlation clustering methods
			cat("Evaluate correlation clustering methods\n",sep="")
			evaluate.corclu.methods(graphs, folder, corclu.algos, repetitions)
		}
	}
}


#############################################################################################
# Evaluates the partitions for the specified partitioning algorithms, for all networks, for 
# the whole dataset, by country and by political group, for the specified thresholds and 
# agreement scores. 
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
# repetitions: number of times each algorithm must be applied.
#############################################################################################
evaluate.all.partitions <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, repetitions)
{	# extract networks for all data
	if(everything)
	{	cat("Process performance measures for all data","\n",sep="")
		subfolder <- "everything"
		evaluate.partitions(neg.thresh, pos.thresh, score.file, subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
	
	# networks by political group
	cat("Process performance measures by group","\n",sep="")
	subfolder <- "bygroup"
	for(group in groups)
	{	cat("Process performance measures for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# setup folder
		grp.subfolder <- paste(subfolder,"/",group,sep="")
		
		# extract networks
		evaluate.partitions(neg.thresh, pos.thresh, score.file, grp.subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
	
	# networks by home country
	cat("Process performance measures by country","\n",sep="")
	subfolder <- "bycountry"
	for(country in countries)
	{	cat("Process performance measures for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# setup folder
		cntr.subfolder <- paste(subfolder,"/",country,sep="")
		
		# extract networks
		evaluate.partitions(neg.thresh, pos.thresh, score.file, cntr.subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
}
