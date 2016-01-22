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
# perf.list: list containing all the previously processed performances.
# avg.vals: list of two matrices, one whose elements correspond to averages processed over
# 			the "perf.list" list (element-wise), and the other which is similar but for
#			standard deviations (as processed by function "average.matrix.list").
# subfolder: subfolder containing the networks and partitions.
#############################################################################################
plot.partition.perf <- function(perf.list, avg.vals, subfolder)
{	# init
	algos <- rownames(perf.list[[1]])
	measures <- colnames(perf.list[[1]])
	
	# process each measure separately
	for(measure in measures)
	{	# generate the algo-specific perf plots
		for(algo in algos)
		{	plot.file <- paste(subfolder,"-",measure,"-",algo,"-performances",sep="")
#TODO mettre à jour les noms de fichiers ici-même
#TODO penser à utiliser les bounds définies juste avant			
			
		}
		
		# generate the average perf plot
		plot.file <- paste(subfolder,"-mean-performances",sep="")
		plot.unif.indiv.count.bars(plot.file, bar.names=coms, 
			counts=avg.vals[[1]][algos,measure], dispersion=avg.vals[[2]][algos,measure], proportions=FALSE, areas=FALSE, 
			y.lim=c(0,NA), 
			x.label="Algorithm", y.label=measure, 
			plot.title=paste("Average ",measure," value by partitioning algorithm"), 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
	}
}



#############################################################################################
# Evaluates the performances of the selected correlation clustering method, using all 
# available measures. The function updates the specified table and records some partition stats.
#
# graphs: graphs to consider.
# subfolder: subfolder containing the partitions (and recorded files).
# algo.name: correlation clustering algorithm to treat.
# perf.table: table containing all the performances, to be updated.
#############################################################################################
evaluate.corclu.method <- function(graphs, subfolder, algo.name, perf.table)
{	# process all measures
	base.name <- paste(PARTITIONS.FOLDER,"/",subfolder,algo.name,sep="")
	partition.file <- paste(base.name,"-membership.txt",sep="")
	if(!file.exists(partition.file))
		cat("WARNING: Partition file '",partition.file,"' not found\n",sep="")
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



#############################################################################################
# Evaluates the performances of the selected community detection method, using all 
# available measures. The function updates the specified table and records some partition stats.
#
# graphs: graphs to consider.
# subfolder: subfolder containing the partitions (and recorded files).
# algo.name: community detection algorithm to treat.
# perf.table: table containing all the performances, to be updated.
#############################################################################################
evaluate.comdet.method <- function(graphs, subfolder, algo.name, perf.table)
{	# partition obtained on the complementary negative subgraph
	neg.algo.name <- comdet.algo.ncg.value(algo.name)
	neg.base.name <- paste(PARTITIONS.FOLDER,"/",subfolder,neg.algo.name,sep="")
	neg.partition.file <- paste(neg.base.name,"-membership.txt",sep="")
	if(!file.exists(neg.partition.file))
		cat("WARNING: Partition file '",neg.partition.file,"' not found\n",sep="")
	else
	{	cat("Process complementary negative partition\n",sep="")
		# load partition
		neg.partition <- as.matrix(read.table(neg.partition.file))
		# record stats
		record.partition.stats(neg.partition, neg.base.name)
		# complete perf table
		perf.table <- process.comdet.measures(graphs$neg, graphs$pos, neg.partition, neg.algo.name, perf.table)
		perf.table <- process.corclu.measures(graphs$signed, neg.partition, neg.algo.name, perf.table)
	}
	
	# partition obtained on the positive subgraph
	pos.base.name <- paste(PARTITIONS.FOLDER,"/",subfolder,algo.name,sep="")
	pos.partition.file <- paste(pos.base.name,"-membership.txt",sep="")
	if(!file.exists(pos.partition.file))
		cat("WARNING: Partition file '",pos.partition.file,"' not found\n",sep="")
	else
	{	cat("Process positive partition\n",sep="")
		# load partition
		pos.partition <- as.matrix(read.table(pos.partition.file))
		# record stats
		record.partition.stats(pos.partition, pos.base.name)
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
# subfolder: subfolder used to store the generated files.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# comdet.algos: community detection algorithms to treat.
# corclu.algos: correlation clustering algorithms to treat.
# repetitions: number of times each algorithm has been applied.
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
					r.subfolder <- paste(folder,r,"/",sep="")
				else
					r.subfolder <- folder
				
				# init the iteration performance table
				perf.table <- matrix(NA,nrow=2*length(comdet.algos)+length(corclu.algos),ncol=length(PART.MEAS.VALUES))
				rownames(perf.table) <- CORCLU.ALGO.VALUES
				colnames(perf.table) <- c(comdet.algos,comdet.algo.ncg.value(comdet.algos),corclu.algos)
				
				# process measures
				for(algo.name in algo.names)
				{	cat("Process algorithm ",algo.name,"\n",sep="")
					
					# community detection method (must treat both positive and complementary negative graphs)
					if(algo.name %in% COMDET.ALGO.VALUES)
						perf.table <- evaluate.comdet.method(graphs, r.subfolder, algo.name, perf.table)
					
					# correlation clustering method (must treat only the signed graph)
					else
						perf.table <- evaluate.corclu.method(graphs, r.subfolder, algo.name, perf.table)
				}
				
				# record iteration table
				table.file <- paste(PARTITIONS.FOLDER,"/",r.subfolder,"performances.csv",sep="")
				write.csv2(perf.table, file=table.file, 
						row.names=c(COMDET.ALGO.NAMES[comdet.algos],comdet.algo.ncg.name(COMDET.ALGO.NAMES[comdet.algos]),CORCLU.ALGO.NAMES[corclu.algos]),
						col.names=PART.MEAS.NAMES[col.names(perf.table)])
				
				# update the list used to process average and plots
				if(repetitions>1)
					perf.list[[r]] <- perf.table
			}
			
			# record the average (over repetitions) tables and generate the plots
			if(repetitions>1)
			{	prefix <- paste(PARTITIONS.FOLDER,"/",folder,sep="")
				
				# record average table
				avg.vals <- average.matrix.list(perf.list)
				table.file <- paste(prefix,"mean-performances.csv",sep="")
				write.csv2(avg.vals$avg, file=table.file, row.names=TRUE)
				table.file <- paste(prefix,"stdev-performances.csv",sep="")
				write.csv2(avg.vals$stev, file=table.file, row.names=TRUE)
				
				# plot all of this
				plot.partition.perf(perf.list, avg.vals, subfolder=prefix)
#TODO		
# - plotter perf par itération dans la fonction
# - renvoyer avg+stdev (ou simplement l'unique matrice de la liste)
# - plotter les perfs moyennes (ou pas) pour tous les algos traités pour une mesure donnée (dans la fonction appelante
# - essayer de reproduire le plot d'israel
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
#############################################################################################
evaluate.all.partitions <- function(mep.details, neg.thresh=NA, pos.thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, repetitions)
{	# process performance for all data
	if(everything)
	{	cat("Process performance measures for all data","\n",sep="")
		subfolder <- "everything"
		evaluate.partitions(neg.thresh, pos.thresh, score.file, subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
	
	# process performance by political group
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
		
		# process performance
		evaluate.partitions(neg.thresh, pos.thresh, score.file, grp.subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
	
	# process performance by home country
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
		
		# process performance
		evaluate.partitions(neg.thresh, pos.thresh, score.file, cntr.subfolder, domains, dates, comdet.algos, corclu.algos, repetitions)
	}
}
