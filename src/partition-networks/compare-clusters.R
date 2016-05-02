#############################################################################################
# Set of function in charge for comparing previously detected partitions.
# 
# 07/2015 Israel Mendonça (v1)
# 11/2015 Vincent Labatut (v2)
#############################################################################################
source("src/partition-networks/networks-common.R")


#############################################################################################
# Processes all the specified measures comparing both partitions.
#
# partition1: first partition.
# partition2: second partition.
# measures: vector of measure names: c("vi", "nmi", "split.join", "rand", "adjusted.rand").
# returns: vector of values corresponding to each specified measures.
#############################################################################################
compare.partition.pair <- function(partition1, partition2, measures="nmi")
{	# init result vector
	result <- rep(NA,length(measures))
	names(result) <- measures
	
	# process measures for specified partitions
	for(measure in measures)
		result[measure] <- compare(partition1, partition2, method=measure)
	
	# TODO one can add the processing of other measures here if needed
	
	return(result)
}


#############################################################################################
# Processes the specified measures comparing all specified partitioning algorithms for the
# data contained in the specified folder.
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# comdet.algos: vector of community detection algorithm names.
# corclu.algos: vector of correlation clustering algorithm names.
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.partitions.measures <- function(thresh=NA, score.file, domain, date, country, group, comdet.algos, corclu.algos, measures="nmi", repetitions)
{	# init the list used to process the average
	if(repetitions>1)
	{	avg.list <- list()
		for(meas in measures)
			avg.list[[meas]] <- list()
	}
	
	# the process might be repeated several times
	for(r in 1:repetitions)
	{	cat("Processing iteration ",r,"/",repetitions,"\n",sep="")
		# setup iteration folder
		if(repetitions>1)
			#r.folder <- paste(folder,r,"/",sep="")
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=r)
		else
			#r.folder <- folder
			part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=NA)
		
		# load partitions
		partitions <- list()
		for(corclu.name in corclu.algos)
		{	cat("Loading partition files for algorithm ",corclu.name,"\n",sep="")
			partition.file <- file.path(part.folder,paste(corclu.name,"-membership.txt",sep=""))
			if(!file.exists(partition.file))
				cat("Partition file ",partition.file," not found\n",sep="")
			else
				partitions[[corclu.name]] <- as.matrix(read.table(partition.file))
		}
		for(comdet.name in comdet.algos)
		{	cat("Loading partition files for algorithm ",comdet.name,"\n",sep="")
			partition.file <- file.path(part.folder,paste(comdet.name,"-membership.txt",sep=""))
			if(!file.exists(partition.file))
				cat("Partition file ",partition.file," not found\n",sep="")
			else
				partitions[[comdet.name]] <- as.matrix(read.table(partition.file))
		}
		for(comdet.name in comdet.algos)
		{	neg.algo.name <- comdet.algo.ncg.value(comdet.name)
			cat("Loading partition files for algorithm ",neg.algo.name,"\n",sep="")
			partition.file <- file.path(part.folder,paste(neg.algo.name,"-membership.txt",sep=""))
			if(!file.exists(partition.file))
				cat("Partition file ",partition.file," not found\n",sep="")
			else
				partitions[[neg.algo.name]] <- as.matrix(read.table(partition.file))
		}
		
		# init iteration matrices
		mats <- list() 
		for(meas in measures)
		{	m <- matrix(NA, nrow=length(partitions), ncol=length(partitions))
			rownames(m) <- names(partitions)
			colnames(m) <- names(partitions)
			mats[[meas]] <- m
		}
		
		# compare partitions
		for(i in 1:(length(partitions)-1))
		{	#print(i);print(length(partitions))
			partition1 <- partitions[[i]]
			for(j in (i+1):length(partitions))
			{	cat("Processing ",names(partitions)[i]," vs ",names(partitions)[j],"\n",sep="")
				partition2 <- partitions[[j]]
				vals <- compare.partition.pair(partition1, partition2, measures)
				#print(vals)
				if(any(is.nan(vals)))
					cat("WARNING: some measures returned NaN, which will appear as NA in the recorded file","\n",sep="")
				for(meas in measures)
				{	mats[[meas]][i,j] <- vals[meas]
					mats[[meas]][j,i] <- vals[meas]
				}
			}
		}
		
		# record iteration matrices
		for(meas in measures)
		{	table.file <- file.path(part.folder,paste("comparison-",meas,".csv",sep=""))
			write.csv2(mats[[meas]], file=table.file, row.names=TRUE)
		}
		
		# update the list used to average
		if(repetitions>1)
		{	for(meas in measures)
				avg.list[[meas]][[r]] <- mats[[meas]]
		}
	}
	
	# record the average tables
	if(repetitions>1)
	{	part.folder <- get.partitions.path(score=score.file, thresh, country, group, domain, period=date, repetition=NA)
		for(meas in measures)
		{	tmp <- average.matrix.list(avg.list[[meas]])
			table.file <- file.path(part.folder,paste("comparison-mean-",meas,".csv",sep=""))
			write.csv2(tmp$avg, file=table.file, row.names=TRUE)
			table.file <- file.path(part.folder,paste("comparison-stdev-",meas,".csv",sep=""))
			write.csv2(tmp$stdev, file=table.file, row.names=TRUE)
		}
	}
}


#############################################################################################
# Compare the partitions for the specified partitioning algorithms, for all possible networks, 
# for all time periods and domains, for the specified thresholds and agreement scores. 
#
# thresh: thresholds used for network extraction (vector of two values).
# score.file: file describing the scores to use when processing the inter-MEP agreement
#			  (without the .txt extension).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# country: member state to consider separately when processing the data.
# group: political group to consider separately when processing the data.
# comdet.algos: community detection algorithms to apply.
# corclu.algos: correlation clustering algorithms to apply.
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.partitions <- function(thresh=NA, score.file, domains, dates, country, group, comdet.algos, corclu.algos, measures, repetitions)
{	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Process performance measures for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# setup graph folder
			#filtered.folder <- paste(folder,"/",score.file,
			#		"/","negtr=",thresh[1],"-postr=",thresh[2],
			#		"/",dom,"/",DATE.STR.T7[date],
			#		"/",sep="")
			
			# compare algorithm performances
			cat("Compare partitioning algorithm performances\n",sep="")
			compare.partitions.measures(thresh, score.file, domain=dom, date, country, group, comdet.algos, corclu.algos, measures, repetitions)
		}
	}
}


#############################################################################################
# Compares the partitions for the specified partitioning algorithms, for all networks, for 
# the whole dataset, by country and by political group, for the specified thresholds and 
# agreement scores. 
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
# measures: vector of comparison measures to process.
# repetitions: number of times each algorithm must be applied.
#############################################################################################
compare.all.partitions <- function(mep.details, thresh=NA, score.file, domains, dates, everything, countries, groups, comdet.algos, corclu.algos, measures, repetitions)
{	# extract networks for all data
	if(everything)
	{	cat("Compare performance measures for all data","\n",sep="")
		compare.partitions(thresh, score.file, domains, dates, country=NA, group=NA, comdet.algos, corclu.algos, measures, repetitions)
	}
	
	# networks by political group
	cat("Compare performance measures by group","\n",sep="")
	for(group in groups)
	{	cat("Compare performance measures for group ",group,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.group(mep.details,group)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		grp.meps <- mep.details[idx,]
		
		# extract networks
		compare.partitions(thresh, score.file, domains, dates, country=NA, group, comdet.algos, corclu.algos, measures, repetitions)
	}
	
	# networks by home country
	cat("Compare performance measures by country","\n",sep="")
	for(country in countries)
	{	cat("Compare performance measures for country ",country,"\n",sep="")
		
		# select data
		filtered.mep.ids <- filter.meps.by.country(mep.details,country)
		idx <- match(filtered.mep.ids,mep.details[,COL.MEPID])
		cntr.meps <- mep.details[idx,]
		
		# extract networks
		compare.partitions(thresh, score.file, domains, dates, country, group=NA, comdet.algos, corclu.algos, measures, repetitions)
	}
}
