#############################################################################################
# Generates plots and data files for various statistics processed on the raw data.
# Some of them concern the votes, other concern the rebellion index.
# 
# 07/2015 Israel Mendonça (v1)
# 10/2015 Vincent Labatut (v2)
#############################################################################################
source("src/define-constants.R")
source("src/plot-tools/plot-bars.R")
source("src/plot-tools/plot-histos.R")
source("src/prepare-data/filter-data.R")



#############################################################################################
# Counts the occurrences of policy domains among the voted documents. Generates the corresponding
# plots: distribution for the whole term and for each year.
#
# doc.details: table describing the voted documents.
# plot.formats: formats of the plot files.
#############################################################################################
process.domain.frequencies <- function(doc.details, plot.formats)
{	cat("Processing the frequencies of documents by policy domain\n",sep="")
	
	# set folder up
	dir.create(DOMAINS.FOLDER, recursive=TRUE, showWarnings=FALSE)
	
	# term domain distribution
		# absolute counts as bars
		title <- paste("Numbers of documents by policy domain for the whole term",sep="")
		plot.file <- file.path(DOMAINS.FOLDER,"term-counts")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=DOMAIN.VALUES, 
			values=doc.details[,COL.DOMID],
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=plot.formats)
		# record as a table
		data <- data.frame(data)
		data <- cbind(data.frame(DOMAIN.VALUES),data)
		colnames(data) <- c(COL.DOMID, COL.COUNT)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv2(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Proportions of documents by policy domain - period=term",sep="")
		plot.file <- file.path(DOMAINS.FOLDER,"term-proportions")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=DOMAIN.VALUES, 
			values=doc.details[,COL.DOMID],
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=plot.formats)
		# record as a table
		data <- data / sum(data)
		data[is.na(data)] <- 0		# remove possible /0 outcomes
		data <- data.frame(data)
		data <- cbind(data.frame(DOMAIN.VALUES),data)
		colnames(data) <- c(COL.DOMID, COL.COUNT)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv2(data,file=table.file, row.names=FALSE)
	
	# process the list of documents by year
	docs <- list()
	for(date in DATE.T7.YEARS)
	{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
		
		# retain only the documents related to the selected dates
		docids <- filter.docs.by.date(doc.details, 
			start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]])
		idx <- match(docids,doc.details[,COL.DOCID])
		if(length(idx)>0)
			docs[[date]] <- doc.details[idx,COL.DOMID]
		else
			docs[[date]] <- NA
	}
	
	# yearly domain distribution
		# absolute counts as bars
		title <- paste("Numbers of documents by policy domain - period=yearly",sep="")
		plot.file <- file.path(DOMAINS.FOLDER,"yearly-counts")
		data <- plot.stacked.indiv.raw.bars(plot.file, 
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=DOMAIN.VALUES, 
			values=docs, 
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=plot.formats)
		# record as a table
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, DOMAIN.VALUES)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv2(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Proportions of documents by policy domain - period=yearly",sep="")
		plot.file <- file.path(DOMAINS.FOLDER,"yearly-proportions")
		data <- plot.stacked.indiv.raw.bars(plot.file, 
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=DOMAIN.VALUES, 
			values=docs, 
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=plot.formats)
		# record as a table
		data <- lapply(data, function(v) if(all(v==0)) {return(rep(0,length(v)))} else {return(v/sum(v))})
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, DOMAIN.VALUES)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv2(data,file=table.file, row.names=FALSE)
}


#############################################################################################
# Generate all plots and tables for the complete vote value distributions (FOR, AGAINST, etc.).
# This means a folder is created for each domain, and it contains a folder for each considered
# period (each year and the whole term). Four different plots and two table are generated, 
# corresponding to the distributions of vote values represented as bars vs. areas, for the
# absolute counts vs. proportions.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# vote.values: the vote values we want to include in the plots and tables.
# file.prefix: string to use in the file names.
# main.folder: folder in which to record the generated plots and tables.
# colors.label: title of the colors legend.
# object: counted object (votes, loyal votes, etc.).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
# plot.formats: formats of the plot files.
#############################################################################################
process.vote.distribution.complete <- function(all.votes, doc.details, vote.values, file.prefix, main.folder, colors.label, object, domains, dates, mode, plot.formats)
{	# setup file prefix
	if(is.na(file.prefix))
		file.prefix <- ""
	else
		file.prefix <- paste(file.prefix,"-",sep="")
	# setup title prefix
	if(is.na(mode))
		plot.prefix <- ""
	else
		plot.prefix <- paste("[",mode,"] ",sep="")
	
	# consider each time period (each individual year as well as the whole term)
	for(date in dates)
	{	
		# consider each domain individually (including all domains at once)
		for(dom in domains)
		{	cat("Processing ",file.prefix," data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.doc.ids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			idx <- match(filtered.doc.ids,doc.details[,COL.DOCID])
			if(length(idx)<=1)
				cat("WARNING: Only ",length(idx)," documents remaining after filtering >> not processing these data\n",sep="")
			else
			{	# order the votes by date
				filtered.doc.details <- doc.details[idx,]
				indices <- order(as.Date(filtered.doc.details[,COL.DATE],"%d/%m/%Y"))
				
				# get the corresponding votes from the complete table
				cols <- match(filtered.doc.ids, colnames(all.votes))
				cols <- cols[indices]
				# convert to list of column-vectors
				votes.temp <- all.votes[,cols]
				if(all(is.na(votes.temp)))
					cat("WARNING: All votes are NAs (this can be correct, not necessarily a problem) >> not processing these data\n",sep="")
				else
				{	votes <- split(votes.temp, rep(1:ncol(votes.temp), each=nrow(votes.temp)))
				
					# setup folder
					folder <- paste(main.folder,dom,"/",DATE.STR.T7[date],"/",sep="")
					dir.create(folder, recursive=TRUE, showWarnings=FALSE)
				
					# absolute counts as bars
					#print(votes)				
					title <- paste(plot.prefix,"Numbers of ",object," - domain=",dom," - period=",DATE.STR.T7[date],sep="")
					plot.file <- paste(folder,file.prefix,"counts-bars",sep="")
					data <- plot.stacked.indiv.raw.bars(plot.file, 
						bar.names=as.character(filtered.doc.ids[indices]), color.names=vote.values, 
						values=votes, 
						proportions=FALSE, areas=FALSE, y.lim=c(0,nrow(all.votes)), 
						x.label="Documents (sorted by date)", colors.label, plot.title=title, 
						x.rotate=TRUE, format=plot.formats)
					# absolute counts as areas
					plot.file <- paste(folder,file.prefix,"counts-areas",sep="")
					data <- plot.stacked.indiv.raw.bars(plot.file, 
						bar.names=1:length(filtered.doc.ids), color.names=vote.values, 
						values=votes, 
						proportions=FALSE, areas=TRUE, y.lim=c(0,nrow(all.votes)), 
						x.label="Documents (sorted by date)", colors.label, plot.title=title, 
						x.rotate=FALSE, format=plot.formats)
					# record as a table
					data <- t(data.frame(data))
					data <- cbind(data.frame(filtered.doc.ids[indices]),data)
					colnames(data) <- c(COL.DOCID, vote.values)
					table.file <- paste(folder,file.prefix,"counts",".csv",sep="")
					write.csv2(data,file=table.file, row.names=FALSE)
					
					# proportions as bars
					title <- paste(plot.prefix,"Proportions of ",object," - domain=",dom," - period=",DATE.STR.T7[date],sep="")
					plot.file <- paste(folder,file.prefix,"proportions-bars",sep="")
					data <- plot.stacked.indiv.raw.bars(plot.file, 
						bar.names=as.character(filtered.doc.ids[indices]), color.names=vote.values, 
						values=votes, 
						proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
						x.label="Documents (sorted by date)", colors.label, plot.title=title, 
						x.rotate=TRUE, format=plot.formats)
					# proportions as areas
					plot.file <- paste(folder,file.prefix,"proportions-areas",sep="")
					data <- plot.stacked.indiv.raw.bars(plot.file, 
						bar.names=1:length(filtered.doc.ids), color.names=vote.values, 
						values=votes, 
						proportions=TRUE, areas=TRUE, y.lim=c(0,NA), 
						x.label="Documents (sorted by date)", colors.label, plot.title=title, 
						x.rotate=FALSE, format=plot.formats)
					# record as a table
					data <- lapply(data, function(v) if(all(v==0)) {return(rep(0,length(v)))} else {return(v/sum(v))})
					data <- t(data.frame(data))
					data <- cbind(data.frame(filtered.doc.ids[indices]),data)
					colnames(data) <- c(COL.DOCID, vote.values)
					table.file <- paste(folder,file.prefix,"proportions",".csv",sep="")
					write.csv2(data,file=table.file, row.names=FALSE)
				}
			}
		}
	}
}


#############################################################################################
# Generate all plots and tables for the aggregated vote value distributions (FOR, AGAINST, etc.).
# Unlike the "complete" variant, this function integrates data on various dimensions (temporal
# or topical). For each domain (including all domains), we aggregate the votes by year and for
# the whole term. The results are represented as bar plots, for both absolute counts and
# proportions.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# vote.values: the vote values we want to include in the plots and tables.
# file.prefix: string to use in the file names.
# main.folder: folder in which to record the generated plots and tables.
# colors.label: title of the colors legend.
# object: counted object (votes, loyal votes, etc.).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
# plot.formats: formats of the plot files.
#############################################################################################
process.vote.distribution.aggregate <- function(all.votes, doc.details, vote.values, file.prefix, main.folder, colors.label, object, domains, dates, mode, plot.formats)
{	# setup file prefix
	if(is.na(file.prefix))
		file.prefix <- ""
	else
		file.prefix <- paste(file.prefix,"-",sep="")
	# setup title prefix
	if(is.na(mode))
		plot.prefix <- ""
	else
		plot.prefix <- paste("[",mode,"] ",sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	cat("Processing ",file.prefix," data for domain ",dom,"\n",sep="")
		
		# consider each time period (each individual year as well as the whole term)
		votes <- list()
		votes.spe <- list()
		for(date in dates)
		{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.doc.ids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			if(length(filtered.doc.ids)>0)
			{	cols <- match(filtered.doc.ids, colnames(all.votes))
				if(date==DATE.T7.TERM)
					votes.spe <- c(all.votes[,cols])
				else
					votes[[date]] <- c(all.votes[,cols])
			}
			else
			{	votes[[date]] <- NA
				cat("WARNING: Only ",length(filtered.doc.ids)," documents remaining after filtering >> not processing these data\n",sep="")
			}
		}
		
		# setup folder
		folder <- paste(main.folder,dom,"/aggregated/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
	
		# term-wise
		if(length(votes.spe)>0)
		{	if(all(is.na(votes.spe)))
				cat("WARNING: All votes are NAs (this can be correct, not necessarily a problem) >> not processing these data\n",sep="")
			else
			{	# absolute counts as bars
				title <- paste(plot.prefix,"Numbers of ",object," - domain=",dom," - aggregation=term",sep="")
				plot.file <- paste(folder,file.prefix,"term-counts",sep="")
				data <- plot.unif.indiv.raw.bars(plot.file, 
					bar.names=vote.values, 
					values=votes.spe, 
					proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
					x.label=colors.label, plot.title=title, 
					x.rotate=FALSE, format=plot.formats)
				# record as a table
				data <- data.frame(data)
				data <- cbind(data.frame(vote.values),data)
				colnames(data) <- c(COL.VOTE, COL.COUNT)
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
				
				# proportions as bars
				title <- paste(plot.prefix,"Proportions of ",object," - domain=",dom," - aggregation=term",sep="")
				plot.file <- paste(folder,file.prefix,"term-proportions",sep="")
				data <- plot.unif.indiv.raw.bars(plot.file, 
					bar.names=vote.values, 
					values=votes.spe, 
					proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
					x.label=colors.label, plot.title=title, 
					x.rotate=FALSE, format=plot.formats)
				# record as a table
				data <- data / sum(data)
				data[is.na(data)] <- 0
				data <- data.frame(data)
				data <- cbind(data.frame(vote.values),data)
				colnames(data) <- c(COL.VOTE, COL.PERCENT)
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
			}
		}
		
		# by year
		if(length(votes[[date]])>1)
		{	if(all(is.na(votes)))
				cat("WARNING: All yearly values are NAs (this can be correct, not necessarily a problem) >> not processing these data\n",sep="")
			else
			{	# absolute counts as bars
				title <- paste(plot.prefix,"Numbers of ",object," - domain=",dom," aggregation=yearly",sep="")
				plot.file <- paste(folder,file.prefix,"yearly-counts",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
					values=votes, 
					proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
					x.label="Years", colors.label, plot.title=title, 
					x.rotate=FALSE, format=plot.formats)
				# record as a table
				data <- t(data.frame(data))
				data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
				colnames(data) <- c(COL.DATE, vote.values)
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
				
				# proportions as bars
				title <- paste(plot.prefix,"Proportions of ",object," - domain=",dom," aggregation=yearly",sep="")
				plot.file <- paste(folder,file.prefix,"yearly-proportions",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
					values=votes, 
					proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
					x.label="Years", colors.label, plot.title=title, 
					x.rotate=FALSE, format=plot.formats)
				# record as a table
				data <- lapply(data, function(v) if(all(v==0)) {return(rep(0,length(v)))} else {return(v/sum(v))})
				data <- t(data.frame(data))
				data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
				colnames(data) <- c(COL.DATE, vote.values)
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
			}
		}
	}
}


#############################################################################################
# Generate all plots and tables for the averaged vote value distributions (FOR, AGAINST, etc.).
# Unlike the "complete" variant, this function integrates data on various dimensions (temporal
# or topical). For each domain (including all domains), we average the votes by year and for
# the whole term. The resulting distributions are represented as histograms, for both absolute 
# counts and proportions.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# target: the vote value we want to count (to get numerical values, since we want an average).
# file.prefix: string to use in the file names.
# main.folder: folder in which to record the generated plots and tables.
# object: counted object (votes, loyal votes, etc.).
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
# plot.formats: formats of the plot files.
#############################################################################################
process.vote.distribution.average <- function(all.votes, doc.details, target, file.prefix, main.folder, object, domains, dates, mode, plot.formats)
{	# setup file prefix
	if(is.na(file.prefix))
		file.prefix <- ""
	else
		file.prefix <- paste(file.prefix,"-",sep="")
	# setup title prefix
	if(is.na(mode))
		plot.prefix <- ""
	else
		plot.prefix <- paste("[",mode,"] ",sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in domains)
	{	cat("Processing ",file.prefix," data for domain ",dom,"\n",sep="")
		
		# setup folder
		folder <- paste(main.folder,dom,"/averaged/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# consider each time period (each individual year as well as the whole term)
		for(date in dates)
		{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOMAIN.ALL)
				domval <- NA
			else
				domval <- dom
			filtered.doc.ids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			# check if there's enough data remaining
			if(length(filtered.doc.ids)>1)
			{	# format data
				cols <- match(filtered.doc.ids, colnames(all.votes))
				temp <- all.votes[,cols]
				#print(temp)
				numerized <- matrix(0,nrow=nrow(temp), ncol=ncol(temp))
				numerized[temp==target] <- 1
				votes <- apply(numerized, 2, mean)
				
				#print(votes)				
				# plot absolute counts as bars
				title <- paste(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,file.prefix,DATE.STR.T7[date],"-counts",sep="")
				data <- plot.histo(plot.file, values=votes,
					x.label=object, 
					proportions=FALSE, x.lim=c(0,1), y.max=NA, break.nbr=NA, 
					plot.title=title, format=plot.formats)
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
				
				# plot proportions as bars
				title <- paste(plot.prefix,"Distribution of ",object," - domain=",dom,", - period=",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,file.prefix,DATE.STR.T7[date],"-proportions",sep="")
				data <- plot.histo(plot.file, values=votes,
					x.label=object, 
					proportions=TRUE, x.lim=c(0,1), y.max=0.5, break.nbr=NA, 
					plot.title=title, format=plot.formats)
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv2(data,file=table.file, row.names=FALSE)
			}
			else
				cat("WARNING: Only ",length(filtered.doc.ids)," documents remaining after filtering >> not processing these data\n",sep="")
		}
	}
}

#############################################################################################
# Deals with the generation of stats and plots related to the distribution of vote values 
# (FOR, AGAINST, etc.)
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
# subfolder: subfolder used to store the generated files.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
# plot.formats: formats of the plot files.
#############################################################################################
process.vote.distribution <- function(all.votes, doc.details, subfolder, domains, dates, mode, plot.formats)
{	# process a simplified version of the data
	all.votes.smpl <- all.votes
	all.votes.smpl[all.votes.smpl==VOTE.ABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.ABST] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.DOCABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.NONE] <- VOTE.OTHER
	
	# setup folder
	main.folder <- paste(VOTES.FOLDER,"/",subfolder,"/",sep="")
	object <- "votes"
	colors.label <- "Votes"
	
	# process complete vote distributions
	cat("Plotting complete vote value distributions","\n",sep="")
	process.vote.distribution.complete(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed", main.folder, colors.label, object, domains, dates, mode, plot.formats)
	process.vote.distribution.complete(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified", main.folder, colors.label, object, domains, dates, mode, plot.formats)
	
	# process aggregated vote distributions
	cat("Plotting aggregated vote values distributions","\n",sep="")
	process.vote.distribution.aggregate(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed", main.folder, colors.label, object, domains, dates, mode, plot.formats)
	process.vote.distribution.aggregate(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified", main.folder, colors.label, object, domains, dates, mode, plot.formats)
}


#############################################################################################
# Deals with the generation of stats and plots related to the distribution of vote values 
# (FOR, AGAINST, etc.)
#
# behavior.values: individual behavior data, i.e. how each MEP voted relatively to his political 
#				   group (loyal or rebel).
# doc.details: description of each voted document.
# subfolder: subfolder used to store the generated files.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# mode: indicates whether we are processing only a subpart of the original MEPs (used in the 
#		plot titles).
# plot.formats: formats of the plot files.
#############################################################################################
process.behavior.stats <- function(behavior.values, doc.details, subfolder, domains, dates, mode, plot.formats)
{	# setup folder
	main.folder <- paste(BEHAVIOR.FOLDER,"/",subfolder,"/",sep="")
	object <- "loyal votes"
	colors.label <- "Behavior"
	
	# process complete behavior distributions
	cat("Plotting complete behavior distributions","\n",sep="")
	process.vote.distribution.complete(all.votes=behavior.values, doc.details, vote.values=BEHAVIOR.VALUES, file.prefix=NA, main.folder, colors.label, object, domains, dates, mode, plot.formats)
	
	# process aggregated behavior distributions
	cat("Plotting aggregated behavior distributions","\n",sep="")
	process.vote.distribution.aggregate(all.votes=behavior.values, doc.details, vote.values=BEHAVIOR.VALUES, file.prefix=NA, main.folder, colors.label, object, domains, dates, mode, plot.formats)
	
	# processed average behavior distributions
	cat("Plotting average behavior distributions","\n",sep="")
	process.vote.distribution.average(all.votes=behavior.values, doc.details, target=BEHAVIOR.LOYAL, file.prefix=NA, main.folder, object="loyalty index", domains, dates, mode, plot.formats)
}


#############################################################################################
# Main function of this script, generating all stat-related tables and plots.
#
# all.votes: individual vote data, i.e. how each MEP voted.
# behavior.values: individual behavior data, i.e. how each MEP voted relatively to his political 
#				   group (i.e. loyal or rebel).
# doc.details: description of each voted document.
# mep.details: description of each MEP.
# domains: political domains to consider when processing the data.
# dates: time periods to consider when processing the data.
# everything: whether to process all data without distinction of country or political group.
# countries: member states to consider separately when processing the data.
# groups: political groups to consider separately when processing the data.
# plot.formats: formats of the plot files.
#############################################################################################
process.stats <- function(all.votes, behavior.values, doc.details, mep.details, domains, dates, everything, countries, groups, plot.formats)
{	# domain stats
	process.domain.frequencies(doc.details, plot.formats)
	
	# process stats for all data
	if(everything)
	{	cat("Process stats for all data","\n",sep="")
		folder <- "everything"
		process.vote.distribution(all.votes, doc.details, folder, domains, dates, mode=NA, plot.formats)
		process.behavior.stats(behavior.values, doc.details, folder, domains, dates, mode=NA, plot.formats)
	}
	
	# stats by political group
	cat("Process stats by group","\n",sep="")
	folder <- "bygroup"
	for(group in groups)
	{	cat("Process stats for group ",group,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.group(mep.details,group)
		idx <- match(mepids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		group.behavior <- behavior.values[idx,]
		
		# setup folder
		grp.folder <- paste(folder,"/",group,sep="")
		# process vote (for, against...) stats
		process.vote.distribution(group.votes, doc.details, grp.folder, domains, dates, mode=group, plot.formats)
		# process behavior (loyalty vs. rebel) stats
		process.behavior.stats(group.behavior, doc.details, grp.folder, domains, dates, mode=group, plot.formats)
	}
	
	# stats by home country
	cat("Process stats by country","\n",sep="")
	folder <- "bycountry"
	for(country in countries)
	#country <- COUNTRY.HR
	{	cat("Process stats for country ",country,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.country(mep.details,country)
		idx <- match(mepids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		country.behavior <- behavior.values[idx,]
		
		# setup folder
		cntr.folder <- paste(folder,"/",country,sep="")
		# process vote (for, against...) stats
		process.vote.distribution(country.votes, doc.details, cntr.folder, domains, dates, mode=country, plot.formats)
		# process behavior (loyalty vs. rebel) stats
		process.behavior.stats(country.behavior, doc.details, cntr.folder, domains, dates, mode=country, plot.formats)
	}
}
