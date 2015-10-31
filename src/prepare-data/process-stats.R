#############################################################################################
# Generates plots and data files for various statistics processed on the raw data.
# Some of them concern the votes, other concern the rebellion index.
# 
# 07/2015 Israel Mendonça (v1)
# 10/2015 Vincent Labatut (v2)
#############################################################################################
source("src/plot-tools/plot-bars.R")


#############################################################################################
# Counts the occurrences of policy domains among the voted documents. Generates the corresponding
# plots: distribution for the whole term and for each year.
#
# doc.details: table describing the voted documents.
#############################################################################################
process.domain.frequencies <- function(doc.details)
{	cat("Processing the frequencies of documents by policy domain\n",sep="")
	
	# set folder up
	folder <- paste(OVERALL.FOLDER,"/domains/",sep="")
	dir.create(folder, recursive=TRUE, showWarnings=FALSE)
	
	# term domain distribution
		# absolute counts as bars
		title <- paste("Numbers of documents by policy domain for the whole term",sep="")
		plot.file <- paste(folder,"term-counts",sep="")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=DOMAIN.VALUES, 
			values=doc.details[,COL.DOMID],
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- data.frame(data)
		data <- cbind(data.frame(DOMAIN.VALUES),data)
		colnames(data) <- c(COL.DOMID, COL.COUNT)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Proportions of documents by policy domain for the whole term",sep="")
		plot.file <- paste(folder,"term-proportions",sep="")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=DOMAIN.VALUES, 
			values=doc.details[,COL.DOMID],
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- data / sum(data)
		data <- data.frame(data)
		data <- cbind(data.frame(DOMAIN.VALUES),data)
		colnames(data) <- c(COL.DOMID, COL.COUNT)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
	
	# process the list of documents by year
	docs <- list()
	for(date in DATE.T7.YEARS)
	{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
		
		# retain only the documents related to the selected dates
		docids <- filter.by.date(doc.details, 
			start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]])
		idx <- match(docids,doc.details[,COL.DOCID])
		if(length(idx)>0)
			docs[[date]] <- doc.details[idx,COL.DOMID]
		else
			docs[[date]] <- NA
	}
	
	# yearly domain distribution
		# absolute counts as bars
		title <- paste("Yearly numbers of documents by policy domain",sep="")
		plot.file <- paste(folder,"yearly-counts",sep="")
		data <- plot.stacked.indiv.raw.bars(plot.file, 
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=DOMAIN.VALUES, 
			values=docs, 
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, DOMAIN.VALUES)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Yearly proportions of documents by policy domain",sep="")
		plot.file <- paste(folder,"yearly-proportions",sep="")
		data <- plot.stacked.indiv.raw.bars(plot.file, 
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=DOMAIN.VALUES, 
			values=docs, 
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- lapply(data,function(v) v/sum(v))
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, DOMAIN.VALUES)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
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
#############################################################################################
process.vote.distribution.complete <- function(all.votes, doc.details, vote.values, file.prefix, main.folder, colors.label, object)
{	# setup file prefix
	if(is.na(file.prefix))
		prefix <- ""
	else
		prefix <- paste(file.prefix,"-",sep="")
	
	# consider each time period (each individual year as well as the whole term)
	for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
	{	
		# consider each domain individually (including all domains at once)
		for(dom in c(DOM.ALL,DOMAIN.VALUES))
		{	cat("Processing ",file.prefix," data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			idx <- match(docids,doc.details[,COL.DOCID])
			if(length(idx)<=1)
				cat("WARNING: Only ",length(idx)," documents remaining after filtering >> not processing these data\n",sep="")
			else
			{	filtered.docs <- doc.details[idx,]
				
				# order the votes by date
				indices <- order(as.Date(filtered.docs[,COL.DATE],"%d/%m/%Y"))
				
				# get the corresponding votes from the complete table
				cols <- match(docids, colnames(all.votes))
				cols <- cols[indices]
				# convert to list of column-vectors
				votes.temp <- all.votes[,cols]
				votes <- split(votes.temp, rep(1:ncol(votes.temp), each=nrow(votes.temp)))
				
				# setup folder
				folder <- paste(main.folder,dom,"/",DATE.STR.T7[date],"/",sep="")
				dir.create(folder, recursive=TRUE, showWarnings=FALSE)
				
				# absolute counts as bars
				title <- paste("Numbers of ",object," for domain ",dom," and period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,"counts-bars",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=as.character(docids[indices]), color.names=vote.values, 
					values=votes, 
					proportions=FALSE, areas=FALSE, y.lim=c(0,nrow(all.votes)), 
					x.label="Documents (sorted by date)", colors.label, plot.title=title, 
					x.rotate=TRUE, format=c("PDF","PNG",NA))
				# absolute counts as areas
				plot.file <- paste(folder,prefix,"counts-areas",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=1:length(docids), color.names=vote.values, 
					values=votes, 
					proportions=FALSE, areas=TRUE, y.lim=c(0,nrow(all.votes)), 
					x.label="Documents (sorted by date)", colors.label, plot.title=title, 
					x.rotate=FALSE, format=c("PDF","PNG",NA))
				# record as a table
				data <- t(data.frame(data))
				data <- cbind(data.frame(docids[indices]),data)
				colnames(data) <- c(COL.DOCID, vote.values)
				table.file <- paste(folder,prefix,"counts",".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
				
				# proportions as bars
				title <- paste("Proportions of ",object," for domain ",dom," and period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,"proportions-bars",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=as.character(docids[indices]), color.names=vote.values, 
					values=votes, 
					proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
					x.label="Documents (sorted by date)", colors.label, plot.title=title, 
					x.rotate=TRUE, format=c("PDF","PNG",NA))
				# proportions as areas
				plot.file <- paste(folder,prefix,"proportions-areas",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=1:length(docids), color.names=vote.values, 
					values=votes, 
					proportions=TRUE, areas=TRUE, y.lim=c(0,NA), 
					x.label="Documents (sorted by date)", colors.label, plot.title=title, 
					x.rotate=FALSE, format=c("PDF","PNG",NA))
				# record as a table
				data <- lapply(data,function(v) v/sum(v))
				data <- t(data.frame(data))
				data <- cbind(data.frame(docids[indices]),data)
				colnames(data) <- c(COL.DOCID, vote.values)
				table.file <- paste(folder,prefix,"proportions",".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
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
#############################################################################################
process.vote.distribution.aggregate <- function(all.votes, doc.details, vote.values, file.prefix, main.folder, colors.label, object)
{	# setup file prefix
	if(is.na(file.prefix))
		prefix <- ""
	else
		prefix <- paste(file.prefix,"-",sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,DOMAIN.VALUES))
	{	cat("Processing ",file.prefix," data for domain ",dom,"\n",sep="")
		
		# consider each time period (each individual year as well as the whole term)
		votes <- list()
		votes.spe <- list()
		for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
		{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.docs.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			if(length(docids)>0)
			{	cols <- match(docids, colnames(all.votes))
				if(date==DATE.T7.ALL)
					votes.spe <- c(all.votes[,cols])
				else
					votes[[date]] <- c(all.votes[,cols])
			}
			else
				votes[[date]] <- NA
		}
		
		# setup folder
		folder <- paste(main.folder,dom,"/aggregated/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# term-wise
			# absolute counts as bars
			title <- paste("Numbers of ",object," for domain ",dom," aggregated over the whole term",sep="")
			plot.file <- paste(folder,prefix,"term-counts",sep="")
			data <- plot.unif.indiv.raw.bars(plot.file, 
				bar.names=vote.values, 
				values=votes.spe, 
				proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
				x.label=colors.label, plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- data.frame(data)
			data <- cbind(data.frame(vote.values),data)
			colnames(data) <- c(COL.VOTE, COL.COUNT)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
			
			# proportions as bars
			title <- paste("Proportions of ",object," for domain ",dom," aggregated over the whole term",sep="")
			plot.file <- paste(folder,prefix,"term-proportions",sep="")
			data <- plot.unif.indiv.raw.bars(plot.file, 
				bar.names=vote.values, 
				values=votes.spe, 
				proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
				x.label=colors.label, plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- data / sum(data)
			data <- data.frame(data)
			data <- cbind(data.frame(vote.values),data)
			colnames(data) <- c(COL.VOTE, COL.PERCENT)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
		
		# by year
			# absolute counts as bars
			title <- paste("Numbers of ",object," for domain ",dom," aggregated over years",sep="")
			plot.file <- paste(folder,prefix,"yearly-counts",sep="")
			data <- plot.stacked.indiv.raw.bars(plot.file, 
				bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
				values=votes, 
				proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
				x.label="Years", colors.label, plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- t(data.frame(data))
			data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
			colnames(data) <- c(COL.DATE, vote.values)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
			
			# proportions as bars
			title <- paste("Proportions of ",object," for domain ",dom," aggregated over years",sep="")
			plot.file <- paste(folder,prefix,"yearly-proportions",sep="")
			data <- plot.stacked.indiv.raw.bars(plot.file, 
				bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
				values=votes, 
				proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
				x.label="Years", colors.label, plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- lapply(data,function(v) v/sum(v))
			data <- t(data.frame(data))
			data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
			colnames(data) <- c(COL.DATE, vote.values)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
	}
}


#############################################################################################
#############################################################################################
process.vote.distribution.average <- function(all.votes, doc.details, target, file.prefix, main.folder, object)
{	# setup file prefix
	if(is.na(file.prefix))
		prefix <- ""
	else
		prefix <- paste(file.prefix,"-",sep="")
	
	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,DOMAIN.VALUES))
	{	cat("Processing ",file.prefix," data for domain ",dom,"\n",sep="")
		
		# consider each time period (each individual year as well as the whole term)
		votes <- list()
		votes.spe <- list()
		for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
		{	cat("Processing period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.docs.by.date.and.domain(doc.details, 
					start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
					domains=domval)
			if(length(docids)>1)
			{	cols <- match(docids, colnames(all.votes))
				temp <- all.votes[,cols]
				#print(temp)
				numerized <- matrix(0,nrow=nrow(temp), ncol=ncol(temp))
				numerized[temp==target] <- 1
				if(date==DATE.T7.ALL)
					votes.spe <- apply(numerized, 2, mean)
				else
					votes[[date]] <- apply(numerized, 2, mean)
			}
			else
				votes[[date]] <- NA
		}
		
		# setup folder
		folder <- paste(main.folder,dom,"/averaged/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# term-wise
			# absolute counts
			title <- paste("Distribution of ",object," for domain ",dom,", for the whole term",sep="")
			plot.file <- paste(folder,prefix,"term-counts",sep="")
			data <- plot.histo(plot.file, values=votes.spe,
				x.label=object, 
				proportions=FALSE, x.lim=c(0,1), y.max=NA, break.nbr=NA, 
				plot.title=title, format=c("PDF","PNG",NA))
			# record as a table
			data <- data[,c("y","xmin","xmax")]
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
			# proportions
			title <- paste("Distribution of ",object," for domain ",dom,", for the whole term",sep="")
			plot.file <- paste(folder,prefix,"term-proportions",sep="")
			data <- plot.histo(plot.file, values=votes.spe,
				x.label=object, 
				proportions=TRUE, x.lim=c(0,1), y.max=0.5, break.nbr=NA, 
				plot.title=title, format=c("PDF","PNG",NA))
			# record as a table
			data <- data[,c("y","xmin","xmax")]
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
		
		# by year
		for(date in DATE.T7.YEARS)
		{	if(length(votes[[date]])>1)
			{	#print(votes[[date]])
				# absolute counts as bars
				title <- paste("Distribution of ",object," for domain ",dom,", for period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,DATE.STR.T7[date],"-counts",sep="")
				data <- plot.histo(plot.file, values=votes[[date]],
					x.label=object, 
					proportions=FALSE, x.lim=c(0,1), y.max=NA, break.nbr=NA, 
					plot.title=title, format=c("PDF","PNG",NA))
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
		
				# proportions as bars
				title <- paste("Distribution of ",object," for domain ",dom,", for period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,prefix,DATE.STR.T7[date],"-proportions",sep="")
				data <- plot.histo(plot.file, values=votes[[date]],
					x.label=object, 
					proportions=TRUE, x.lim=c(0,1), y.max=0.5, break.nbr=NA, 
					plot.title=title, format=c("PDF","PNG",NA))
				# record as a table
				data <- data[,c("y","xmin","xmax")]
				table.file <- paste(plot.file,".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
			}
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
#############################################################################################
process.vote.distribution <- function(all.votes, doc.details, subfolder)
{	# process a simplified version of the data
	all.votes.smpl <- all.votes
	all.votes.smpl[all.votes.smpl==VOTE.ABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.ABST] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.DOCABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.NONE] <- VOTE.OTHER
	
	# setup folder
	main.folder <- paste(OUT.FOLDER,"/votes/",subfolder,"/",sep="")
	object <- "votes"
	colors.label <- "Votes"
	
	# process complete vote distributions
	cat("Plotting complete vote value distributions","\n",sep="")
	process.vote.distribution.complete(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed", main.folder, colors.label, object)
	process.vote.distribution.complete(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified", main.folder, colors.label, object)
	
	# process aggregated vote distributions
	cat("Plotting aggregated vote values distributions","\n",sep="")
	process.vote.distribution.aggregate(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed", main.folder, colors.label, object)
	process.vote.distribution.aggregate(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified", main.folder, colors.label, object)
}


#############################################################################################
# Deals with the generation of stats and plots related to the distribution of vote values 
# (FOR, AGAINST, etc.)
#
# behavior.values: individual behavior data, i.e. how each MEP voted relatively to his political 
#				   group (loyal or rebel).
# doc.details: description of each voted document.
# subfolder: subfolder used to store the generated files.
#############################################################################################
process.behavior.stats <- function(behavior.values, doc.details, subfolder)
{	# setup folder
	main.folder <- paste(OUT.FOLDER,"/behavior/",subfolder,"/",sep="")
	object <- "loyal votes"
	colors.label <- "Behavior"
	
	# process complete behavior distributions
	cat("Plotting complete behavior distributions","\n",sep="")
	process.vote.distribution.complete(all.votes=behavior.values, doc.details, vote.values=BEHAVIOR.VALUES, file.prefix=NA, main.folder, colors.label, object)
	
	# process aggregated behavior distributions
	cat("Plotting aggregated behavior distributions","\n",sep="")
	process.vote.distribution.aggregate(all.votes=behavior.values, doc.details, vote.values=BEHAVIOR.VALUES, file.prefix=NA, main.folder, colors.label, object)
	
	# processed average behavior distributions
	cat("Plotting average behavior distributions","\n",sep="")
	process.vote.distribution.average(all.votes=behavior.values, doc.details, target=BEHAVIOR.LOYAL, file.prefix=NA, main.folder, object="loyalty index")
}


#############################################################################################
# Main function of this script, generating all stat-related tables and plots.
#
# all.votes: individual vote data, i.e. how each MEP voted.
# behavior.values: individual behavior data, i.e. how each MEP voted relatively to his political 
#				   group (i.e. loyal or rebel).
# doc.details: description of each voted document.
#############################################################################################
process.stats <- function(all.votes, behavior.values, doc.details)
{	# domain stats
#	process.domain.frequencies(doc.details)
	
	# process stats for all data
#	cat("Process stats for all data","\n",sep="")
#	folder <- "everything"
#	process.vote.distribution(all.votes, doc.details, folder)
#	process.behavior.stats(behavior.values, doc.details, folder)
	
	# stats by political group
	cat("Process stats by group","\n",sep="")
	folder <- "bygroup"
	for(group in GROUP.NAMES)
	{	cat("Process stats for group ",group,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.group(mep.details,group)
		idx <- match(mepids,all.votes[,COL.MEPID])
		group.votes <- all.votes[idx,]
		group.behavior <- behavior.values[idx,]
		
		# setup folder
		grp.folder <- paste(folder,"/",group,sep="")
		# process vote (for, against...) stats
		process.vote.distribution(group.votes, doc.details, grp.folder)
		# process behavior (loyalty vs. rebel) stats
		process.behavior.stats(group.behavior, doc.details, grp.folder)
	}
	
	# stats by home country
	cat("Process stats by country","\n",sep="")
	folder <- "bycountry"
	for(country in COUNTRY.VALUES)
	{	cat("Process stats for country ",country,"\n",sep="")
		
		# select data
		mepids <- filter.meps.by.country(mep.details,country)
		idx <- match(mepids,all.votes[,COL.MEPID])
		country.votes <- all.votes[idx,]
		country.behavior <- behavior.values[idx,]
		
		# setup folder
		cntr.folder <- paste(folder,"/",country,sep="")
		# process vote (for, against...) stats
		process.vote.distribution(country.votes, doc.details, cntr.folder)
		# process behavior (loyalty vs. rebel) stats
		process.behavior.stats(country.behavior, doc.details, cntr.folder)
	}
}
#process.stats(all.votes, behavior.values, doc.details)
	
	
#TODO
# - on peut extraire des réseaux au niveau des partis politiques
# - peut être aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 
# 
# - on pourrait aussi voir le comportement individuel: nombre de vote de chaque type pour une personne.
#   ça peut être complet, agrégé par année, par législature... 

# les trous n'apparaissent pas dans graphiques d'aires en proportion