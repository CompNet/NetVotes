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
	
	# overall domain distribution
		# absolute counts as bars
		title <- paste("Numbers of documents by policy domain for the whole term",sep="")
		plot.file <- paste(folder,"term-counts",sep="")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=names(DOMAIN.FULLNAMES), 
			values=doc.details[,COL.DOMID],
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- data.frame(data)
		data <- cbind(data.frame(names(DOMAIN.FULLNAMES)),data)
		colnames(data) <- c(COL.DOMID, COL.COUNT)
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Proportions of documents by policy domain for the whole term",sep="")
		plot.file <- paste(folder,"term-proportions",sep="")
		data <- plot.unif.indiv.raw.bars(plot.file, 
			bar.names=names(DOMAIN.FULLNAMES), 
			values=doc.details[,COL.DOMID],
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- data / sum(data)
		data <- data.frame(data)
		data <- cbind(data.frame(names(DOMAIN.FULLNAMES)),data)
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
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=names(DOMAIN.FULLNAMES), 
			values=docs, 
			proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, names(DOMAIN.FULLNAMES))
		table.file <- paste(plot.file,".csv",sep="")
		write.csv(data,file=table.file, row.names=FALSE)
		
		# proportions as bars
		title <- paste("Yearly proportions of documents by policy domain",sep="")
		plot.file <- paste(folder,"yearly-proportions",sep="")
		data <- plot.stacked.indiv.raw.bars(plot.file, 
			bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=names(DOMAIN.FULLNAMES), 
			values=docs, 
			proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
			x.label="Years", colors.label="Domains", plot.title=title, 
			x.rotate=FALSE, format=c("PDF","PNG",NA))
		# record as a table
		data <- lapply(data,function(v) v/sum(v))
		data <- t(data.frame(data))
		data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
		colnames(data) <- c(COL.DATE, names(DOMAIN.FULLNAMES))
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
#############################################################################################
process.vote.distribution.complete <- function(all.votes, doc.details, vote.values, file.prefix)
{	# consider each time period (each individual year as well as the whole term)
	for(date in c(DATE.T7.ALL,DATE.T7.YEARS))
	{	
		# consider each domain individually (including all domains at once)
		for(dom in c(DOM.ALL,names(DOMAIN.FULLNAMES)))
		{	cat("Processing ",file.prefix," data for domain ",dom," and period ",DATE.STR.T7[date],"\n",sep="")
			
			# retain only the documents related to the selected topic and dates
			if(dom==DOM.ALL)
				domval <- NA
			else
				domval <- dom
			docids <- filter.by.date.and.domain(doc.details, 
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
				folder <- paste(OUT.FOLDER,"/votes/",dom,"/",DATE.STR.T7[date],"/",sep="")
				dir.create(folder, recursive=TRUE, showWarnings=FALSE)
				
				# absolute counts as bars
				title <- paste("Numbers of votes for domain ",dom," and period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,file.prefix,"-counts-bars",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=as.character(docids[indices]), color.names=vote.values, 
					values=votes, 
					proportions=FALSE, areas=FALSE, y.lim=c(0,nrow(all.votes)), 
					x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
					x.rotate=TRUE, format=c("PDF","PNG",NA))
				# absolute counts as areas
				plot.file <- paste(folder,file.prefix,"-counts-areas",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=1:length(docids), color.names=vote.values, 
					values=votes, 
					proportions=FALSE, areas=TRUE, y.lim=c(0,nrow(all.votes)), 
					x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
					x.rotate=FALSE, format=c("PDF","PNG",NA))
				# record as a table
				data <- t(data.frame(data))
				data <- cbind(data.frame(docids[indices]),data)
				colnames(data) <- c(COL.DOCID, vote.values)
				table.file <- paste(folder,file.prefix,"-counts",".csv",sep="")
				write.csv(data,file=table.file, row.names=FALSE)
				
				# proportions as bars
				title <- paste("Proportions of votes for domain ",dom," and period ",DATE.STR.T7[date],sep="")
				plot.file <- paste(folder,file.prefix,"-proportions-bars",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=as.character(docids[indices]), color.names=vote.values, 
					values=votes, 
					proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
					x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
					x.rotate=TRUE, format=c("PDF","PNG",NA))
				# proportions as areas
				plot.file <- paste(folder,file.prefix,"-proportions-areas",sep="")
				data <- plot.stacked.indiv.raw.bars(plot.file, 
					bar.names=1:length(docids), color.names=vote.values, 
					values=votes, 
					proportions=TRUE, areas=TRUE, y.lim=c(0,NA), 
					x.label="Documents (sorted by date)", colors.label="Votes", plot.title=title, 
					x.rotate=FALSE, format=c("PDF","PNG",NA))
				# record as a table
				data <- lapply(data,function(v) v/sum(v))
				data <- t(data.frame(data))
				data <- cbind(data.frame(docids[indices]),data)
				colnames(data) <- c(COL.DOCID, vote.values)
				table.file <- paste(folder,file.prefix,"-proportions",".csv",sep="")
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
#############################################################################################
process.vote.distribution.aggregate <- function(all.votes, doc.details, vote.values, file.prefix)
{	# consider each domain individually (including all domains at once)
	for(dom in c(DOM.ALL,names(DOMAIN.FULLNAMES)))
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
			docids <- filter.by.date.and.domain(doc.details, 
				start.date=DATE.START.T7[[date]], end.date=DATE.END.T7[[date]], 
				domains=domval)
			idx <- match(docids,doc.details[,COL.DOCID])
			if(length(idx)>0)
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
		folder <- paste(OUT.FOLDER,"/votes/",dom,"/aggregated/",sep="")
		dir.create(folder, recursive=TRUE, showWarnings=FALSE)
		
		# overall
			# absolute counts as bars
			title <- paste("Numbers of votes for domain ",dom," aggregated over the whole term",sep="")
			plot.file <- paste(folder,file.prefix,"-overall-counts",sep="")
			data <- plot.unif.indiv.raw.bars(plot.file, 
				bar.names=vote.values, 
				values=votes.spe, 
				proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
				x.label="Vote values", plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- data.frame(data)
			data <- cbind(data.frame(vote.values),data)
			colnames(data) <- c(COL.VOTE, COL.COUNT)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
			
			# proportions as bars
			title <- paste("Proportions of votes for domain ",dom," aggregated over the whole term",sep="")
			plot.file <- paste(folder,file.prefix,"-overall-proportions",sep="")
			data <- plot.unif.indiv.raw.bars(plot.file, 
				bar.names=vote.values, 
				values=votes.spe, 
				proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
				x.label="Vote values", plot.title=title, 
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
			title <- paste("Numbers of votes for domain ",dom," aggregated over years",sep="")
			plot.file <- paste(folder,file.prefix,"-yearly-counts",sep="")
			data <- plot.stacked.indiv.raw.bars(plot.file, 
				bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
				values=votes, 
				proportions=FALSE, areas=FALSE, y.lim=c(0,NA), 
				x.label="Years", colors.label="Votes", plot.title=title, 
				x.rotate=FALSE, format=c("PDF","PNG",NA))
			# record as a table
			data <- t(data.frame(data))
			data <- cbind(data.frame(DATE.STR.T7[DATE.T7.YEARS]),data)
			colnames(data) <- c(COL.DATE, vote.values)
			table.file <- paste(plot.file,".csv",sep="")
			write.csv(data,file=table.file, row.names=FALSE)
			
			# proportions as bars
			title <- paste("Proportions of votes for domain ",dom," aggregated over years",sep="")
			plot.file <- paste(folder,file.prefix,"-yearly-proportions",sep="")
			data <- plot.stacked.indiv.raw.bars(plot.file, 
				bar.names=DATE.STR.T7[DATE.T7.YEARS], color.names=vote.values, 
				values=votes, 
				proportions=TRUE, areas=FALSE, y.lim=c(0,1), 
				x.label="Years", colors.label="Votes", plot.title=title, 
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
# Deals with the generation of stats and plots related to the distribution of vote values 
# (FOR, AGAINST, etc.)
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
#############################################################################################
process.vote.distribution <- function(all.votes, doc.details)
{	# process a simplified version of the data
	all.votes.smpl <- all.votes
	all.votes.smpl[all.votes.smpl==VOTE.ABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.ABST] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.DOCABSENT] <- VOTE.OTHER
	all.votes.smpl[all.votes.smpl==VOTE.NONE] <- VOTE.OTHER
	
	# process complete vote distributions
	cat("Plotting complete vote value distributions","\n",sep="")
	process.vote.distribution.complete(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed")
	process.vote.distribution.complete(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified")
	
	# process aggregated vote distributions
	cat("Plotting aggregated vote values distributions","\n",sep="")
	process.vote.distribution.aggregate(all.votes=all.votes, doc.details, vote.values=VOTE.VALUES, file.prefix="detailed")
	process.vote.distribution.aggregate(all.votes=all.votes.smpl, doc.details, vote.values=VOTE.VALUES.SMPL, file.prefix="simplified")
}


#############################################################################################
#############################################################################################
process.rebellion.stats <- function(all.votes, doc.details)
{
	
}


#############################################################################################
# Main function of this script, generating all stat-related tables and plots.
#
# all.votes: raw vote data, including how each MEP voted.
# doc.details: description of each voted document.
#############################################################################################
process.stats <- function(all.votes, doc.details)
{	# domains
	process.domain.frequencies(doc.details)
	
	# votes
	process.vote.distribution(all.votes, doc.details)
}




#TODO
# - histogram of rebellion: each bar displays the proportion of loyal/rebel for each *expressed* vote
# - same complementary stuff as above
#
#
# - on peut extraire des réseaux au niveau des partis politiques
# - peut être aussi pour chaque vote ? mais les clusters seront triviaux (pr vs ctr) 




## Calculate the rebelion indices for each MeP regarding it Party
#CalculateRebelionIndex <- function(matrix) {
#	converted.matrix <- ConvertMatrix(matrix)
#	reply <- rowSums(converted.matrix,na.rm = TRUE) # Sum the values of each row and store in a vector ignoring NA values
#	number_documents <- apply(converted.matrix, 1, function(x) length(which(!is.na(x)))) # Check how many documents are valid for the normalization
#	reply <- reply / number_documents # Find the percentage of rebelion for each candidate (only for valid documents)
#	reply[which(is.nan(reply))] <- 0
#	return(reply)
#}
#
## Plot the Rebelion indices
#RebelionPlot <- function(rebelion.indexes) {
#	pdf(file=file.path(paste0(output.agreement.dir,"/",dir.title,"/",file.title,"_rebelion_histogram.pdf")))
#	hist(rebelion.indexes, xlab="Rebelion",ylab="Frequency",col=1:20,main="Rebelion Index")
#	dev.off()
#}
